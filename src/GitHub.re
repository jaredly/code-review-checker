open FluidMac;
open Data;

open Fluid.Native;

module Api = Api;

let str = Fluid.string;

let gray = n => {r: n, g: n, b: n, a: 1.};

external isDarkMode: unit => bool = "codeReviewChecker_isDarkMode";
external openUrl: string => unit = "codeReviewChecker_openUrl";

let revision = Revisionn.revision;

module TimeoutTracker =
  FluidMac.Tracker({
    type arg = unit;
    let name = "codeReviewChecker_timeout_cb";
    let once = true;
    type res = unit;
  });
external setTimeout: (TimeoutTracker.callbackId, int) => unit =
  "codeReviewChecker_setTimeout";
let setTimeout = (fn, time) => setTimeout(TimeoutTracker.track(fn), time);

let toSeconds = Data.toSeconds;

let%component revisionList =
              (~snoozeItem, ~revisions: list(Data.PR.t), ~title, hooks) =>
  if (revisions == []) {
    Fluid.Null;
  } else {
    let darkMode = isDarkMode();
    <view layout={Layout.style(~alignItems=AlignStretch, ())}>
      <view
        backgroundColor={darkMode ? gray(0.15) : gray(0.9)}
        layout={Layout.style(~paddingHorizontal=8., ~paddingVertical=4., ())}>
        {str(title)}
      </view>
      {Fluid.Native.view(
         ~children=
           revisions
           ->Belt.List.sort((a, b) =>
               toSeconds(b.updated_at) - toSeconds(a.updated_at)
             )
           ->Belt.List.map(rev => <revision snoozeItem rev />),
         (),
       )}
    </view>;
  };

let fetchGithubData = () => {
  module C = Lets.Async.Result;
  let repo = "Khan/mobile";
  let%C person = Api.GitHub.whoAmI;
  let%C revisions = Api.GitHub.getPRs(repo);
  let%C teams = Api.GitHub.getTeams();
  let person = {...person, team_ids: teams->Belt.List.map(team => team.id)};
  let%C revisionsWithReviews =
    Lets.Async.Result.all(
      revisions->Belt.List.map(revision => {
        let%C reviews = Api.GitHub.getReviews(repo, revision.number, person);
        let%C checks = Api.GitHub.getChecks(repo, revision.head.sha);
        C.resolve({...revision, reviews, checks});
      }),
    );
  let%Lets.Async () = Api.GitHub.preloadReviewerAvis(revisionsWithReviews);
  C.resolve((person, revisionsWithReviews));
};

let isMine = (me: Data.PR.user, revision: Data.PR.t) => {
  revision.user.login == me.login;
};

let needsMyReview = (me: Data.PR.user, revision: Data.PR.t) => {
  !isMine(me, revision)
  && (
    revision.requested_reviewers->Belt.List.some(req => req.login == me.login)
    || revision.requested_teams
       ->Belt.List.some(req => me.team_ids->Belt.List.has(req.id, (==)))
  );
};

let needsAction = (revision: Data.PR.t) => {
  revision.mergeable == Some(false)
  || revision.checks->Belt.List.some(Data.Check.isFailed)
  || (revision.reviews == [] && revision.requested_reviewers == [] && revision.requested_teams == [])
  || revision.reviews
     ->Belt.List.some(review => Data.Review.isRejected(review));
};

let isWaiting = (revision: Data.PR.t) => {
  !needsAction(revision) && (

  revision.checks == []
  || revision.mergeable == None
  || !revision.reviews->Belt.List.some(review => Data.Review.isAccepted(review))
  || revision.checks->Belt.List.some(Data.Check.isPending)
  || revision.reviews->Belt.List.some(review => review.state === "PENDING")
  )
};

let isLandable = (revision: Data.PR.t) => {
  !isWaiting(revision) && !needsAction(revision);
};

let makeTitle = (me: Data.PR.user, revisions: list(Data.PR.t)) => {
  let mine = revisions->Belt.List.keep(isMine(me));
  let landable = mine->Belt.List.keep(isLandable);
  let needAction = mine->Belt.List.keep(needsAction);
  let needReview = revisions->Belt.List.keep(needsMyReview(me));
  let waiting = mine->Belt.List.keep(isWaiting);

  let items =
    [
      (landable, "✅"),
      (needAction, "❌"),
      (needReview, "🙏"),
      //   (waiting, "⌛"),
    ]
    ->Belt.List.keepMap(((items, emoji))
        // let items = items->Belt.List.keep(r => !r.snoozed);
        =>
          items === []
            ? None : Some(emoji ++ " " ++ string_of_int(List.length(items)))
        );
  print_endline("Made title");

  // if (landable != []) {
  //   items := [("✅" ++ " " ++ string_of_int(landable->List.length)), ...items^];
  // };

  // let items = items^;

  // let items = [
  //   (revisions.mine.accepted, "✅"),
  //   (revisions.mine.needsRevision, "❌"),
  //   (revisions.theirs.needsReview, "🙏"),
  // ]
  // ->Belt.List.keepMap(((items, emoji)) => {
  //   let items = items->Belt.List.keep(r => !r.snoozed);
  //   items === []
  //     ? None
  //     : Some(
  //         emoji ++ " " ++ string_of_int(List.length(items)),
  //       )
  // });

  if (items === []) {
    "🐶";
  } else {
    items |> String.concat(" · ");
  };
};

let setCancellableTimeout = (fn, tm) => {
  let cancelled = ref(false);
  setTimeout(
    () =>
      if (! cancelled^) {
        fn();
      },
    tm,
  );
  () => cancelled := true;
};

let refreshTime = 5 * 60 * 1000;
// let refreshTime = 3 * 1000;

let updateSnoozed = revisions => {
  let now = Unix.time();
  revisions->Revision.map(__x =>
    Belt.List.map(__x, Config.setSnoozed(Config.current^, now))
  );
};

Printexc.record_backtrace(true);

let%component main = (~assetsDir, ~refresh, ~setTitle, hooks) => {
  let%hook (data, setData) = Fluid.Hooks.useState(None);
  let%hook (refreshing, setRefreshing) = Fluid.Hooks.useState(false);

  let snoozeItem = (phid, until) => {
    print_endline("Snoozing " ++ phid);
    // Gc.full_major();
    switch (data) {
    | None => ()
    | Some((p, r)) =>
      Config.toggleSnoozed(phid, until);
      // let revisions = updateSnoozed(r);
      setTitle(Fluid.App.String(makeTitle(p, r)));
      setData(Some((p, r)));
    };
  };
  /* print_endline("render"); */

  let%hook () =
    Fluid.Hooks.useEffect(
      () => {
        let cancel = ref(() => ());
        let rec loop = () => {
          cancel^();
          setRefreshing(true);
          let%Lets.Async.Consume result = fetchGithubData();
          /* print_endline("Fetched"); */
          switch (result) {
          | Error(error) =>
            print_endline("Failed to fetch " ++ error);
            setTitle(String("⌛"));
            setData(None);
          | Ok((person, revisions)) =>
            // let revisions = updateSnoozed(revisions);
            setTitle(Fluid.App.String(makeTitle(person, revisions)));
            /* print_endline("a"); */
            setData(Some((person, revisions)));
          };
          /* print_endline("b"); */
          setRefreshing(false);
          /* print_endline("c"); */
          cancel := setCancellableTimeout(loop, refreshTime);
          /* print_endline("Updated"); */
        };
        refresh := loop;
        loop();

        () => ();
      },
      (),
    );

  <view
    layout={Layout.style(
      ~width=500.,
      ~maxHeight=800.,
      /* ~height=500., */
      (),
    )}>

      {str(
         ~layout=Layout.style(~position=Absolute, ~top=5., ~right=10., ()),
         refreshing ? "🕓" : "",
       )}
      {switch (data) {
       | None => str("⌛ loading...")
       | Some((person, revisions)) =>
         let mine = revisions->Belt.List.keep(isMine(person));
         let theirs = revisions->Belt.List.keep(r => !isMine(person, r));
         let landable = mine->Belt.List.keep(isLandable);
         let needAction = mine->Belt.List.keep(needsAction);
         let needReview = revisions->Belt.List.keep(needsMyReview(person));
         let waiting = mine->Belt.List.keep(isWaiting);

         <scrollView
           layout={Layout.style(
             /* ~flexGrow=1., */
             ~alignItems=AlignStretch,
             ~alignSelf=AlignStretch,
             ~overflow=Scroll,
             (),
           )}>
           <view layout={Layout.style(~alignItems=AlignStretch, ())}>
             <view layout={Layout.style(~alignItems=AlignStretch, ())}>
               // {str("Hello")}

                 <revisionList
                   title="✅ Ready to land"
                   snoozeItem
                   revisions=landable
                 />
                 <revisionList
                   title="❌ Ready to update"
                   snoozeItem
                   revisions=needAction
                 />
                 <revisionList
                   title="🙏 Ready to review"
                   snoozeItem
                   revisions=needReview
                 />
                 <revisionList
                   title="⌛ Waiting"
                   snoozeItem
                   revisions=waiting
                 />
                 <revisionList
                   title="⌛❌ Waiting for them to change"
                   snoozeItem
                   revisions={theirs->Belt.List.keep(needsAction)}
                 />
                 <revisionList
                   title="⌛✅ Waiting for them to land"
                   snoozeItem
                   revisions={theirs->Belt.List.keep(isLandable)}
                 />
               </view>
           </view>
         </scrollView>;
       }}
      <button
        layout=Layout.style(~position=Absolute, ~top=3., ~right=35., ())
        onPress={() => openUrl("https://github.com/Khan/mobile/pulls")}
        title="Open web"
      />
    </view>;
    //  <CodeReviewChecker.main
    //     assetsDir
    //     refresh
    //  />
};

let run = assetsDir => {
  Fluid.App.launch(
    ~isAccessory=true,
    () => {
      Fluid.App.setupAppMenu(
        ~title="GitHub",
        ~appItems=[||],
        ~menus=[|Fluid.App.defaultEditMenu()|],
      );

      let closeWindow = ref(() => ());

      let statusBarItem = ref(None);

      let refresh = ref(() => ());

      let win =
        Fluid.launchWindow(
          ~title="GitHub",
          ~floating=true,
          ~hidden=true,
          ~onResize=
            ({width, height}, window) =>
              window->Fluid.Window.resize({x: width, y: height}),
          ~onBlur=win => Fluid.Window.hide(win),
          <main
            assetsDir
            refresh
            setTitle={title =>
              switch (statusBarItem^) {
              | None => ()
              | Some(item) => Fluid.App.statusBarSetTitle(item, title)
              }
            }
          />,
        );

      closeWindow := (() => Fluid.Window.hide(win));

      statusBarItem :=
        Some(
          Fluid.App.statusBarItem(
            ~isVariableLength=true,
            ~title=String("⌛"),
            ~onClick=pos => {
              refresh^();
              Fluid.Window.showAtPos(win, pos);
            },
          ),
        );
    },
  );
};