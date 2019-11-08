open FluidMac;
open Data;

open Fluid.Native;

module Api = Api;

let str = Fluid.string;

let gray = n => {r: n, g: n, b: n, a: 1.};

external openUrl: string => unit = "phabrador_openUrl";
external isDarkMode: unit => bool = "phabrador_isDarkMode";

module TimeoutTracker =
  FluidMac.Tracker({
    type arg = unit;
    let name = "phabrador_timeout_cb";
    let once = true;
    type res = unit;
  });
external setTimeout: (TimeoutTracker.callbackId, int) => unit =
  "phabrador_setTimeout";
let setTimeout = (fn, time) => setTimeout(TimeoutTracker.track(fn), time);

let timePrinter = ODate.Unix.To.generate_printer("%I:%M%P")->Lets.Opt.force;
let datePrinter = ODate.Unix.To.generate_printer("%b %E, %Y")->Lets.Opt.force;

let recentDate = seconds => {
  open ODate.Unix;
  let date = From.seconds(seconds);
  let now = now();
  let today = beginning_of_the_day(~tz=ODate.Local, now);
  let yesterday = today->advance_by_days(-1);

  if (date > today) {
    "Today at " ++ To.string(~tz=ODate.Local, timePrinter, date);
  } else if (date > yesterday) {
    "Yesterday at " ++ To.string(~tz=ODate.Local, timePrinter, date);
  } else {
    let diff = between(date, now);
    let days = ODuration.To.day(diff);
    if (days <= 10) {
      Printf.sprintf("%d days ago", days);
    } else if (days <= 7 * 4) {
      Printf.sprintf("%d weeks ago", days / 7);
    } else {
      To.string(~tz=ODate.Local, datePrinter, date);
    };
  };
};

let toSeconds = Data.toSeconds;

let%component revision = (~rev: Data.PR.t, ~snoozeItem, hooks) => {
  let author = rev.user;
  // let repo = rev.repository;
  if (false) {
    failwith(
      "WIP",
      // if (rev.snoozed) {
      //   <view layout={Layout.style(
      //     ~paddingVertical=8.,
      //     ~marginHorizontal=8.,
      //     ~alignSelf=AlignStretch,
      //     ~flexDirection=Row,
      //     ())}
      //   >
      //     {str(~layout=Layout.style(~flexGrow=1., ()), ~font={fontName: "Helvetica", fontSize: 12.}, rev.Revision.title)}
      //     <button
      //       onPress={() => {
      //         if (rev.snoozed) {
      //           snoozeItem(rev.phid, None)
      //         } else {
      //           let tomorrow = ODate.Unix.From.seconds_float(Unix.time())
      //           -> ODate.Unix.beginning_of_the_day(~tz=ODate.Local, _)
      //           -> ODate.Unix.advance_by_days(1);
      //           snoozeItem(rev.phid, Some(tomorrow->ODate.Unix.To.seconds_float))
      //         }
      //       }}
      //       title={"Unsnooze"}
      //     />
      //   </view>
    );
  } else {
    <view
      layout={Layout.style(
        ~paddingVertical=8.,
        ~marginHorizontal=8.,
        ~flexDirection=Row,
        ~alignSelf=AlignStretch,
        (),
      )}>
      // onPress={() => openUrl(Api.diffUrl(rev.number))}

        <view>
          <view>
            <image
              src={
                switch (Api.cachedImage(author.avatar_url)) {
                | None =>
                  // print_endline("Uncached " ++ author.login);
                  Plain(author.avatar_url);
                | Some(i) => Preloaded(i)
                // Plain(
                //   author.avatar_url,
                // )
                }
              }
              layout={Layout.style(~margin=3., ~width=30., ~height=30., ())}
            />
            <button
              layout={Layout.style(~left=-5., ())}
              onPress={() => openUrl(Api.GitHub.diffUrl(rev.number))}
              title="Go"
            />
          </view>
        </view>
        // {str(switch (rev.mergeable) {
        //   | None => "Checking mergeability"
        //   | Some(false) => "Merge conflicts!"
        //   | Some(trye) => "Mergeable"
        // })}
        /* {str(person.userName)} */
        <view layout={Layout.style(~flexGrow=1., ~flexShrink=1., ())}>
          <view layout={Layout.style(~flexDirection=Row, ())}>
            {str(
               ~layout=Layout.style(~flexGrow=1., ~flexShrink=1., ()),
               ~font={fontName: "Helvetica", fontSize: 18.},
               rev.title,
             )}
            <button
              onPress={() =>
                // if (rev.snoozed) {
                //   snoozeItem(rev.phid, None)
                // } else {
                //   let tomorrow = ODate.Unix.From.seconds_float(Unix.time())
                //   -> ODate.Unix.beginning_of_the_day
                //   -> ODate.Unix.advance_by_days(1);
                //   snoozeItem(rev.phid, Some(tomorrow->ODate.Unix.To.seconds_float))
                // }
                ()}
              // title={rev.snoozed ? "❗" : "💤"}
              title="zzz"
            />
          </view>
          {Fluid.Native.view(
             ~layout=
               {Layout.style(~flexDirection=Row, ~flexWrap=CssWrap, ())},
             ~children=
               {
                 if (rev.reviews != []) {
                 rev.reviews
                ->Belt.List.map(review =>
                    <view layout={Layout.style(~flexDirection=Row, ())}>
                      <image
                        src={
                          switch (Api.cachedImage(review.user.avatar_url)) {
                          | None => Plain(review.user.avatar_url);
                          | Some(i) => Preloaded(i)
                          }
                        }
                        layout={Layout.style(
                          ~margin=3.,
                          ~width=20.,
                          ~height=20.,
                          (),
                        )}
                      />
                      {str(
                         Data.Review.isRejected(review)
                           ? "❌"
                           : Data.Review.isAccepted(review) ? "✅" : "💬",
                       )}
                    </view>
                  )
                 } else if (rev.requested_reviewers != [] || rev.requested_teams != []) {
                   List.append(
                 rev.requested_reviewers
                ->Belt.List.map(user =>
                    <view layout={Layout.style(~flexDirection=Row, ())}>
                      <image
                        src={
                          switch (Api.cachedImage(user.avatar_url)) {
                          | None => Plain(user.avatar_url);
                          | Some(i) => Preloaded(i)
                          }
                        }
                        layout={Layout.style(
                          ~margin=3.,
                          ~width=20.,
                          ~height=20.,
                          (),
                        )}
                      />
                      {str("⌛")}
                    </view>
                  ),
                 rev.requested_teams
                ->Belt.List.map(team =>
                    <view layout={Layout.style(~flexDirection=Row, ())}>
                      {str(team.name)}
                      {str("⌛")}
                    </view>
                  )
                   )
                 } else {
                   [str("❗ no reviewers requested")]
                 }

                // {str(review.user.login)}
                // {str(
                //    ~font={
                //      ...Fluid.NativeInterface.defaultFont,
                //      fontSize: 8.0,
                //    },
                //    check.name,
                //  )}
},
             (),
           )}
          {Fluid.Native.view(
             ~layout=
               {Layout.style(~flexDirection=Row, ~flexWrap=CssWrap, ())},
             ~children=
               {rev.checks
                ->Belt.List.sort((a, b) => compare(a.name, b.name))
                ->Belt.List.map(check =>
                    <view layout={Layout.style(~flexDirection=Row, ())}>
                      {str(
                         check.status != "completed"
                           ? "⌛"
                           : check.conclusion == Some("failure")
                               ? "❌"
                               : check.conclusion == Some("success")
                                   ? "✅" : "❔",
                       )}
                    </view>
                  )
                // {str(
                //    ~font={
                //      ...Fluid.NativeInterface.defaultFont,
                //      fontSize: 8.0,
                //    },
                //    check.name,
                //  )}
},
             (),
           )}
          <view layout={Layout.style(~flexDirection=Row, ())}>
            {str(recentDate(toSeconds(rev.updated_at)))}
            <view layout={Layout.style(~flexGrow=1., ())} />
            // {str(switch repo {
            //   | None => "Unknown repo"
            //   | Some({name}) =>
            //   switch (rev.diff) {
            //     | Some({branch: Some(branch)}) => branch ++ " : "
            //     | _ => ""
            //   } ++ name
            // })}
            {str(rev.head.ref)}
          </view>
        </view>
      </view>;
  };
};

let%component revisionList =
              (~snoozeItem, ~revisions: list(Data.PR.t), ~title, hooks) =>
  if (revisions == []) {
    Fluid.Null;
  } else {
    let darkMode = isDarkMode();
    <view layout={Layout.style(~alignItems=AlignStretch, ())}>
      <view
        backgroundColor={darkMode ? gray(0.1) : gray(0.9)}
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
    //  <Phabrador.main
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