
open FluidMac;
open Data;

open Fluid.Native;

module Api = Api;

let str = Fluid.string;

let gray = n => {r: n, g: n, b: n, a: 1.};

external openUrl: string => unit = "phabrador_openUrl";

module TimeoutTracker = FluidMac.Tracker({type arg = unit; let name = "phabrador_timeout_cb"; let once = true;});
external setTimeout: (TimeoutTracker.callbackId, int) => unit = "phabrador_setTimeout";
let setTimeout = (fn, time) => setTimeout(TimeoutTracker.track(fn), time);

module ImageCache = Fluid.Cache({
  type arg = string;
  type result = Fluid.NativeInterface.image;
  let reason = Fluid.noReason;
  let fetch = (src, onDone) => {
    Fluid.NativeInterface.preloadImage(~src, ~onDone)
  }
});

/* let%component imageLoader = (~src, ~layout, hooks) => {
  print_endline("imageLoader render " ++ src);
  let data = ImageCache.fetch(src);

  <image src=Preloaded(data) layout />
};

let%component loadingImage = (~src, ~layout, hooks) => {
  let%hook suspended = Fluid.Hooks.useSuspenseHandler();

  <view layout >
  {
  if (suspended != []) {
    str("Preloading...")
  } else {
    <imageLoader src layout />
  }
}
  </view>
}; */

let timePrinter = ODate.Unix.To.generate_printer("%I:%M%P")->Lets.Opt.force;
let datePrinter = ODate.Unix.To.generate_printer("%b %E, %Y")->Lets.Opt.force;

let recentDate = seconds => {
  open ODate.Unix;
  let date = From.seconds(seconds);
  let now = now();
  let today = beginning_of_the_day(~tz=ODate.Local, now);
  let yesterday = today->advance_by_days(-1);

  if (date > today) {
    "Today at " ++ To.string(~tz=ODate.Local, timePrinter, date)
  } else if (date > yesterday) {
    "Yesterday at " ++ To.string(~tz=ODate.Local, timePrinter, date)
  } else {
    let diff = between(date, now);
    let days = ODuration.To.day(diff);
    if (days <= 10) {
      Printf.sprintf("%d days ago", days);
    } else if (days <= 7 * 4) {
      Printf.sprintf("%d weeks ago", days / 7);
    } else {
      To.string(~tz=ODate.Local, datePrinter, date)
    }
  }
};

let%component revision = (~rev: Data.Revision.t, ~repos: Belt.Map.String.t(Repository.t), ~users: Belt.Map.String.t(Person.t), hooks) => {
  let author = users->Belt.Map.String.get(rev.authorPHID);
  let repo = repos->Belt.Map.String.get(rev.repositoryPHID);
  <view layout={Layout.style(
    ~paddingVertical=8.,
    ~marginHorizontal=8.,
    ~flexDirection=Row,
    ~alignSelf=AlignStretch,
    ())}
  >
    <view
    >
      {switch author {
        | None => str("Unknown author: " ++ rev.authorPHID)
        | Some(person) =>
        <view>
          /* {str(person.userName)} */
          /* <loadingImage src={person.image} 
          layout={Layout.style(~width=30., ~height=30., ())} /> */
          <image
            src={
              switch (person.loadedImage) {
                | None => Plain(person.image)
                | Some(i) => Preloaded(i)
              }
            }
            layout={Layout.style(~margin=3., ~width=30., ~height=30., ())}
          />
        <button
          layout={Layout.style(~left=-5., ())}
        onPress={() => openUrl(Api.base ++ "/D" ++ string_of_int(rev.id))}
          title="Go"
        />
        </view>
      }}
    </view>
    <view 
    layout={Layout.style(~flexGrow=1., ~flexShrink=1., ())}
    >
      {str(~font={fontName: "Helvetica", fontSize: 18.}, rev.Revision.title)}
      <view layout={Layout.style(~flexDirection=Row, ())}>
        {str(recentDate(rev.dateModified))}
        <view layout={Layout.style(~flexGrow=1., ())} />
        {str(switch repo {
          | None => "Unknown repo"
          | Some({name}) => name
        })}
      </view>
    </view>
  </view>
};

let%component revisionList = (~revisions: list(Revision.t),
~repos: Belt.Map.String.t(Repository.t),
~users: Belt.Map.String.t(Person.t), ~title, hooks) => {
  <view layout={Layout.style(~alignItems=AlignStretch, ())}>
    <view backgroundColor=gray(0.9) layout={Layout.style(~paddingHorizontal=8., ~paddingVertical=4., ())}>
      {str(title)}
    </view>
    {Fluid.Native.view(
      ~children=revisions->Belt.List.map(rev => <revision repos users rev /> ),
      ()
    )}
  </view>
};

type revisions = {
  readyToLand: list(Data.Revision.t),
  waiting: list(Data.Revision.t),
  readyToReview: list(Data.Revision.t),
  changesRequested: list(Data.Revision.t),
};

let organizeRevisions = (person, revisions: list(Data.Revision.t)) => {
  let readyToLand =
    revisions->Belt.List.keep(r =>
      r.authorPHID == person.Person.phid && r.status == "accepted"
    );
  let waiting =
    revisions->Belt.List.keep(r =>
      r.authorPHID == person.Person.phid && r.status == "needs-review"
    );
  let readyToReview =
    revisions->Belt.List.keep(r =>
      r.authorPHID != person.Person.phid && r.status == "needs-review"
    );
  let changesRequested =
    revisions->Belt.List.keep(r =>
      r.authorPHID == person.Person.phid && r.status == "needs-revision"
    );
  {readyToLand, readyToReview, waiting, changesRequested};
};

let fetchData = () => {
  module C = Lets.Async;
  let%C person = Api.whoAmI;
  let%C revisions = Api.getRevisions(person);
  let%C users = Api.getUsers(revisions->Belt.List.map(r => r.Revision.authorPHID));
  let%C repos = Api.getRepositories(revisions->Belt.List.map(r => r.repositoryPHID));
  let revisions = organizeRevisions(person, revisions);
  Lets.Async.resolve((person, users, revisions, repos))
};

let makeTitle = revisions => {
  [
    (revisions.readyToLand, "✅"),
    (revisions.changesRequested, "❌"),
    (revisions.readyToReview, "🙏"),
    (revisions.waiting, "⌛"),
  ]
  ->Belt.List.keepMap(((items, emoji)) =>
      items == []
        ? None
        : Some(
            emoji ++ " " ++ string_of_int(List.length(items)),
          )
    )
  |> String.concat(" · ");
};

let setCancellableTimeout = (fn, tm) => {
  let cancelled = ref(false);
  setTimeout(() => {
    if (!cancelled^) {
      fn();
    }
  }, tm);
  () => cancelled := true;
};

let refreshTime = 5 * 60 * 1000;
/* let refreshTime = 5 * 1000; */

let%component main = (~assetsDir, ~refresh, ~setTitle, hooks) => {
  let%hook (data, setData) = Fluid.Hooks.useState(None);
  let%hook (refreshing, setRefreshing) = Fluid.Hooks.useState(false);

  let%hook () =
    Fluid.Hooks.useEffect(
      () => {
        let cancel = ref(() => ());
        let rec loop = () => {
          cancel^();
          print_endline("Looping here");
          setRefreshing(true);
          let%Lets.Async.Consume (person, users, revisions, repos) = fetchData();
          print_endline("Fetched");
          setTitle(Fluid.App.String(makeTitle(revisions)));
          setData(Some((person, users, revisions, repos)));
          setRefreshing(false);
          cancel := setCancellableTimeout(loop, refreshTime);
        };
        refresh := loop;
        loop();

        () => ();
      },
      (),
    );
  <view layout={Layout.style(~width=500., ~height=500., ())}>
    {
      str(~layout=Layout.style(~position=Absolute, ~top=5., ~right=10., ()), refreshing ? "🕓" : "")
    }
    <scrollView
      layout={Layout.style(
        ~flexGrow=1.,
        ~alignItems=AlignStretch,
        ~alignSelf=AlignStretch,
        ~overflow=Scroll,
        (),
      )}>
      <view layout={Layout.style(~alignItems=AlignStretch, ())}>
        {switch (data) {
         | None => str("⌛ loading...")
         | Some((
             person,
             users,
             {readyToLand, readyToReview, waiting, changesRequested},
             repos
           )) =>
           <view layout={Layout.style(~alignItems=AlignStretch, ())}>
             <revisionList title="✅ Ready to land" users repos revisions=readyToLand />
             <revisionList
               title="❌ Ready to update"
               repos
               users
               revisions=changesRequested
             />
             <revisionList
               title="🙏 Ready to review"
               repos
               users
               revisions=readyToReview
             />
             <revisionList title="⌛ Waiting on review" users repos revisions=waiting />
           </view>
         }}
      </view>
    </scrollView>
  </view>;
};

let run = assetsDir => {
  Fluid.App.launch(
    ~isAccessory=true,
    () => {
    Fluid.App.setupAppMenu(
      ~title="Phabrador",
      ~appItems=[||],
      ~menus=[| Fluid.App.defaultEditMenu() |]
    );

    let closeWindow = ref(() => ());

    let statusBarItem = ref(None);

    let refresh = ref(() => ());

    let win = Fluid.launchWindow(
      ~title="Phabrador",
      ~floating=true,
      ~hidden=true,
      ~onBlur=win => {
        Fluid.Window.hide(win);
      },
      <main
        assetsDir
        refresh
        setTitle={title => switch (statusBarItem^) {
          | None => ()
          | Some(item) => Fluid.App.statusBarSetTitle(item, title)
        }}
      />
    );

    closeWindow := () => Fluid.Window.hide(win);

    statusBarItem := Some(Fluid.App.statusBarItem(
      ~isVariableLength=true,
      ~title=String("⌛ Connecting..."),
      ~onClick=pos => {
        refresh^();
        Fluid.Window.showAtPos(win, pos)
      }
    ));
  });

};
