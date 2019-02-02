
open FluidMac;
open Data;

open Fluid.Native;

let str = Fluid.string;

let gray = n => {r: n, g: n, b: n, a: 1.};

external openUrl: string => unit = "phabrador_openUrl";

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

let%component revision = (~rev: Data.Revision.t, ~users: Belt.Map.String.t(Person.t), hooks) => {
  let author = users->Belt.Map.String.get(rev.authorPHID);
  let date = ODate.Unix.From.seconds(rev.dateModified);
  <view layout={Layout.style(~paddingVertical=8., ~paddingHorizontal=8., ())}>
    {str(rev.Revision.title)}
    {str(ODate.Unix.Printer.to_birthday(date))}
    {switch author {
      | None => str("Unknown author: " ++ rev.authorPHID)
      | Some(person) => <view>
      {str(person.userName)}
      /* <loadingImage src={person.image} 
      layout={Layout.style(~width=30., ~height=30., ())} /> */
      <image src={
        switch (person.loadedImage) {
          | None => Plain(person.image)
          | Some(i) => Preloaded(i)
        }
        /* Plain(person.image) */
      } layout={Layout.style(~width=30., ~height=30., ())} />
      </view>
    }}
    <button onPress={() => openUrl(Api.base ++ "/D" ++ string_of_int(rev.id))}
      title="Open Diff"
    />
  </view>
};

let%component revisionList = (~revisions: list(Revision.t), ~users: Belt.Map.String.t(Person.t), ~title, hooks) => {
  <view layout={Layout.style(~alignItems=AlignStretch, ())}>
    <view backgroundColor=gray(0.9) layout={Layout.style(~paddingHorizontal=8., ~paddingVertical=4., ())}>
      {str(title)}
    </view>
    {Fluid.Native.view(
      ~children=revisions->Belt.List.map(rev => <revision users rev /> ),
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

let%component main = (~assetsDir, ~setTitle, hooks) => {
  let%hook (data, setData) = Fluid.Hooks.useState(None);
  let%hook () =
    Fluid.Hooks.useEffect(
      () => {
        {
          module C = Lets.Async.Consume;
          let%C person = Api.whoAmI;
          let%C revisions = Api.getRevisions(person);
          let%C users = Api.getUsers(revisions->Belt.List.map(r => r.Revision.authorPHID));
          Printf.printf("Got users %d\n", Belt.Map.String.size(users));
          print_endline("ok");
          let revisions = organizeRevisions(person, revisions);
          let title =
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
          setTitle(Fluid.App.String(title));
          setData(Some((person, users, revisions)));
        };

        () => ();
      },
      (),
    );
  <view layout={Layout.style(~width=500., ~height=500., ())}>
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
           )) =>
           <view layout={Layout.style(~alignItems=AlignStretch, ())}>
             <revisionList title="✅ Ready to land" users revisions=readyToLand />
             <revisionList
               title="❌ Ready to update"
               users
               revisions=changesRequested
             />
             <revisionList
               title="🙏 Ready to review"
               users
               revisions=readyToReview
             />
             <revisionList title="⌛ Waiting on review" users revisions=waiting />
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

    let win = Fluid.launchWindow(
      ~title="Phabrador",
      ~floating=true,
      ~hidden=true,
      ~onBlur=win => {
        Fluid.Window.hide(win);
      },
      <main
        assetsDir
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
        Fluid.Window.showAtPos(win, pos)
      }
    ));
  });

};
