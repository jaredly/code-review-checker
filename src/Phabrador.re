
open FluidMac;
open Data;

open Fluid.Native;

let str = Fluid.string;

let gray = n => {r: n, g: n, b: n, a: 1.};

let%component revisionList = (~revisions, ~title, hooks) => {
  <view layout={Layout.style(~alignItems=AlignStretch, ())}>
    <view backgroundColor=gray(0.9) layout={Layout.style(~paddingHorizontal=8., ~paddingVertical=4., ())}>
      {str(title)}
    </view>
    {Fluid.Native.view(
      ~children=revisions->Belt.List.map(rev => {
        let date = ODate.Unix.From.seconds(rev.Revision.dateModified);
        <view layout={Layout.style(~paddingVertical=8., ~paddingHorizontal=8., ())}>
          {str(rev.Revision.title)}
          {str(ODate.Unix.Printer.to_birthday(date))}
        </view>
      }),
      ()
    )}
  </view>
}

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
  let%hook (revisions, setRevisions) = Fluid.Hooks.useState(None);
  let%hook () =
    Fluid.Hooks.useEffect(
      () => {
        Api.whoAmI(person =>
          Api.getRevisions(
            person,
            revisions => {
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
              setRevisions(Some((person, revisions)));
            },
          )
        );
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
        {switch (revisions) {
         | None => str("⌛ loading...")
         | Some((
             person,
             {readyToLand, readyToReview, waiting, changesRequested},
           )) =>
           <view layout={Layout.style(~alignItems=AlignStretch, ())}>
             <revisionList title="✅ Ready to land" revisions=readyToLand />
             <revisionList
               title="❌ Ready to update"
               revisions=changesRequested
             />
             <revisionList
               title="🙏 Ready to review"
               revisions=readyToReview
             />
             <revisionList title="⌛ Waiting on review" revisions=waiting />
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
