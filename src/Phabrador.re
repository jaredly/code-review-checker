
open FluidMac;

let str = Fluid.string;

module FetchTracker = Tracker({type arg = (string, int); let name = "phabrador_fetch_cb"; let once = true;});

external homeDirectory: unit => string = "phabrador_homeDirectory";
external fetch: (~url: string, ~callback: FetchTracker.callbackId, ~headers: array((string, string))) => unit = "phabrador_fetch";
let fetch = (~url, ~headers=[||], callback) => fetch(~url, ~callback=FetchTracker.track(callback), ~headers);

let getAuth = () => {
  let fpath = Filename.concat(homeDirectory(), ".arcrc");
  let json = Json.parse(Files.readFileExn(fpath));
  open Json.Infix;
  let hosts = json |> Json.get("hosts") |?> Json.obj |? [];
  switch hosts {
    | [] => failwith("No hosts in .arcrc")
    | [(hostname, obj), ..._] => switch (obj |> Json.get("token") |?> Json.string) {
      | None => failwith("No token for host in .arcrc")
      | Some(token) => (hostname, token)
    }
  }
};

let (hostname, token) = getAuth();

module Person = {
  type t = {userName: string, realName: string, phid: string, image: string};
  let parse = result => {
    module F = Lets.Opt;
    open Json.Infix;
    let%F phid = result |> Json.get("phid") |?> Json.string;
    let%F userName = result |> Json.get("userName") |?> Json.string;
    let%F realName = result |> Json.get("realName") |?> Json.string;
    let%F image = result |> Json.get("image") |?> Json.string;
    Some({userName, realName, image, phid});
  };
};

module Revision = {
  type t = {
    title: string,
    phid: string,
    id: int,
    repositoryPHID: string,
    authorPHID: string,
    summary: string,
    dateCreated: int,
    dateModified: int,
    status: string,
    color: string,
  };
  let parse = result => {
    module F = Lets.Opt;
    open Json.Infix;
    let%F phid = result |> Json.get("phid") |?> Json.string;
    let%F id = result |> Json.get("id") |?> Json.number |?>> int_of_float;
    let%F fields = result |> Json.get("fields");

    let%F title = fields |> Json.get("title") |?> Json.string;
    let%F repositoryPHID = fields |> Json.get("repositoryPHID") |?> Json.string;
    let%F authorPHID = fields |> Json.get("authorPHID") |?> Json.string;
    let%F summary = fields |> Json.get("summary") |?> Json.string;
    let%F dateCreated = fields |> Json.get("dateCreated") |?> Json.number |?>> int_of_float;
    let%F dateModified = fields |> Json.get("dateModified") |?> Json.number |?>> int_of_float;
    let%F statusObj = fields |> Json.get("status");
    let%F status = statusObj |> Json.get("value") |?> Json.string;
    let%F color = statusObj |> Json.get("color.ansi") |?> Json.string;
    print_endline(status);

    Some({title, phid, id, repositoryPHID, authorPHID, summary, dateModified, dateCreated, status, color});
  };
};

let kwargs = items => String.concat("&", items->Belt.List.map(((k, v)) => k ++ "=" ++ EncodeURIComponent.encode(v)));

let call = (endp, args, cb) => {
  let url = hostname ++ endp ++ "?" ++ kwargs([("api.token", token), ...args]);
  print_endline("calling " ++ url);
  fetch(~url, ((body, status)) => {
    Files.writeFileExn("./result-" ++ endp, body);
    let json = try (Json.parse(body)) {
      | Failure(f) => failwith("Unable to parse body: " ++ f)
    };
    module F = Lets.Opt.Force;
    let%F result = json |> Json.get("result");
    cb(result);
  });
};

let whoAmI = cb => {
  call("user.whoami", [], result => {
    module F = Lets.Opt.Force;
    let%F person = Person.parse(result);
    cb(person)
  });
};

let getRevisions = (me, cb) => {
  call("differential.revision.search", [
    ("queryKey", "active"),
    ("constraints[responsiblePHIDs][0]", me.Person.phid),
  ], result => {
    module F = Lets.Opt.Force;
    open Json.Infix;
    let%F data = result |> Json.get("data") |?> Json.array;
    cb(data->Belt.List.keepMap(Revision.parse))
  })
};

let gray = n => {r: n, g: n, b: n, a: 1.};

let revisionList = (~revisions, ~title, hooks) => {
  <view layout={Layout.style(~alignItems=AlignStretch, ())}>
    <view backgroundColor=gray(0.8) layout={Layout.style(~paddingHorizontal=8., ~paddingVertical=4., ())}>
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

let main = (~assetsDir, ~setTitle, hooks) => {
  let%hook (revisions, setRevisions) = Fluid.Hooks.useState(None);
  let%hook () = Fluid.Hooks.useEffect(() => {
    whoAmI(person => {
      getRevisions(person, revisions => {
        let readyToLand = revisions->Belt.List.keep(r => r.authorPHID == person.Person.phid && r.status == "accepted");
        let waiting = revisions->Belt.List.keep(r => r.authorPHID == person.Person.phid && r.status == "needs-review");
        let readyToReview = revisions->Belt.List.keep(r => r.authorPHID != person.Person.phid && r.status == "needs-review");
        let changesRequested = revisions->Belt.List.keep(r => r.authorPHID == person.Person.phid && r.status == "needs-revision");
        let title = ([
          (readyToLand, "✅"),
          (changesRequested, "❌"),
          (readyToReview, "🙏"),
          (waiting, "⌛"),
        ])->Belt.List.keepMap(((items, emoji)) => items == [] ? None : Some(emoji ++ " " ++ string_of_int(List.length(items)))) |> String.concat(" · ");
        setTitle(Fluid.App.String(title));
        setRevisions(Some((person, revisions)));
      });
    });
    () => ()
  }, ());
      <view layout={Layout.style(~width=500., ~height=500., ())}>
      <scrollView
        layout={Layout.style(~flexGrow=1., ~alignItems=AlignStretch, ~alignSelf=AlignStretch, ~overflow=Scroll, ())}
      >
      <view layout={Layout.style(~alignItems=AlignStretch, ())}>
      {

  switch (revisions) {
    | None => 
        {str("⌛ loading...")}
    | Some((person, revisions)) =>
      let readyToLand = revisions->Belt.List.keep(r => r.authorPHID == person.Person.phid && r.status == "accepted");
      let waiting = revisions->Belt.List.keep(r => r.authorPHID == person.Person.phid && r.status == "needs-review");
      let readyToReview = revisions->Belt.List.keep(r => r.authorPHID != person.Person.phid && r.status == "needs-review");
      let changesRequested = revisions->Belt.List.keep(r => r.authorPHID == person.Person.phid && r.status == "needs-revision");
      let title = Printf.sprintf("✅ %d | ❌ %d | 🙏 %d", List.length(readyToLand), List.length(waiting), List.length(readyToReview));
      <view layout={Layout.style(~alignItems=AlignStretch, ())}>
        <RevisionList
          title="✅ Ready to land"
          revisions=readyToLand
        />
        <RevisionList
          title="❌ Ready to update"
          revisions=changesRequested
        />
        <RevisionList
          title="🙏 Ready to review"
          revisions=readyToReview
        />
        <RevisionList
          title="⌛ Waiting on review"
          revisions=waiting
        />
      </view>
      }
    }
    </view>
    </scrollView>
    </view>
}

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
      <Main
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
