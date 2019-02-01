
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

    Some({title, phid, id, repositoryPHID, authorPHID, summary, dateModified, dateCreated, status, color});
  };
};

let kwargs = items => String.concat("&", items->Belt.List.map(((k, v)) => k ++ "=" ++ EncodeURIComponent.encode(v)));

let call = (endp, args, cb) => {
  let url = hostname ++ endp ++ "?" ++ kwargs([("api.token", token), ...args]);
  print_endline("calling " ++ url);
  fetch(~url, ((body, status)) => {
  /* print_endline(body); */
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

let main = (~assetsDir as _, hooks) => {
  let%hook () = Fluid.Hooks.useEffect(() => {
    whoAmI(person => {
      getRevisions(person, revisions => {
        print_endline("ok");
      });
    });
    () => ()
  }, ());
  <view>
    {str("Hello")}
  </view>
}

let run = assetsDir => {

  /* startChecking(assetsDir); */

  Fluid.App.launch(
    ~isAccessory=true,
    () => {
    Fluid.App.setupAppMenu(
      ~title="Phabrador",
      ~appItems=[||],
      ~menus=[| Fluid.App.defaultEditMenu() |]
    );

    let closeWindow = ref(() => ());

    let win = Fluid.launchWindow(
      ~title="Phabrador",
      ~floating=true,
      ~hidden=true,
      ~onBlur=win => {
        Fluid.Window.hide(win);
      },
      <Main assetsDir />
    );

    closeWindow := () => Fluid.Window.hide(win);

    let _statusBarItem = Fluid.App.statusBarItem(
      ~isVariableLength=true,
      ~title=String("✅ 3 | ❌ 2 | 🙏 4"),
      ~onClick=pos => {
        Fluid.Window.showAtPos(win, pos)
      }
    );
  });

};
