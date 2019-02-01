

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

let main = (~assetsDir as _, hooks) => {
  let%hook () = Fluid.Hooks.useEffect(() => {
    fetch(~url=hostname ++ "user.whoami?api.token=" ++ token, ((body, status)) => {
      print_endline("Success! " ++ string_of_int(status));
      print_endline(body)
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
