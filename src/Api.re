
module FetchTracker = FluidMac.Tracker({type arg = (string, int); let name = "phabrador_fetch_cb"; let once = true;});

external homeDirectory: unit => string = "phabrador_homeDirectory";
external fetch: (~url: string, ~callback: FetchTracker.callbackId, ~headers: array((string, string))) => unit = "phabrador_fetch";
let fetch = (~url, ~headers=[||], callback) => fetch(~url, ~callback=FetchTracker.track(callback), ~headers);

let getAuth = () => {
  let fpath = Filename.concat(homeDirectory(), ".arcrc");
  let json = Json.parse(Files.readFileExn(fpath));
  open Json.Infix;
  let base = json |> Json.get("config") |?> Json.get("default") |?> Json.string |! "No config.default";
  let hosts = json |> Json.get("hosts") |?> Json.obj |? [];
  switch hosts {
    | [] => failwith("No hosts in .arcrc")
    | [(hostname, obj), ..._] => switch (obj |> Json.get("token") |?> Json.string) {
      | None => failwith("No token for host in .arcrc")
      | Some(token) => (hostname, token, base)
    }
  }
};

let (hostname, token, base) = getAuth();

let kwargs = items => String.concat("&", items->Belt.List.map(((k, v)) => k ++ "=" ++ EncodeURIComponent.encode(v)));

let call = (endp, args, cb) => {
  let url = hostname ++ endp ++ "?" ++ kwargs([("api.token", token), ...args]);

  /* let body = Files.readFileExn("./result-" ++ endp);
  let json = try (Json.parse(body)) {
    | Failure(f) => failwith("Unable to parse body: " ++ f)
  };
  module F = Lets.Opt.Force;
  let%F result = json |> Json.get("result");
  FluidMac.Fluid.App.setTimeout(() => {
    cb(result);
  }, 10); */

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
    let%F person = Data.Person.parse(result);
    cb(person)
  });
};

let getRevisions = (me, cb) => {
  call("differential.revision.search", [
    ("queryKey", "active"),
    ("constraints[responsiblePHIDs][0]", me.Data.Person.phid),
  ], result => {
    module F = Lets.Opt.Force;
    open Json.Infix;
    let%F data = result |> Json.get("data") |?> Json.array;
    cb(data->Belt.List.keepMap(Data.Revision.parse))
  })
};
