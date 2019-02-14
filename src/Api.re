
module FetchTracker = FluidMac.Tracker({
  type arg = (string, int); let name = "phabrador_fetch_cb"; let once = true;
  type res = unit;
  });

external homeDirectory: unit => string = "phabrador_homeDirectory";
external fetch: (~url: string, ~callback: FetchTracker.callbackId, ~headers: array((string, string))) => unit = "phabrador_fetch";
let fetch = (~url, ~headers=[||], callback) => fetch(~url, ~callback=FetchTracker.track(callback), ~headers);

let debug = ref(false);

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

let call = (endp, args): Lets.Async.Result.t(Json.t, string) => {
  let url = hostname ++ endp ++ "?" ++ kwargs([("api.token", token), ...args]);
  /* print_endline("calling " ++ url); */
  let%Lets.Async (body, status) = fetch(~url);
  /* STOPSHIP make this dev-only or something */
  if (debug^) {
    Files.writeFileExn("./.cache/" ++ endp ++ ".json", body);
  };
  switch (Json.parse(body)) {
    | exception Failure(f) => Lets.Async.Result.reject(f)
    | json =>
    let result = json |> Json.get("result");
    switch result {
     | Some(result) => Lets.Async.Result.resolve(result);
     | None => Lets.Async.Result.reject("API error")
    }
  };
};

let wait = (time, cb) => FluidMac.Fluid.App.setTimeout(cb, time);

let callOffline = (endp, args) => {
  let body = Files.readFileExn("./.cache/" ++ endp ++ ".json");
  let json = try (Json.parse(body)) {
    | Failure(f) => failwith("Unable to parse body: " ++ f)
  };
  let%Lets.Opt.Force result = json |> Json.get("result");
  let%Lets.Async () = wait(10);
  Lets.Async.Result.resolve(result);
};

let whoAmI = cb => {
  let%Lets.Async.Result result = call("user.whoami", []);
  let%Lets.Opt.Force person = Data.Person.parse(result);
  Lets.Async.Result.resolve(person)
}(cb);

let getRevisions = (me) => {
  let%Lets.Async.Result result = call("differential.revision.search", [
    ("queryKey", "active"),
    ("constraints[responsiblePHIDs][0]", me.Data.Person.phid),
  ]);
  open Json.Infix;
  let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
  Lets.Async.Result.resolve(data->Belt.List.keepMap(Data.Revision.parse))
};

let unique = items => {
  let seen = Hashtbl.create(10);
  items->Belt.List.keep(k => if (Hashtbl.mem(seen, k)) { false } else {
    Hashtbl.add(seen, k, ()); true
  })
}

let getUsers = (phids) => {
  let phids = unique(phids);
  let%Lets.Async.Result result = call("user.query", phids->Belt.List.mapWithIndex((i, phid) => (
    ("phids[" ++ string_of_int(i) ++ "]", phid)
  )));
  let%Lets.Opt.Force data = result |> Json.array;
  /* let%Lets.Async */
  let people = data->Belt.List.keepMap(Data.Person.parse);
  let people =
    people->Belt.List.map((person, cb) =>
      FluidMac.Fluid.NativeInterface.preloadImage(~src=person.image, ~onDone=loadedImage => {
        cb({...person, loadedImage: Some(loadedImage)})
      })
    );
  let%Lets.Async people = Lets.Async.all(people);
  Lets.Async.Result.resolve(people->Belt.List.reduce(Belt.Map.String.empty, (map, person) => {
    Belt.Map.String.set(map, person.phid, person)
  }))
};

let getRepositories = (phids) => {
  let phids = unique(phids);
  let%Lets.Async.Result result = call("diffusion.repository.search", phids->Belt.List.mapWithIndex((i, phid) => (
    ("constraints[phids][" ++ string_of_int(i) ++ "]", phid)
  )));
  open Json.Infix;
  let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
  let repos = data->Belt.List.keepMap(Data.Repository.parse);
  Lets.Async.Result.resolve(repos->Belt.List.reduce(Belt.Map.String.empty, (map, repo) => {
    Belt.Map.String.set(map, repo.phid, repo)
  }))
};

let getDiffs = (phids) => {
  let%Lets.Async.Result result = call("differential.diff.search", phids->Belt.List.mapWithIndex((i, phid) => (
    ("constraints[phids][" ++ string_of_int(i) ++ "]", phid)
  )));
  open Json.Infix;
  let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
  let repos = data->Belt.List.keepMap(Data.Diff.parse);
  Lets.Async.Result.resolve(repos->Belt.List.reduce(Belt.Map.String.empty, (map, diff) => {
    Belt.Map.String.set(map, diff.phid, diff)
  }))
};

