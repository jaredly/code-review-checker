module FetchTracker =
  FluidMac.Tracker({
    type arg = (string, int);
    let name = "phabrador_fetch_cb";
    let once = true;
    type res = unit;
  });

external homeDirectory: unit => string = "phabrador_homeDirectory";
external fetch:
  (
    ~url: string,
    ~callback: FetchTracker.callbackId,
    ~headers: array((string, string))
  ) =>
  unit =
  "phabrador_fetch";
let fetch = (~url, ~headers=[||], callback) =>
  fetch(~url, ~callback=FetchTracker.track(callback), ~headers);
let kwargs = items =>
  String.concat(
    "&",
    items->Belt.List.map(((k, v)) =>
      k ++ "=" ++ EncodeURIComponent.encode(v)
    ),
  );

let imageTbl = Hashtbl.create(10);
let getImage = (src, onDone) => {
  switch (Hashtbl.find(imageTbl, src)) {
  | exception Not_found =>
    print_endline("Getting " ++ src);
    FluidMac.Fluid.NativeInterface.preloadImage(
      ~src,
      ~onDone=loadedImage => {
        print_endline("Got " ++ src);
        Hashtbl.replace(imageTbl, src, loadedImage);
        onDone(loadedImage);
      },
    )
  | img => onDone(img)
  };
};

let cachedImage = (src) => {
  switch (Hashtbl.find(imageTbl, src)) {
  | exception Not_found => 
    FluidMac.Fluid.NativeInterface.preloadImage(
      ~src,
      ~onDone=loadedImage => {
        print_endline("Got " ++ src);
        Hashtbl.replace(imageTbl, src, loadedImage);
        // onDone(loadedImage);
      },
    )
  None
  | img => Some(img)
  };

}

let debug = ref(false);

module Phabricator = {
  let getAuth = () => {
    let fpath = Filename.concat(homeDirectory(), ".arcrc");
    // let fpath = "/Users/jared/.arcrc";
    print_endline("Looking for auth in " ++ fpath);
    if (Files.maybeStat(fpath) == None) {
      print_endline("File doesn't exist");
      exit(10);
    } else {
      print_endline("Found!");
    };
    let json = Json.parse(Files.readFileExn(fpath));
    open Json.Infix;
    let base =
      json
      |> Json.get("config")
      |?> Json.get("default")
      |?> Json.string
      |! "No config.default";
    let hosts = json |> Json.get("hosts") |?> Json.obj |? [];
    switch (hosts) {
    | [] => failwith("No hosts in .arcrc")
    | [(hostname, obj), ..._] =>
      switch (obj |> Json.get("token") |?> Json.string) {
      | None => failwith("No token for host in .arcrc")
      | Some(token) => (hostname, token, base)
      }
    };
  };

  let (hostname, token, base) = getAuth();
  let diffUrl = id => base ++ "/D" ++ string_of_int(id);
  // Api.base ++ "/D" ++ string_of_int(rev.id)

  let call = (endp, args): Lets.Async.Result.t(Json.t, string) => {
    let url =
      hostname ++ endp ++ "?" ++ kwargs([("api.token", token), ...args]);
    /* print_endline("calling " ++ url); */
    let%Lets.Async (body, status) = fetch(~url);
    /* STOPSHIP make this dev-only or something */
    if (debug^) {
      Files.writeFileExn("./.cache/" ++ endp ++ ".json", body);
    };
    switch (Json.parse(body)) {
    | exception (Failure(f)) => Lets.Async.Result.reject(f)
    | json =>
      let result = json |> Json.get("result");
      switch (result) {
      | Some(result) => Lets.Async.Result.resolve(result)
      | None => Lets.Async.Result.reject("API error")
      };
    };
  };

  let wait = (time, cb) => FluidMac.Fluid.App.setTimeout(cb, time);

  let callOffline = (endp, args) => {
    let body = Files.readFileExn("./.cache/" ++ endp ++ ".json");
    let json =
      try (Json.parse(body)) {
      | Failure(f) => failwith("Unable to parse body: " ++ f)
      };
    let%Lets.Opt.Force result = json |> Json.get("result");
    let%Lets.Async () = wait(10);
    Lets.Async.Result.resolve(result);
  };

  let whoAmI = cb =>
    {
      let%Lets.Async.Result result = call("user.whoami", []);
      let%Lets.Opt.Force person = Data.Person.parse(result);
      Lets.Async.Result.resolve(person);
    }(
      cb,
    );

  let getRevisions = me => {
    let%Lets.Async.Result result =
      call(
        "differential.revision.search",
        [
          ("queryKey", "active"),
          ("constraints[responsiblePHIDs][0]", me.Data.Person.phid),
        ],
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
    Lets.Async.Result.resolve(data->Belt.List.keepMap(Data.Revision.parse));
  };

  let unique = items => {
    let seen = Hashtbl.create(10);
    items->Belt.List.keep(k =>
      if (Hashtbl.mem(seen, k)) {
        false;
      } else {
        Hashtbl.add(seen, k, ());
        true;
      }
    );
  };

  let getUsers = phids => {
    let phids = unique(phids);
    let%Lets.Async.Result result =
      call(
        "user.query",
        phids->Belt.List.mapWithIndex((i, phid) =>
          ("phids[" ++ string_of_int(i) ++ "]", phid)
        ),
      );
    let%Lets.Opt.Force data = result |> Json.array;
    /* let%Lets.Async */
    let people = data->Belt.List.keepMap(Data.Person.parse);
    let people =
      people->Belt.List.map((person, cb) =>
        getImage(person.image, loadedImage
          => cb({...person, loadedImage: Some(loadedImage)}))
          // FluidMac.Fluid.NativeInterface.preloadImage(~src=person.image, ~onDone=loadedImage => {
          //   cb({...person, loadedImage: Some(loadedImage)})
          // })
      );
    let%Lets.Async people = Lets.Async.all(people);
    Lets.Async.Result.resolve(
      people->Belt.List.reduce(Belt.Map.String.empty, (map, person) =>
        Belt.Map.String.set(map, person.phid, person)
      ),
    );
  };

  let getRepositories = phids => {
    let phids = unique(phids);
    let%Lets.Async.Result result =
      call(
        "diffusion.repository.search",
        phids->Belt.List.mapWithIndex((i, phid) =>
          ("constraints[phids][" ++ string_of_int(i) ++ "]", phid)
        ),
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
    let repos = data->Belt.List.keepMap(Data.Repository.parse);
    Lets.Async.Result.resolve(
      repos->Belt.List.reduce(Belt.Map.String.empty, (map, repo) =>
        Belt.Map.String.set(map, repo.phid, repo)
      ),
    );
  };

  let getDiffs = phids => {
    let%Lets.Async.Result result =
      call(
        "differential.diff.search",
        phids->Belt.List.mapWithIndex((i, phid) =>
          ("constraints[phids][" ++ string_of_int(i) ++ "]", phid)
        ),
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
    let repos = data->Belt.List.keepMap(Data.Diff.parse);
    Lets.Async.Result.resolve(
      repos->Belt.List.reduce(Belt.Map.String.empty, (map, diff) =>
        Belt.Map.String.set(map, diff.phid, diff)
      ),
    );
  };
};

let yamlErr = v =>
  switch (v) {
  | Ok(v) => Ok(v)
  | Error(`Msg(s)) => Error(Failure(s))
  };

module GitHub = {
  let getAuth = () => {
    let fpath = Filename.concat(homeDirectory(), ".config/hub");
    // let fpath = "/Users/jared/.arcrc";
    print_endline("Looking for auth in " ++ fpath);
    if (Files.maybeStat(fpath) == None) {
      print_endline("File doesn't exist");
      exit(10);
    } else {
      print_endline("Found!");
    };
    let txt = Files.readFileExn(fpath);
    let token =
      switch (txt |> String.split_on_char('\n')) {
      | ["github.com:", _, token_string, ..._] =>
        switch (token_string |> String.split_on_char(':')) {
        | [_, second] => String.trim(second)
        }
      };
    token;
  };

  let diffUrl = id =>
    "https://github.com/Khan/mobile/pull/" ++ string_of_int(id);

  let token = getAuth();
  let hostname = "https://api.github.com/";

  let call = (endp, args): Lets.Async.Result.t(Json.t, string) => {
    let url = hostname ++ endp ++ "?" ++ kwargs(args);
    // print_endline("calling " ++ url);
    let%Lets.Async (body, status) =
      fetch(
        ~headers=[|
          ("Authorization", "token " ++ token),
          (
            "Accept",
            String.concat(
              ",",
              [
                "application/vnd.github.symmetra-preview+json",
                "application/json",
                "application/vnd.github.hellcat-preview+json",
                "application/vnd.github.antiope-preview+json",
              ],
            ),
          ),
        |],
        ~url,
      );
    if (debug^) {
      Files.writeFileExn(
        "./.cache/"
        ++ Str.global_replace(Str.regexp("[^a-zA-Z0-9_]"), "-", endp)
        ++ ".json",
        body,
      );
    };
    switch (Json.parse(body)) {
    | exception (Failure(f)) =>
      print_endline("Failed to parse " ++ f);
      Lets.Async.Result.reject(f);
    | json =>
      // let result = json |> Json.get("data");
      // switch result {
      // | Some(result) =>
      Lets.Async.Result.resolve(json)
    // | None =>
    // print_endline("Api error: " ++ body);
    // Lets.Async.Result.reject("API error")
    // }
    };
  };

  let wait = (time, cb) => FluidMac.Fluid.App.setTimeout(cb, time);

  let callOffline = (endp, args) => {
    let body = Files.readFileExn("./.cache/" ++ endp ++ ".json");
    let json =
      try (Json.parse(body)) {
      | Failure(f) => failwith("Unable to parse body: " ++ f)
      };
    let%Lets.Opt.Force result = json |> Json.get("result");
    let%Lets.Async () = wait(10);
    Lets.Async.Result.resolve(result);
  };

  let whoAmI = cb =>
    {
      let%Lets.Async.Result result = call("user", []);
      print_endline("Got result");
      let%Lets.Try.Force person = Data.PR.parseUser(result);
      print_endline("Person " ++ person.login);
      Lets.Async.Result.resolve(person);
    }(
      cb,
    );

  let getTeams = () => {
    let%Lets.Async.Result result =
      call(
        "user/teams",
        [],
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.array;
    Lets.Async.Result.resolve(
      data->Belt.List.keepMap(item => Data.PR.parseTeam(item) |> Lets.Try.ok),
    );
  };

  let getPRs = repo => {
    let%Lets.Async.Result result =
      call(
        "repos/" ++ repo ++ "/pulls",
        [("sort", "updated"), ("direction", "desc")],
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.array;
    let prs = data -> Belt.List.keepMap(item => Data.PR.parse(item) -> Lets.Try.ok)
    // let%Lets.Async.Result prs = Lets.Async.Result.wrap(data |> Data.tryMap(item => Data.PR.parse(item)));
    let%Lets.Async.Result prs = Lets.Async.Result.all(prs->Belt.List.map((pr, cb) => {
        getImage(pr.user.avatar_url, loadedImage
          => cb(Ok({...pr, user: {...pr.user, loadedImage: Some(loadedImage)}})))
    }));
    Lets.Async.Result.resolve(
      prs
    );
  };

  let getChecks = (repo, sha) => {
    let%Lets.Async.Result result =
      call(
        "repos/" ++ repo ++ "/commits/" ++ sha ++ "/check-runs",
        [],
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.get("check_runs") |?> Json.array;
    Lets.Async.Result.wrap(
      data|> Data.tryMap(Data.Check.parse),
    );
  };

  let unique = items => {
    let seen = Hashtbl.create(10);
    items->Belt.List.keep(k =>
      if (Hashtbl.mem(seen, k)) {
        false;
      } else {
        Hashtbl.add(seen, k, ());
        true;
      }
    );
  };

  let getReviews = (repo, pr) => {
    let%Lets.Async.Result result =
      call(
        "repos/" ++ repo ++ "/pulls/" ++ string_of_int(pr) ++ "/reviews",
        [],
      );
    open Json.Infix;
    let%Lets.Opt.Force data = result |> Json.array;
    let%Lets.Async.Result reviews = Lets.Async.Result.wrap(data |> Data.tryMap(Data.Review.parse));

    // let%Lets.Async.Result reviews = Lets.Async.Result.all(reviews->Belt.List.map((review, cb) => {
    //     getImage(review.user.avatar_url, loadedImage
    //       => {
    //         print_endline("Done " ++ review.user.avatar_url);
    //         cb(Ok({...review, user: {...review.user, loadedImage: Some(loadedImage)}}))
    //       })
    // }));
    
    Lets.Async.Result.resolve(reviews)
  };

  // let getUsers = phids => {
  //   let phids = unique(phids);
  //   let%Lets.Async.Result result =
  //     call(
  //       "user.query",
  //       phids->Belt.List.mapWithIndex((i, phid) =>
  //         ("phids[" ++ string_of_int(i) ++ "]", phid)
  //       ),
  //     );
  //   let%Lets.Opt.Force data = result |> Json.array;
  //   /* let%Lets.Async */
  //   let people = data->Belt.List.keepMap(Data.Person.parse);
  //   let people =
  //     people->Belt.List.map((person, cb) =>
  //       getImage(person.image, loadedImage
  //         => cb({...person, loadedImage: Some(loadedImage)}))
  //         // FluidMac.Fluid.NativeInterface.preloadImage(~src=person.image, ~onDone=loadedImage => {
  //         //   cb({...person, loadedImage: Some(loadedImage)})
  //         // })
  //     );
  //   let%Lets.Async people = Lets.Async.all(people);
  //   Lets.Async.Result.resolve(
  //     people->Belt.List.reduce(Belt.Map.String.empty, (map, person) =>
  //       Belt.Map.String.set(map, person.phid, person)
  //     ),
  //   );
  // };

  // let getRepositories = phids => {
  //   let phids = unique(phids);
  //   let%Lets.Async.Result result =
  //     call(
  //       "diffusion.repository.search",
  //       phids->Belt.List.mapWithIndex((i, phid) =>
  //         ("constraints[phids][" ++ string_of_int(i) ++ "]", phid)
  //       ),
  //     );
  //   open Json.Infix;
  //   let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
  //   let repos = data->Belt.List.keepMap(Data.Repository.parse);
  //   Lets.Async.Result.resolve(
  //     repos->Belt.List.reduce(Belt.Map.String.empty, (map, repo) =>
  //       Belt.Map.String.set(map, repo.phid, repo)
  //     ),
  //   );
  // };

  // let getDiffs = phids => {
  //   let%Lets.Async.Result result =
  //     call(
  //       "differential.diff.search",
  //       phids->Belt.List.mapWithIndex((i, phid) =>
  //         ("constraints[phids][" ++ string_of_int(i) ++ "]", phid)
  //       ),
  //     );
  //   open Json.Infix;
  //   let%Lets.Opt.Force data = result |> Json.get("data") |?> Json.array;
  //   let repos = data->Belt.List.keepMap(Data.Diff.parse);
  //   Lets.Async.Result.resolve(
  //     repos->Belt.List.reduce(Belt.Map.String.empty, (map, diff) =>
  //       Belt.Map.String.set(map, diff.phid, diff)
  //     ),
  //   );
  // };
};

include Phabricator;