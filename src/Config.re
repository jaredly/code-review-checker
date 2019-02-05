
/* type snooze = {
  phid: string,
  until: int,
}; */

type t = {
  snoozed: Belt.Map.String.t(float),
  checkForUpdates: bool,
};

external homeDirectory: unit => string = "phabrador_homeDirectory";
let default = {checkForUpdates: true, snoozed: Belt.Map.String.empty};
let ofJson = json => {
  open Json.Infix;
  let checkForUpdates = json |> Json.get("checkForUpdates") |?> Json.bool |? true;
  let snoozed = json |> Json.get("snoozed") |?> Json.array |?>> Belt.List.keepMap(_, item => {
    switch (item |> Json.get("phid") |?> Json.string, item |> Json.get("until") |?> Json.number) {
      | (Some(name), Some(until)) => Some((name, until))
      | _ => None
    }
  }) |?>> Belt.List.toArray |?>> Belt.Map.String.fromArray |? Belt.Map.String.empty;
  {checkForUpdates, snoozed}
};
let toJson = ({checkForUpdates, snoozed}) => Json.Object([
  ("checkForUpdates", checkForUpdates ? Json.True : Json.False),
  ("snoozed", Json.Array(snoozed->Belt.Map.String.toArray->Belt.List.fromArray->Belt.List.map(((phid, until)) => Json.Object([
    ("phid", Json.String(phid)),
    ("until", Json.Number(until)),
  ]))))
]);
let load = path => switch (Json.parse(Files.readFileExn(path))) {
  | exception _ => default
  | json => ofJson(json)
};
let save = (config, path) => Files.writeFileExn(path, Json.stringify(toJson(config)));

let (/+) = Filename.concat;

let configPath = homeDirectory() /+ "Library" /+ "Preferences" /+ "com.jaredforsyth.phabrador.json";

print_endline("Saving config to " ++ configPath);

let current = ref(load(configPath));
let update = fn => {
  current := fn(current^);
  save(current^, configPath);
};

let toggleSnoozed = (phid: string, until: option(float)) => {
  update(({snoozed} as config) => 
  {...config, snoozed: switch until {
    | None => snoozed->Belt.Map.String.remove(phid)
    | Some(until) => snoozed->Belt.Map.String.set(phid, until)
  }}
  )
};

let setSnoozed = (config, now, item) => {
  ...item,
  Data.Revision.snoozed: switch (Belt.Map.String.get(config.snoozed, item.Data.Revision.phid)) {
    | None => 
    print_endline(item.phid ++ " not snoozed");
    false
    | Some(t) =>
    Printf.printf("%s times %f vs now %f\n", item.phid, t, now);
    t > now
  }
};
