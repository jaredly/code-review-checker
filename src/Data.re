let (|!!) = (a, b) =>
  switch (a) {
  | Some(x) => Ok(x)
  | None => Error(b)
  };

let dateParser =
  ODate.Unix.From.generate_parser(ODate.Unix.Format.iso)->Lets.Opt.force;

let toSeconds = text =>
  ODate.Unix.From.string(dateParser, text)->ODate.Unix.To.seconds;

module Person = {
  type t = {
    userName: string,
    realName: string,
    phid: string,
    image: string,
    loadedImage: option(FluidMac.Fluid.NativeInterface.image),
  };
  let parseGithub = result => {
    module F = Lets.Try;
    open Json.Infix;
    let%F phid = result |> Json.get("id") |?> Json.number |!! "No id";
    let%F userName =
      result |> Json.get("login") |?> Json.string |!! "No login";
    let%F realName = result |> Json.get("name") |?> Json.string |!! "No name";
    let%F image =
      result |> Json.get("full_name") |?> Json.string |!! "No avatar";
    Ok({
      userName,
      realName,
      image,
      phid: string_of_float(phid),
      loadedImage: None,
    });
  };
  let parse = result => {
    module F = Lets.Opt;
    open Json.Infix;
    let%F phid = result |> Json.get("phid") |?> Json.string;
    let%F userName = result |> Json.get("userName") |?> Json.string;
    let%F realName = result |> Json.get("realName") |?> Json.string;
    let%F image = result |> Json.get("image") |?> Json.string;
    Some({userName, realName, image, phid, loadedImage: None});
  };
};

module Repository = {
  type t = {
    phid: string,
    name: string,
    callsign: string,
  };
  let parse = result => {
    module F = Lets.Opt;
    open Json.Infix;
    let%F phid = result |> Json.get("phid") |?> Json.string;
    let%F fields = result |> Json.get("fields");
    let%F name = fields |> Json.get("name") |?> Json.string;
    let%F callsign = fields |> Json.get("callsign") |?> Json.string;
    Some({phid, name, callsign});
  };
};

module Diff = {
  type t = {
    id: int,
    phid: string,
    revisionPHID: string,
    authorPHID: string,
    repositoryPHID: string,
    branch: option(string),
    dateCreated: int,
    dateModified: int,
  };
  let parse = result => {
    module F = Lets.Opt;
    open Json.Infix;
    let%F phid = result |> Json.get("phid") |?> Json.string;
    let%F id = result |> Json.get("id") |?> Json.number |?>> int_of_float;
    let%F fields = result |> Json.get("fields");

    let%F repositoryPHID =
      fields |> Json.get("repositoryPHID") |?> Json.string;
    let%F revisionPHID = fields |> Json.get("revisionPHID") |?> Json.string;
    let%F authorPHID = fields |> Json.get("authorPHID") |?> Json.string;

    let%F dateCreated =
      fields |> Json.get("dateCreated") |?> Json.number |?>> int_of_float;
    let%F dateModified =
      fields |> Json.get("dateModified") |?> Json.number |?>> int_of_float;

    let branch =
      switch (
        fields |> Json.get("refs") |?> Json.array |?> Belt.List.get(_, 0)
      ) {
      | None => None
      | Some(obj) =>
        let%F typ = obj |> Json.get("type") |?> Json.string;
        let%F name = obj |> Json.get("name") |?> Json.string;
        if (typ == "branch") {
          Some(name);
        } else {
          None;
        };
      };

    Some({
      phid,
      id,
      repositoryPHID,
      authorPHID,
      revisionPHID,
      dateModified,
      dateCreated,
      branch,
    });
  };
};

module RJson = {
  let getString = (x, y) =>
    switch (Json.get(x, y)) {
    | None => Error("Unable to get " ++ x)
    | Some(Json.String(x)) => Ok(x)
    | Some(_) => Error(x ++ " is not a string")
    };
  let get = (x, y) =>
    switch (Json.get(x, y)) {
    | None => Error("Unable to get " ++ x)
    | Some(x) => Ok(x)
    };
  let array = x =>
    switch (Json.array(x)) {
    | None => Error("Not an array")
    | Some(x) => Ok(x)
    };
  let string = x =>
    switch (Json.string(x)) {
    | None => Error("Not a string")
    | Some(x) => Ok(x)
    };
  let number = x =>
    switch (Json.number(x)) {
    | None => Error("Not a number")
    | Some(x) => Ok(x)
    };
  module Infix = {
    let (|?>>) = (x, y) => {
      switch (x) {
      | Error(e) => Error(e)
      | Ok(v) => Ok(y(v))
      };
    };
    let (|?>) = (x, y) => {
      switch (x) {
      | Error(e) => Error(e)
      | Ok(v) => y(v)
      };
    };
  };
};

let tryMap = (fn, arr) => {
  let res =
    arr->Belt.List.reduce(Ok([]), (coll, item) =>
      switch (coll) {
      | Ok(v) =>
        switch (fn(item)) {
        | Ok(x) => Ok([x, ...v])
        | Error(e) => Error(e)
        }
      | Error(e) => Error(e)
      }
    );
  switch (res) {
  | Error(e) => Error(e)
  | Ok(v) => Ok(List.rev(v))
  };
};

module PR = {
  type team = {id: int, name: string, slug: string};
  let parseTeam = data => {
    module F = Lets.Try;
    open RJson.Infix;
    let%F id = data |> RJson.get("id") |?> RJson.number |?>> int_of_float;
    let%F name = data |> RJson.get("name") |?> RJson.string;
    let%F slug = data |> RJson.get("slug") |?> RJson.string;
    Ok({id, name, slug});
  };
  type user = {
    login: string,
    id: int,
    avatar_url: string,
    // loadedImage: option(FluidMac.Fluid.NativeInterface.image),
    html_url: string,
    team_ids: list(int),
  };

  let parseUser = data => {
    module F = Lets.Try;
    open RJson.Infix;

    let%F login = data |> RJson.get("login") |?> RJson.string;
    let%F id = data |> RJson.get("id") |?> RJson.number |?>> int_of_float;
    let%F avatar_url = data |> RJson.get("avatar_url") |?> RJson.string;
    let%F html_url = data |> RJson.get("html_url") |?> RJson.string;

    Ok({login, id, avatar_url, html_url, team_ids: []});
  };
  type label = {
    name: string,
    color: string,
    description: string,
    id: int,
  };
  let parseLabel = data => {
    module F = Lets.Try;
    open RJson.Infix;

    let%F name = data |> RJson.get("name") |?> RJson.string;
    let%F id = data |> RJson.get("id") |?> RJson.number |?>> int_of_float;
    let%F description = data |> RJson.get("description") |?> RJson.string;
    let%F color = data |> RJson.get("color") |?> RJson.string;

    Ok({name, color, description, id});
  };
  type repo = {
    id: int,
    name: string,
    full_name: string,
    html_url: string,
  };
  let parseRepo = data => {
    module F = Lets.Try;
    open RJson.Infix;

    let%F name = data |> RJson.get("name") |?> RJson.string;
    let%F id = data |> RJson.get("id") |?> RJson.number |?>> int_of_float;
    let%F full_name = data |> RJson.get("full_name") |?> RJson.string;
    let%F html_url = data |> RJson.get("html_url") |?> RJson.string;

    Ok({name, id, full_name, html_url});
  };
  type branch = {
    label: string,
    ref: string,
    sha: string,
    user,
    repo,
  };
  let parseBranch = data => {
    module F = Lets.Try;
    open RJson.Infix;

    let%F label = data |> RJson.get("label") |?> RJson.string;
    let%F user = data |> RJson.get("user") |?> parseUser;
    let%F repo = data |> RJson.get("repo") |?> parseRepo;
    let%F ref = data |> RJson.get("ref") |?> RJson.string;
    let%F sha = data |> RJson.get("sha") |?> RJson.string;

    Ok({label, user, repo, ref, sha});
  };
  type review = {
    id: int,
    user: user,
    body: string,
    state: string,
    html_url: string,
    submitted_at: string,
  };
  type check_output = {
    title: option(string),
    summary: option(string),
    text: option(string),
    annotations_count: int,
  };
  type check = {
    id: int,
    head_sha: string,
    status: string,
    conclusion: option(string),
    started_at: string,
    completed_at: option(string),
    output: check_output,
    name: string,
  };
  type t = {
    number: int,
    state: string,
    title: string,
    user,
    body: string,
    labels: list(label),
    created_at: string,
    updated_at: string,
    merged_at: option(string),
    assignee: option(user),
    assignees: list(user),
    requested_reviewers: list(user),
    requested_teams: list(team),
    reviews: list(review),
    checks: list(check),
    head: branch,
    base: branch,
    mergeable: option(bool),
  };

  let parse = result => {
    module F = Lets.Try;
    open RJson.Infix;
    let%F number =
      result |> RJson.get("number") |?> RJson.number |?>> int_of_float;
    let%F state = result |> RJson.get("state") |?> RJson.string;
    let%F title = result |> RJson.get("title") |?> RJson.string;
    let%F body = result |> RJson.get("body") |?> RJson.string;
    let%F created_at = result |> RJson.get("created_at") |?> RJson.string;
    let%F updated_at = result |> RJson.get("updated_at") |?> RJson.string;
    let mergeable = Some(true);
    // let mergeable = Json.Infix.(result |> Json.get("merge_commit_sha") |?> Json.bool);
    let merged_at =
      result |> RJson.get("merged_at") |?> RJson.string |> Lets.Try.ok;

    let%F user = result |> RJson.get("user") |?> parseUser;
    let%F assignees =
      result |> RJson.get("assignees") |?> RJson.array |?> tryMap(parseUser);
    let%F requested_reviewers =
      result
      |> RJson.get("requested_reviewers")
      |?> RJson.array
      |?> tryMap(parseUser);
    let%F requested_teams =
      result
      |> RJson.get("requested_teams")
      |?> RJson.array
      |?> tryMap(parseTeam);
    let%F labels =
      result |> RJson.get("labels") |?> RJson.array |?> tryMap(parseLabel);
    let assignee =
      result |> RJson.get("asignee") |?> parseUser |> Lets.Try.ok;

    let%F head = result |> RJson.get("head") |?> parseBranch;
    let%F base = result |> RJson.get("base") |?> parseBranch;

    Ok({
      number,
      state,
      title,
      user,
      body,
      labels,
      created_at,
      updated_at,
      merged_at,
      assignee,
      assignees,
      requested_reviewers,
      requested_teams,
      head,
      base,
      mergeable,
      reviews: [],
      checks: [],
    });
    // Some({
    //   phid,
    //   id,
    //   repositoryPHID,
    //   authorPHID,
    //   revisionPHID,
    //   dateModified,
    //   dateCreated,
    //   branch
    // });
  };
};

module Check = {
  let isPending = (check: PR.check) => check.status != "completed";
  let isFailed = (check: PR.check) => check.status == "completed" && (check.conclusion != Some("success") && check.conclusion != Some("neutral"));
  let isSucceeded = (check: PR.check) => check.status == "completed" && check.conclusion == Some("success");
  let parse = (result) => {
    module F = Lets.Try;
    open RJson.Infix;
    let%F id = result |> RJson.get("id") |?> RJson.number |?>> int_of_float;
    let%F head_sha = result |> RJson.getString("head_sha");
    let%F status = result |> RJson.getString("status");
    let conclusion = result |> RJson.getString("conclusion") |> Lets.Try.ok;
    let%F started_at = result |> RJson.getString("started_at");
    let completed_at = result |> RJson.getString("completed_at") |> Lets.Try.ok;
    let%F name = result |> RJson.getString("name");
    let outputObj = result |> RJson.get("output");
    let title = outputObj |?> RJson.getString("title") |> Lets.Try.ok;
    let summary = outputObj |?> RJson.getString("summary") |> Lets.Try.ok;
    let text = outputObj |?> RJson.getString("text") |> Lets.Try.ok;
    let%F annotations_count = outputObj |?> RJson.get("annotations_count") |?> RJson.number |?>> int_of_float;
    let output: PR.check_output = {
      title, summary, text, annotations_count,
    }
    Ok({
      id,
      head_sha,
      status,
      conclusion,
      started_at,
      completed_at,
      output,
      name,
    }: PR.check)
  }
}

module Review = {
  let isAccepted = (r: PR.review) => r.PR.state == "APPROVED";
  let isRejected = (r: PR.review) => r.PR.state == "CHANGES_REQUESTED";
  let isConclusive = (r: PR.review) => isAccepted(r) || isRejected(r);
  let parse = result => {
    module F = Lets.Try;
    open RJson.Infix;
    let%F id = result |> RJson.get("id") |?> RJson.number |?>> int_of_float;
    let%F user = result |> RJson.get("user") |?> PR.parseUser;
    let%F body = result |> RJson.get("body") |?> RJson.string;
    let%F state = result |> RJson.get("state") |?> RJson.string;
    let%F html_url = result |> RJson.get("html_url") |?> RJson.string;
    let%F submitted_at = result |> RJson.get("submitted_at") |?> RJson.string;

    Ok({PR.id, user, body, state, html_url, submitted_at})
  }
}

module Revision = {
  type t = {
    title: string,
    phid: string,
    id: int,
    repositoryPHID: string,
    repository: option(Repository.t),
    authorPHID: string,
    author: option(Person.t),
    diffPHID: string,
    diff: option(Diff.t),
    summary: string,
    dateCreated: int,
    dateModified: int,
    snoozed: bool,
    status: string,
    color: string,
  };

  let parseGithub = result => {
    module F = Lets.Try;
    open Json.Infix;
    let%F phid =
      result
      |> Json.get("number")
      |?> Json.number
      |?>> string_of_float
      |!! "no number";
    let%F id =
      result |> Json.get("id") |?> Json.number |?>> int_of_float |!! "no id";
    // let%F fields = result |> Json.get("fields");

    let%F title = result |> Json.get("title") |?> Json.string |!! "No title";
    // let repositoryPHID = "mobile";
    let%F repositoryPHID =
      result
      |> Json.get("base")
      |?> Json.get("repo")
      |?> Json.get("full_name")
      |?> Json.string
      |!! "no base full_name";
    let%F authorPHID =
      result
      |> Json.get("user")
      |?> Json.get("login")
      |?> Json.string
      |!! "no uiser login";
    let diffPHID = "";
    // let%F diffPHID = result |> Json.get("diffPHID") |?> Json.string;
    let%F summary =
      result |> Json.get("body") |?> Json.string |!! "no summary";
    let%F dateCreated =
      result
      |> Json.get("created_at")
      |?> Json.string
      |?>> ODate.Unix.From.string(dateParser)
      |?>> ODate.Unix.To.seconds
      |!! "no created date";
    let%F dateModified =
      result
      |> Json.get("updated_at")
      |?> Json.string
      |?>> ODate.Unix.From.string(dateParser)
      |?>> ODate.Unix.To.seconds
      |!! "no updated date";
    let%F status = result |> Json.get("state") |?> Json.string |!! "no state";
    // let%F status = statusObj |> Json.get("value") |?> Json.string |!! "no status value";
    // let%F color = statusObj |> Json.get("color.ansi") |?> Json.string;

    Ok({
      title,
      phid,
      id,
      snoozed: false,
      repositoryPHID,
      repository: None,
      authorPHID,
      author: None,
      diffPHID,
      diff: None,
      summary,
      dateModified,
      dateCreated,
      status,
      color: "green",
    });
  };

  let parse = result => {
    module F = Lets.Opt;
    open Json.Infix;
    let%F phid = result |> Json.get("phid") |?> Json.string;
    let%F id = result |> Json.get("id") |?> Json.number |?>> int_of_float;
    let%F fields = result |> Json.get("fields");

    let%F title = fields |> Json.get("title") |?> Json.string;
    let%F repositoryPHID =
      fields |> Json.get("repositoryPHID") |?> Json.string;
    let%F authorPHID = fields |> Json.get("authorPHID") |?> Json.string;
    let%F diffPHID = fields |> Json.get("diffPHID") |?> Json.string;
    let%F summary = fields |> Json.get("summary") |?> Json.string;
    let%F dateCreated =
      fields |> Json.get("dateCreated") |?> Json.number |?>> int_of_float;
    let%F dateModified =
      fields |> Json.get("dateModified") |?> Json.number |?>> int_of_float;
    let%F statusObj = fields |> Json.get("status");
    let%F status = statusObj |> Json.get("value") |?> Json.string;
    let%F color = statusObj |> Json.get("color.ansi") |?> Json.string;

    Some({
      title,
      phid,
      id,
      snoozed: false,
      repositoryPHID,
      repository: None,
      authorPHID,
      author: None,
      diffPHID,
      diff: None,
      summary,
      dateModified,
      dateCreated,
      status,
      color,
    });
  };

  type groups = {
    accepted: list(t),
    needsReview: list(t),
    needsRevision: list(t),
    changesPlanned: list(t),
  };
  type all = {
    mine: groups,
    theirs: groups,
  };

  let checkStatus = (status, r) => r.status == status;
  let makeGroups = (revisions: list(t)) => {
    accepted: revisions->Belt.List.keep(checkStatus("accepted")),
    needsReview: revisions->Belt.List.keep(checkStatus("needs-review")),
    needsRevision: revisions->Belt.List.keep(checkStatus("needs-revision")),
    changesPlanned:
      revisions->Belt.List.keep(checkStatus("changes-planned")),
  };
  let appendGroups = (a, b) => {
    accepted: List.append(a.accepted, b.accepted),
    needsReview: List.append(a.accepted, b.accepted),
    needsRevision: List.append(a.accepted, b.accepted),
    changesPlanned: List.append(a.accepted, b.accepted),
  };
  let append = (a, b) => {
    mine: appendGroups(a.mine, b.mine),
    theirs: appendGroups(a.theirs, b.theirs),
  };
  let organize = (person: Person.t, revisions: list(t)) => {
    mine:
      makeGroups(revisions->Belt.List.keep(r => r.authorPHID == person.phid)),
    theirs:
      makeGroups(revisions->Belt.List.keep(r => r.authorPHID != person.phid)),
  };
  let mapGroups = ({accepted, needsReview, needsRevision, changesPlanned}, m) => {
    accepted: m(accepted),
    needsReview: m(needsReview),
    needsRevision: m(needsRevision),
    changesPlanned: m(changesPlanned),
  };
  let map = ({mine, theirs}, m) => {
    mine: mapGroups(mine, m),
    theirs: mapGroups(theirs, m),
  };
};