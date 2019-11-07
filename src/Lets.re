module Async = {
  type t('a) = ('a => unit) => unit;
  /* let try_ = (promise, continuation) => Js.Promise.catch(continuation, promise); */
  let let_: (t('a), 'a => t('b)) => t('b) = (promise, continuation) => fin => promise(v => continuation(v, fin));
  let resolve: 'a => t('a) = (data, fn) => fn(data);
  let all = (items, fn) => {
    let items = items->Belt.List.toArray;
    let results = items->Belt.Array.map((_) => None);
    let set = (i, value) => {
      results[i] = Some(value)
      if (results->Belt.Array.every(m => m != None)) {
        fn(results->Belt.Array.keepMap(m => m)->Belt.List.fromArray)
      }
    };
    items->Belt.Array.forEachWithIndex((i, fn) => {
      fn(set(i))
    });
  };
  /* let reject = Js.Promise.reject; */
  /* let map = (promise, fn) => Js.Promise.then_(v => Js.Promise.resolve(fn(v)), promise); */

  module Result = {
    type single('a) = t('a);
    type t('a, 'b) = (Belt.Result.t('a, 'b) => unit) => unit;
    let resolve: 'a => t('a, 'b) = (data, fn) => fn(Belt.Result.Ok(data));
    let reject: 'b => t('a, 'b) = (data, fn) => fn(Belt.Result.Error(data));
    let let_: (t('a, 'b), 'a => t('c, 'b)) => t('c, 'b) =
      (promise, continuation, fin) =>
        promise(v =>
          switch (v) {
          | Error(error) => fin(Error(error))
          | Ok(value) => continuation(value, fin)
          }
        );
    let try_: (t('a, 'b), 'b => t('a, 'c)) => t('a, 'c) = (promise, continuation, fin) =>
      promise(v => switch v {
        | Error(e) => continuation(e, fin)
        | Ok(value) => fin(Ok(value))
      });
    module Handle = {
      let try_: (t('a, 'b), 'b => single('a)) => single('a) = (promise, continuation, fin) =>
        promise(v => switch v {
          | Error(e) => continuation(e, fin)
          | Ok(value) => fin(value)
        });
    };
  };

  module Wrap = {
    let let_ = (promise, cont) => fin => promise(v => fin(cont(v)));
  };

  module Consume = {
    let let_ = (promise, cont) => promise(cont)
  };
};

module Guard = {
  let let_ = ((condition, default), continuation) =>
    if (condition) {
      continuation();
    } else {
      default;
    };
};

module Try = {
  let let_ = (a, b) =>
    switch (a) {
    | Error(e) => Error(e)
    | Ok(x) => b(x)
    };
  let map = (a, b) =>
    switch (a) {
    | Error(e) => Error(e)
    | Ok(x) => Ok(b(x))
    };
  let flatMap = let_;
  let try_ = (a, b) =>
    switch (a) {
    | Error(e) => b(e)
    | Ok(v) => Ok(v)
    };
  let force = t =>
    switch (t) {
    | Error(e) =>
      print_endline(Printexc.to_string(e));
      failwith("Force unwrapped an Error()");
    | Ok(v) => v
    };
  let ok = t => {
    switch t {
      | Ok(a) => Some(a)
      | Error(e) => {
        print_endline(e);
        None
      }
    }
  }
  module Force = {
    let force = t => switch t {
      | Ok(t) => t
      | Error(e) => failwith("Force unwrapped an Error(): " ++ e)
    };
    let let_: (result('a, string), 'a => 'b) => 'b = (a, b) => b(force(a));
  }
};

module TryWrap = {
  let let_ = Try.map;
};

module TryForce = {
  let let_ = (a, b) => b(Try.force(a));
};

module TryLog = {
  let let_ = (a, b) =>
    switch (a) {
    | Error(e) => print_endline(e)
    | Ok(v) => b(v)
    };
};

module Opt = {
  let let_ = (a, b) =>
    switch (a) {
    | None => None
    | Some(x) => b(x)
    };
  let map = (a, b) =>
    switch (a) {
    | None => None
    | Some(x) => Some(b(x))
    };
  let force = value =>
    switch (value) {
    | None => failwith("Force unwrapped a none")
    | Some(x) => x
    };
  let orError = (value, error) =>
    switch (value) {
    | Some(v) => Ok(v)
    | None => Error(error)
    };
  let flatMap = let_;

  module Force = {
    let let_ = (a, b) =>
      switch (a) {
      | None => failwith("Unwrapping an empty optional")
      | Some(x) => b(x)
      };
  };

  module If = {
    let let_ = (a, b) =>
      if (a) {
        b();
      } else {
        None;
      };
  };
  


  module Default = {
    let let_ = ((a, default), b) =>
      switch (a) {
      | None => default
      | Some(x) => b(x)
      };
    let or_ = (v, default) =>
      switch (v) {
      | None => default
      | Some(c) => c
      };
  };


  module Wrap = {
    let let_ = (a, b) =>
      switch (a) {
      | None => None
      | Some(x) => Some(b(x))
      };
  };

  module Or = {
    let let_ = (a, b) =>
      switch (a) {
      | None => b()
      | Some(x) => a
      };
  };

  module Consume = {
    let let_ = (a, b) =>
      switch (a) {
      | None => ()
      | Some(x) => b(x)
      };
  };

};

module UnitIf = {
  let let_ = (a, b) =>
    if (a) {
      b();
    } else {
      ();
    };
};
