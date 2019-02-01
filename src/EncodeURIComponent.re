let safe_chars = {
  let a = Array.make(256, false);
  let always_safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-~";
  for (i in 0 to String.length(always_safe) - 1) {
    let c = Char.code(always_safe.[i]);
    a[c] = true;
  };
  a;
};

let sub_delims = (a) => {
  let subd = "!$&'()*+,;=";
  for (i in 0 to String.length(subd) - 1) {
    let c = Char.code(subd.[i]);
    a[c] = true;
  };
  a;
};

let pchar = {
  let a = sub_delims(Array.copy(safe_chars));
  a[Char.code(':')] = true;
  a[Char.code('@')] = true;
  a;
};

let safe_chars_for_query = {
  /* TODO: What about {"!","$",","}? See <https://github.com/avsm/ocaml-uri/commit/1ef3f1dfb41bdb4f33f223ffe16e62a33975661a#diff-740f2de53c9eb36e9670ddfbdb9ba914R171> */
  let a = Array.copy(pchar);
  a[Char.code('/')] = true;
  a[Char.code('?')] = true;
  /* '&' is safe but we should encode literals to avoid ambiguity
     with the already parsed qs params */
  a[Char.code('&')] = false;
  /* ';' is safe but some systems treat it like '&'. */
  a[Char.code(';')] = false;
  a[Char.code('+')] = false;
  a;
};

/** Scan for reserved characters and replace them with
      percent-encoded equivalents.
      @return a percent-encoded string */
let encode = (b) => {
  let safe_chars = safe_chars_for_query;
  let len = String.length(b);
  let buf = Buffer.create(len);
  let rec scan = (start, cur) =>
    if (cur >= len) {
      Buffer.add_substring(buf, b, start, cur - start);
    } else {
      let c = Char.code(b.[cur]);
      if (safe_chars[c]) {
        scan(start, cur + 1);
      } else {
        if (cur > start) {
          Buffer.add_substring(buf, b, start, cur - start);
        };
        Buffer.add_string(buf, Printf.sprintf("%%%02X", c));
        scan(cur + 1, cur + 1);
      };
    };
  scan(0, 0);
  Buffer.contents(buf);
};
