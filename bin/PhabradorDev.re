let (/+) = Filename.concat;
/* let assetDir = Filename.dirname(Sys.argv[0]) /+ ".." /+ ".." /+ ".." /+ "assets"; */
let assetDir = "." /+ "assets";
/* Phabrador.Api.debug := true; */
Phabrador.run(assetDir);
