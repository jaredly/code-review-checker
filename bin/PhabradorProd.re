let (/+) = Filename.concat;
let assetDir = Filename.dirname(Sys.argv[0]) /+ "assets";
let gitHead = "assets" /+ "git-head";
Phabrador.run(assetDir);
