let (/+) = Filename.concat;
/* let assetDir = Filename.dirname(Sys.argv[0]) /+ ".." /+ ".." /+ ".." /+ "assets"; */
let assetDir = "." /+ "assets";
/* CodeReviewChecker.Api.debug := true; */
CodeReviewChecker.GitHub.run(assetDir);
// CodeReviewChecker.run(assetDir);
