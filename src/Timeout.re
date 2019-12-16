
module TimeoutTracker = FluidMac.Tracker({type arg = unit; let name = "codeReviewChecker_timeout_cb"; let once = true; type res = unit});
external setTimeout: (TimeoutTracker.callbackId, int) => unit = "codeReviewChecker_setTimeout";
let setTimeout = (fn, time) => setTimeout(TimeoutTracker.track(fn), time);
