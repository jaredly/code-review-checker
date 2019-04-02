
module TimeoutTracker = FluidMac.Tracker({type arg = unit; let name = "phabrador_timeout_cb"; let once = true; type res = unit});
external setTimeout: (TimeoutTracker.callbackId, int) => unit = "phabrador_setTimeout";
let setTimeout = (fn, time) => setTimeout(TimeoutTracker.track(fn), time);
