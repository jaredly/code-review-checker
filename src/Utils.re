
let timePrinter = ODate.Unix.To.generate_printer("%I:%M%P")->Lets.Opt.force;
let datePrinter = ODate.Unix.To.generate_printer("%b %E, %Y")->Lets.Opt.force;

let recentDate = seconds => {
  open ODate.Unix;
  let date = From.seconds(seconds);
  let now = now();
  let today = beginning_of_the_day(~tz=ODate.Local, now);
  let yesterday = today->advance_by_days(-1);

  if (date > today) {
    "Today at " ++ To.string(~tz=ODate.Local, timePrinter, date);
  } else if (date > yesterday) {
    "Yesterday at " ++ To.string(~tz=ODate.Local, timePrinter, date);
  } else {
    let diff = between(date, now);
    let days = ODuration.To.day(diff);
    if (days <= 10) {
      Printf.sprintf("%d days ago", days);
    } else if (days <= 7 * 4) {
      Printf.sprintf("%d weeks ago", days / 7);
    } else {
      To.string(~tz=ODate.Local, datePrinter, date);
    };
  };
};
