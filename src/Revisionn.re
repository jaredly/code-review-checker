open FluidMac;
open Data;

open Fluid.Native;

let str = Fluid.string;
external openUrl: string => unit = "codeReviewChecker_openUrl";


let%component checks = (~checks: list(Data.PR.check), ~labels: list(Data.PR.label), hooks) => {
  let good = checks->Belt.List.keep(Data.Check.isSucceeded)->List.length;
  let waiting = checks->Belt.List.keep(Data.Check.isPending)->List.length;

  let children = [];
  let children = waiting > 0 ? [str(string_of_int(waiting) ++ " ⌛"), ...children] : children;
  let children = good > 0 ? [str(string_of_int(good) ++ " ✅"), ...children] : children;
  let failed = checks->Belt.List.keep(Data.Check.isFailed)
      ->Belt.List.sort((a, b) => compare(a.name, b.name))
      ->Belt.List.map(check =>
            str(check.name ++ "❌")
        );

  Fluid.Native.view(
    ~layout={
      Layout.style(~flexDirection=Row, ~flexWrap=CssWrap, ());
    },
    ~children={
      children @ failed @ Belt.List.map(labels, label => {
        <view backgroundColor={{FluidMac.r: 0.3, g: 0.3, b: 0.3, a: 1.0}}
        layout=Layout.style(
          ~alignItems=AlignCenter,
          ~justifyContent=JustifyCenter,
          ~alignSelf=AlignCenter,
          ())
        >
        {str(
          ~layout=Layout.style(
            ~marginBottom=4.0,
            ~marginLeft=4.0,
            ()
          ),
          label.name)}
        </view>
      })
    },
    (),
  );
};

let%component revision = (~rev: Data.PR.t, ~snoozeItem, hooks) => {
  let author = rev.user;
  // let repo = rev.repository;
  if (false) {
    failwith(
      "WIP",
      // if (rev.snoozed) {
      //   <view layout={Layout.style(
      //     ~paddingVertical=8.,
      //     ~marginHorizontal=8.,
      //     ~alignSelf=AlignStretch,
      //     ~flexDirection=Row,
      //     ())}
      //   >
      //     {str(~layout=Layout.style(~flexGrow=1., ()), ~font={fontName: "Helvetica", fontSize: 12.}, rev.Revision.title)}
      //     <button
      //       onPress={() => {
      //         if (rev.snoozed) {
      //           snoozeItem(rev.phid, None)
      //         } else {
      //           let tomorrow = ODate.Unix.From.seconds_float(Unix.time())
      //           -> ODate.Unix.beginning_of_the_day(~tz=ODate.Local, _)
      //           -> ODate.Unix.advance_by_days(1);
      //           snoozeItem(rev.phid, Some(tomorrow->ODate.Unix.To.seconds_float))
      //         }
      //       }}
      //       title={"Unsnooze"}
      //     />
      //   </view>
    );
  } else {
    <view
      layout={Layout.style(
        ~paddingVertical=8.,
        ~marginHorizontal=8.,
        ~flexDirection=Row,
        ~alignSelf=AlignStretch,
        (),
      )}>
      // onPress={() => openUrl(Api.diffUrl(rev.number))}

        <view>
          <view>
            <image
              src={
                switch (Api.cachedImage(author.avatar_url)) {
                | None =>
                  // print_endline("Uncached " ++ author.login);
                  Plain(author.avatar_url)
                | Some(i) => Preloaded(i)
                // Plain(
                //   author.avatar_url,
                // )
                }
              }
              layout={Layout.style(~margin=3., ~width=30., ~height=30., ())}
            />
            <button
              layout={Layout.style(~left=-5., ())}
              onPress={() => openUrl(Api.GitHub.diffUrl(rev.number))}
              title="Go"
            />
          </view>
        </view>
        <view layout={Layout.style(~flexGrow=1., ~flexShrink=1., ())}>
          <view layout={Layout.style(~flexDirection=Row, ())}>
            {str(
               ~layout=Layout.style(~flexGrow=1., ~flexShrink=1., ()),
               ~font={fontName: "Helvetica", fontSize: 18.},
               rev.title,
             )}
            <button
              onPress={() =>
                // if (rev.snoozed) {
                //   snoozeItem(rev.phid, None)
                // } else {
                //   let tomorrow = ODate.Unix.From.seconds_float(Unix.time())
                //   -> ODate.Unix.beginning_of_the_day
                //   -> ODate.Unix.advance_by_days(1);
                //   snoozeItem(rev.phid, Some(tomorrow->ODate.Unix.To.seconds_float))
                // }
                ()}
              // title={rev.snoozed ? "❗" : "💤"}
              title="zzz"
            />
          </view>
          {Fluid.Native.view(
             ~layout=
               {Layout.style(~flexDirection=Row, ~flexWrap=CssWrap, ())},
             ~children=
               if (rev.reviews != []) {
                 rev.reviews
                 ->Belt.List.map(review =>
                     <view layout={Layout.style(~flexDirection=Row, ())}>
                       <image
                         src={
                           switch (Api.cachedImage(review.user.avatar_url)) {
                           | None => Plain(review.user.avatar_url)
                           | Some(i) => Preloaded(i)
                           }
                         }
                         layout={Layout.style(
                           ~margin=3.,
                           ~width=20.,
                           ~height=20.,
                           (),
                         )}
                       />
                       {str(
                          Data.Review.isRejected(review)
                            ? "❌"
                            : Data.Review.isAccepted(review) ? "✅" : "💬",
                        )}
                     </view>
                   );
               } else if (rev.requested_reviewers != []
                          || rev.requested_teams != []) {
                 List.append(
                   rev.requested_reviewers
                   ->Belt.List.map(user =>
                       <view layout={Layout.style(~flexDirection=Row, ())}>
                         <image
                           src={
                             switch (Api.cachedImage(user.avatar_url)) {
                             | None => Plain(user.avatar_url)
                             | Some(i) => Preloaded(i)
                             }
                           }
                           layout={Layout.style(
                             ~margin=3.,
                             ~width=20.,
                             ~height=20.,
                             (),
                           )}
                         />
                         {str("⌛")}
                       </view>
                     ),
                   rev.requested_teams
                   ->Belt.List.map(team =>
                       <view layout={Layout.style(~flexDirection=Row, ())}>
                         {str(team.name)}
                         {str("⌛")}
                       </view>
                     ),
                 );
               } else
                 {
                   [str("❗ no reviewers requested")];
                 },
                 // {str(review.user.login)}
                 // {str(
                 //    ~font={
                 //      ...Fluid.NativeInterface.defaultFont,
                 //      fontSize: 8.0,
                 //    },
                 //    check.name,
                 //  )}
             (),
           )}
          <checks checks={rev.checks} labels={rev.labels} />
          <view layout={Layout.style(~flexDirection=Row, ())}>
            {str(Utils.recentDate(toSeconds(rev.updated_at)))}
            <view layout={Layout.style(~flexGrow=1., ())} />
            {rev.base.ref == "develop"
               ? Fluid.Null : str(rev.base.ref ++ " <-")}
            {str(rev.head.ref)}
          </view>
        </view>
      </view>;
  };
};