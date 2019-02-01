

open FluidMac;

let str = Fluid.string;

let main = (~assetsDir as _, _hooks) => {
  <view>
    {str("Hello")}
  </view>
}

let run = assetsDir => {

  /* startChecking(assetsDir); */

  Fluid.App.launch(
    ~isAccessory=true,
    () => {
    Fluid.App.setupAppMenu(
      ~title="Phabrador",
      ~appItems=[||],
      ~menus=[| Fluid.App.defaultEditMenu() |]
    );

    let closeWindow = ref(() => ());

    let win = Fluid.launchWindow(
      ~title="Phabrador",
      ~floating=true,
      ~hidden=true,
      ~onBlur=win => {
        Fluid.Window.hide(win);
      },
      <Main assetsDir />
    );

    closeWindow := () => Fluid.Window.hide(win);

    let _statusBarItem = Fluid.App.statusBarItem(
      ~isVariableLength=true,
      ~title=String("✅ 3 | ❌ 2 | 🙏 4"),
      ~onClick=pos => {
        Fluid.Window.showAtPos(win, pos)
      }
    );
  });

};
