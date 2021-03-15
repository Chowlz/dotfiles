{ ... }:

{
  modules.yabai = {
    enable = true;
    config = {
      windowPlacement = "second_child";
      windowTopmost = "off";
      windowOpacity = "off";
      windowOpacityDuration = 0.0;
      activeWindowOpacity = 1.0;
      windowShadow = "on";
      autoBalance = "off";
      splitRatio = 0.50;
      mouseModifier = "ctrl";
      mouseAction1 = "move";
      mouseAction2 = "resize";
      layout = "bsp";
      topPadding = 2;
      bottomPadding = 2;
      leftPadding = 2;
      rightPadding = 2;
    };
    rules = [
      {
        app = { regex = "^Cisco .*$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^Disk Utility$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^Finder$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^iTerm2$"; };
        manage = "off";
      }
      {
        app = { regex = "^Spotify$"; };
        manage = "off";
      }
      {
        app = { regex = "^System Preferences$"; };
        sticky = "on";
        layer = "above";
        manage = "off";
      }
      {
        app = { regex = "^Terminal$"; };
        manage = "off";
      }
    ];
  };
}
