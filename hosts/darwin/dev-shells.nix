{ system, inputs }:

let
  pkgs = import inputs.determinate-nixpkgs { system = system.arch.aarch64-darwin; };
in
pkgs.mkShellNoCC {
  packages = with pkgs; [
    # For: nix develop --command nix-darwin-build
    (writeShellApplication {
      name = "nix-darwin-build";
      runtimeInputs = [
        # Make the darwin-rebuild package available in the script
        inputs.nix-darwin.packages.${system.arch.aarch64-darwin}.darwin-rebuild
      ];
      text = ''
        echo "> Running darwin-rebuild switch as root. 🛠️.."
        sudo darwin-rebuild build --flake "path:$(pwd)#darwin"
        echo "> darwin-rebuild build was successful ✅"
      '';
    })
    # For: nix develop --command nix-darwin-switch
    (writeShellApplication {
      name = "nix-darwin-switch";
      runtimeInputs = [
        # Make the darwin-rebuild package available in the script
        inputs.nix-darwin.packages.${system.arch.aarch64-darwin}.darwin-rebuild
      ];
      text = ''
        echo "> Running darwin-rebuild switch as root. 🛠️.."
        sudo darwin-rebuild switch --flake "path:$(pwd)#darwin"
        echo "> darwin-rebuild switch was successful ✅"
      '';
    })
  ];
}