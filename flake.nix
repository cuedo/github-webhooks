{
  description = "GitHub webhooks Haskell library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { config, inputs', pkgs, system, ... }: {
        # https://flake.parts/options/haskell-flake.html#opt-perSystem.haskellProjects
        # where the <name> is "default"
        haskellProjects.default = {
          # any sub-options if needed
        };

        packages.default = config.packages.github-webhooks;
      };
    };
}
