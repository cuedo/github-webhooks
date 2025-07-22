{
  description = "A flake for developing or using github-webhooks";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc98;
    in {
      packages.default = haskellPackages.callCabal2nix "github-webhooks" ./. {};
      devShells.default = haskellPackages.shellFor {
        packages = _: [self.packages.${system}.default];
        buildInputs = [ pkgs.cabal-install ];
      };
  });
}
