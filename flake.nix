{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hspkgs = pkgs.haskellPackages;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            hspkgs.ghc # Compiler
            hspkgs.cabal-install # Package manager
            hspkgs.haskell-language-server # HLS
            hspkgs.hlint # Linter
            hspkgs.fourmolu # Formatter (alternative to ormolu)
            pkgs.git # Version control
            pkgs.zlib # Needed for some Haskell packages
          ];

          shellHook = ''
            echo "Welcome to your Haskell development environment!"
          '';
        };
      });
}
