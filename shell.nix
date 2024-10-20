with import <nixpkgs> {};

let
  ghc = haskell.packages.ghc96.ghcWithPackages (pkgs: with pkgs; [ zlib gd ]);
in
  stdenv.mkDerivation {
    name = "hmoe";
    src = ./.;
    buildInputs = [
      elmPackages.elm
      ghc
      haskellPackages.cabal-install
    ];
  }
