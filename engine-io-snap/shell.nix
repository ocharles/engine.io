let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskell-ng.packages.ghc7101.override {
    overrides = self: super: {
      engine-io = self.callPackage ../engine-io {};
      engineIoSnap = self.callPackage ./. {};
    };
  };

in haskellPackages.engineIoSnap.env
