let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      engineIo = self.callPackage ../engine-io {};
      engineIoSnap = self.callPackage ./. {};
    };
  };

in pkgs.lib.overrideDerivation haskellPackages.engineIoSnap (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })