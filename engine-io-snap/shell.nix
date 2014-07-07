with import <nixpkgs> {};
let
  haskellPackages = haskellPackages_ghc782.profiling.override {
    extension = self: super: {
      engineIo = self.callPackage ../engine-io {};
      engineIoSnap = self.callPackage ./. {};
    };
  };

in lib.overrideDerivation haskellPackages.engineIoSnap (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 git ] ++ attrs.buildInputs;
   })