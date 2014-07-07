with import <nixpkgs> {};
let
  haskellPackages = haskellPackages_ghc782.profiling.override {
    extension = self: super: {
      engineIo = self.callPackage ../engine-io {};
      socketIo = self.callPackage ./. {};
    };
  };

in lib.overrideDerivation haskellPackages.socketIo (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 git ] ++ attrs.buildInputs;
   })