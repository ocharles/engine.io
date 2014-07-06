with import <nixpkgs> {};
let
  haskellPackages = haskellPackages_ghc782.profiling.override {
    extension = self: super: {
      engineIo = self.callPackage ./. {};
      attoparsecIteratee = self.callPackage ./attoparsec-iteratee.nix {};
    };
  };

in lib.overrideDerivation haskellPackages.engineIo (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })