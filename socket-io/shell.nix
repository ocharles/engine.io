let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      engineIo = self.callPackage ../engine-io {};
      socketIo = self.callPackage ./. {};
    };
  };

in pkgs.lib.overrideDerivation haskellPackages.socketIo (attrs: {
     buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
   })
