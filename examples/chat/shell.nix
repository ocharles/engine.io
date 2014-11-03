let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      engineIo = self.callPackage ../../engine-io {};
      socketIo = self.callPackage ../../socket-io {};
      engineIoSnap = self.callPackage ../../engine-io-snap {};
      engineIoYesod = self.callPackage ../../engine-io-yesod {};
      example = self.callPackage ./. {};
    };
  };

in pkgs.lib.overrideDerivation haskellPackages.example (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })