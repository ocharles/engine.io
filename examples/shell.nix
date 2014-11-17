let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      engineIo = self.callPackage ../engine-io {};
      socketIo = self.callPackage ../socket-io {};
      engineIoSnap = self.callPackage ../engine-io-snap {};
      engineIoYesod = self.callPackage ../engine-io-yesod {};
      chatExample = self.callPackage ./. {};
    };
  };

in pkgs.lib.overrideDerivation haskellPackages.chatExample (attrs: {
     buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
   })
