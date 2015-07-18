let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      engine-io = self.callPackage ../engine-io {};
      socket-io = self.callPackage ../socket-io {};
      engine-io-snap = self.callPackage ../engine-io-snap {};
      engine-io-yesod = self.callPackage ../engine-io-yesod {};
      engine-io-wai = self.callPackage ../engine-io-wai {};
      chatExample = self.callPackage ./. {};
    };
  };

in haskellPackages.chatExample.env
