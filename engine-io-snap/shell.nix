let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      engineIo = self.callPackage ../engine-io {};
      engineIoSnap = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.engineIoSnap.name;
     buildInputs = [
       pkgs.curl
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.engineIoSnap.propagatedNativeBuildInputs)))
     ];
   }