let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      engineIo = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.engineIo.name;
     buildInputs = [
       pkgs.curl
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.engineIo.propagatedNativeBuildInputs)))
     ];
   }