{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring
      , engine-io, mtl, stdenv, stm, text, transformers
      , unordered-containers, vector
      }:
      mkDerivation {
        pname = "socket-io";
        version = "1.3.6";
        src = ./.;
        libraryHaskellDepends = [
          aeson attoparsec base bytestring engine-io mtl stm text
          transformers unordered-containers vector
        ];
        homepage = "http://github.com/ocharles/engine.io";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
