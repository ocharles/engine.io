{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, attoparsec, base
      , base64-bytestring, bytestring, either, free, monad-loops
      , mwc-random, stdenv, stm, stm-delay, text, transformers
      , unordered-containers, vector, websockets
      }:
      mkDerivation {
        pname = "engine-io";
        version = "1.2.15";
        src = ./.;
        libraryHaskellDepends = [
          aeson async attoparsec base base64-bytestring bytestring either
          free monad-loops mwc-random stm stm-delay text transformers
          unordered-containers vector websockets
        ];
        homepage = "http://github.com/ocharles/engine.io";
        description = "A Haskell implementation of Engine.IO";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
