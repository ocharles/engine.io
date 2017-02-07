{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, engine-io
      , io-streams, lifted-base, snap-core, stdenv, unordered-containers
      , websockets, websockets-snap
      }:
      mkDerivation {
        pname = "engine-io-snap";
        version = "1.0.3";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring containers engine-io io-streams lifted-base
          snap-core unordered-containers websockets websockets-snap
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
