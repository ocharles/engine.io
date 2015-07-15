{ mkDerivation, aeson, attoparsec, base, bytestring, engine-io, mtl
, stdenv, stm, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "socket-io";
  version = "1.3.0";
  src = ./.;
  buildDepends = [
    aeson attoparsec base bytestring engine-io mtl stm text
    transformers unordered-containers vector
  ];
  homepage = "http://github.com/ocharles/engine.io";
  license = stdenv.lib.licenses.bsd3;
}
