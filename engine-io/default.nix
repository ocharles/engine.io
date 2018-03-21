{ mkDerivation, aeson, async, attoparsec, base, base64-bytestring
, bytestring, errors, free, monad-loops, mwc-random, stdenv, stm
, stm-delay, text, transformers, unordered-containers, vector
, websockets, zlib
}:
mkDerivation {
  pname = "engine-io";
  version = "1.2.18";
  src = ./.;
  libraryHaskellDepends = [
    aeson async attoparsec base base64-bytestring bytestring errors
    free monad-loops mwc-random stm stm-delay text transformers
    unordered-containers vector websockets zlib
  ];
  homepage = "http://github.com/ocharles/engine.io";
  description = "A Haskell implementation of Engine.IO";
  license = stdenv.lib.licenses.bsd3;
}
