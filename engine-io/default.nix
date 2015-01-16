{ mkDerivation, aeson, async, attoparsec, base, base64-bytestring
, bytestring, either, free, monad-loops, mwc-random, stdenv, stm
, stm-delay, text, transformers, unordered-containers, vector
, websockets
}:
mkDerivation {
  pname = "engine-io";
  version = "1.2.3";
  sha256 = "05sc939x24f3na6nxywgbq0yx4wdamh6zk4spdclqgq65j7v0hxc";
  buildDepends = [
    aeson async attoparsec base base64-bytestring bytestring either
    free monad-loops mwc-random stm stm-delay text transformers
    unordered-containers vector websockets
  ];
  homepage = "http://github.com/ocharles/engine.io";
  description = "A Haskell implementation of Engine.IO";
  license = stdenv.lib.licenses.bsd3;
}
