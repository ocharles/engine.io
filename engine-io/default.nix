{ cabal, aeson, async, attoparsec, base64Bytestring, monadLoops
, mwcRandom, stm, text, transformers, unorderedContainers, vector
, websockets, either, stmDelay
}:

cabal.mkDerivation (self: {
  pname = "engine-io";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson async attoparsec base64Bytestring monadLoops mwcRandom stm
    text transformers unorderedContainers vector websockets either
    stmDelay
  ];
  meta = {
    homepage = "http://github.com/ocharles/engine.io";
    description = "A Haskell implementation of Engine.IO";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
