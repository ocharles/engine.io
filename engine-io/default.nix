{ cabal, aeson, async, attoparsec, base64Bytestring, monadLoops
, mwcRandom, stm, text, transformers, unorderedContainers, vector
, websockets
}:

cabal.mkDerivation (self: {
  pname = "engine-io";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson async attoparsec base64Bytestring monadLoops mwcRandom stm
    text transformers unorderedContainers vector websockets
  ];
  meta = {
    homepage = "http://github.com/ocharles/engine.io";
    description = "A Haskell implementation of Engine.IO";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
