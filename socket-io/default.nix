{ cabal, aeson, attoparsec, engineIo, mtl, stm, text, transformers
, unorderedContainers, vector
}:

cabal.mkDerivation (self: {
  pname = "engine-io";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson attoparsec engineIo mtl stm text transformers
    unorderedContainers vector
  ];
  meta = {
    homepage = "http://github.com/ocharles/engine.io";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
