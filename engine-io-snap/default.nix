{ cabal, engineIo, snapCore, websocketsSnap }:

cabal.mkDerivation (self: {
  pname = "engine-io-snap";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    engineIo snapCore websocketsSnap
  ];
  meta = {
    homepage = "http://github.com/ocharles/engine.io";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
