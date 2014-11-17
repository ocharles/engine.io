{ cabal, aeson, engineIo, engineIoSnap, snapServer, snapCors, socketIo, stm, text
}:

cabal.mkDerivation (self: {
  pname = "chat-example";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson engineIo engineIoSnap snapCors snapServer socketIo text
  ];
  meta = {
    homepage = "http://github.com/ocharles/engine.io";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
