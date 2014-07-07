{ cabal, aeson, attoparsec, async, base64Bytestring, httpTypes, monadLoops, mwcRandom, stm, vector, websockets
, snapServer, snapCORS, attoparsecEnumerator, websocketsSnap
}:
cabal.mkDerivation (self: {
  pname = "engine-io";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson attoparsec async base64Bytestring httpTypes monadLoops mwcRandom stm vector websockets
    snapServer snapCORS attoparsecEnumerator websocketsSnap
  ];
})