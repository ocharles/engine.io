{ mkDerivation, attoparsec-enumerator, base, bytestring, containers
, engine-io, MonadCatchIO-transformers, snap-core, stdenv
, unordered-containers, websockets, websockets-snap
}:
mkDerivation {
  pname = "engine-io-snap";
  version = "1.0.2";
  src = ./.;
  buildDepends = [
    attoparsec-enumerator base bytestring containers engine-io
    MonadCatchIO-transformers snap-core unordered-containers websockets
    websockets-snap
  ];
  homepage = "http://github.com/ocharles/engine.io";
  license = stdenv.lib.licenses.bsd3;
}
