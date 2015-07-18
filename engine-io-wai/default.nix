{ mkDerivation, base, bytestring, conduit, conduit-extra, engine-io
, http-types, stdenv, text, unordered-containers, wai
, wai-websockets, websockets, mtl, transformers, attoparsec
}:
mkDerivation {
  pname = "engine-io-wai";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    base bytestring conduit conduit-extra engine-io http-types text
    unordered-containers wai wai-websockets websockets mtl attoparsec
    transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
