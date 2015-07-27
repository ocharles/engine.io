{ mkDerivation, base, bytestring, conduit, conduit-extra, engine-io
, http-types, stdenv, text, unordered-containers, wai
, wai-websockets, websockets, mtl, transformers, attoparsec
, transformers-compat, either
}:
mkDerivation {
  pname = "engine-io-wai";
  version = "1.0.1";
  src = ./.;
  buildDepends = [
    base bytestring conduit conduit-extra engine-io http-types text
    unordered-containers wai wai-websockets websockets mtl attoparsec
    transformers transformers-compat either
  ];
  license = stdenv.lib.licenses.bsd3;
}

