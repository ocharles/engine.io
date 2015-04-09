{ mkDerivation, base, bytestring, conduit, conduit-extra, engine-io
, http-types, stdenv, text, unordered-containers, wai
, wai-websockets, websockets, yesod-core
}:
mkDerivation {
  pname = "engine-io-yesod";
  version = "1.0.2";
  src = ./.;
  buildDepends = [
    base bytestring conduit conduit-extra engine-io http-types text
    unordered-containers wai wai-websockets websockets yesod-core
  ];
  license = stdenv.lib.licenses.bsd3;
}
