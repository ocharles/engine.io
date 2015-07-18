{ mkDerivation, aeson, engine-io, engine-io-snap, snap-server, snap-cors
, socket-io, stm, text, stdenv, engine-io-yesod, engine-io-wai, wai, warp
, yesod-core, yesod-static, system-filepath, wai-app-static, wai-util
, http-types
}:

mkDerivation {
  pname = "chat-example";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson engine-io engine-io-snap snap-cors snap-server socket-io text
    engine-io-yesod engine-io-wai wai warp yesod-core yesod-static system-filepath
    wai-app-static wai-util http-types
  ];
  license = stdenv.lib.licenses.bsd3;
}
