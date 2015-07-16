{ mkDerivation, aeson, engine-io, engine-io-snap, snap-server, snap-cors,
socket-io, stm, text, stdenv
}:

mkDerivation {
  pname = "chat-example";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson engine-io engine-io-snap snap-cors snap-server socket-io text
  ];
  license = stdenv.lib.licenses.bsd3;
}
