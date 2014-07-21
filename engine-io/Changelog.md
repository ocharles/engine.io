## 1.0.2

* The `ping` thread spawned by `websockets` is now disabled, as it has been
  observed that these pings are using invalid sockets. Specifically, see
  https://github.com/lpsmith/postgresql-simple/issues/117.

## 1.0.1

* Purposefully double-encode websocket traffic. Unfortunately this is necessary
  due to https://github.com/Automattic/engine.io-client/issues/322. When a new
  version of `engine.io` is released upstream, I will revert this.

## 1.0.0

* Initial release
