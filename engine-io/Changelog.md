## 1.0.1

* Purposefully double-encode websocket traffic. Unfortunately this is necessary
  due to https://github.com/Automattic/engine.io-client/issues/322. When a new
  version of `engine.io` is released upstream, I will revert this.

## 1.0.0

* Initial release
