## 1.2.1

* Fixed a potential race condition in session allocation, where we could
  clobber existing session ids.

## 1.2.0

* `ServerAPI`'s `srvParseParseRequestBody` has changed its return type to
  `Either String a`. This allows API providers to catch exceptions that may
  happen when attempting to perform this parse.

## 1.1.2

* 1.1.1 accidently removed `websockets` from the list of available upgrades.
  This release reverts that change.

## 1.1.1

* Long-polling connections now emit a `ping` message after 45 seconds, if no
  data is written to them.

* There is a new `dupRawReader` function, which lets you create a read-only
  stream of raw communication with a socket.

## 1.1.0

* The `ServerAPI` functions `srvWriteBuilder`, `srvSetContentType` and
  `srvSetResponseCode` have been merged into a single function:
  `srvTerminateWithResponse`. This should allow `ServerAPI` to be provided for
  Yesod.

  Thanks to Tim Baumann (@timjb) for this change.

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
