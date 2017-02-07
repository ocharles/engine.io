## 1.0.4

* Increased the upper-bound of `base`, `snap-core`, `websockets` and `websockets-snap`.
* Snap 1.0 is now required.
* Use `lifted-base` instead of `MonadCatchIO-transformers`.

## 1.0.3

* Increased the upper-bound of base to allow < 4.9. Now builds on
  GHC 7.10.1.

## 1.0.2

* Updated for `ServerAPI` in `engine-io-1.2`.

* ParseError is now caught correctly, and no longer results in a response 500 to the client.

## 1.0.1

* Updated for the new `ServerAPI` in `engine-io-1.1`.

* Fixed incomplete pattern matches when parsing request method.

## 1.0.0

* Initial release.
