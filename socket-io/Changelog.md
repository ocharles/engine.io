## 1.3.7

* Increased the upper-bound of `aeson` and `vector`.

## 1.3.6

* Increased the upper-bound of `base` and `transformers`.

## 1.3.5

* Increase upper-bound of `aeson` to < 0.12.

## 1.3.4

* Increased the upper-bound of `aeson` to allow < 0.11.

## 1.3.3

* Increase upper-bound of `vector` to < 0.12.

## 1.3.2

* Increased upper-bounds of aeson to < 0.10 and of attoparsec to < 0.14.

## 1.3.1

* Increased the upper-bound of base to allow < 4.9. Now builds on
  GHC 7.10.1.

## 1.3.0

* Change `on` to be variadic. `on` is now capable of parsing an arbitrary amount
  of arguments from the event payload (0 or more). This API should change should
  be backwards compatible with previous uses of `socket-io`, but allows more uses.

## 1.2.0

* Change `appendDisconnectHandler` to work in the `EventHandler` monad. This
  allows you to broadcast messages to other clients when a socket disconnects.
  The chat example has been updated to broadcast a "user left" message to
  demonstrate this.

  Thanks to Kayo Phoenix (@katyo) for the majority of this work!

## 1.1.1

* Build with `engine-io` 1.2

## 1.1.0

* When building an initial routing table, you now have access to the `Socket`.

* `engineIOSocket` lets you access the underlying `engine-io` `Socket` for a
  Socket.io session.

## 1.0.1

* Increase upper-bound of engine-io to <1.2

## 1.0.0

* Initial implementation
