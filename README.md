engine.io
=========

This repository contains the following projects:

* [`engine-io`[(./engine-io) contains a Haskell implementation of [Engine.IO](http://github.com/automattic/engine.io), a real-time abstraction for the web, allowing one to interact with clients in a socket-like fashion (rather than the more traditional request-response pattern). Supports long-polling along with upgrading to WebSockets. This library is API agnostic - you need to connect it your web server by providing a `ServerAPI` object.

* [`engine-io-snap`](./engine-io-snap) provides a `ServerAPI` that can be used to connect run an Engine.IO application in [Snap](http://snapframework.com).

* [`socket-io`](./socket-io) implements the [Socket.IO](http://socket.io) protocol on top of `engine-io`. Socket.IO is a higher-level abstraction based on events. Clients send events to the server, which can respond by sending events back to clients, or broadcasting events to all other clients.

* [`examples`](./examples) contains an example `engine-io` application (a latency test) and a chat server application written using `socket-io`.
