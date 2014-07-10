var eio = require("engine.io-client");

var socket = new eio.Socket("http://localhost:8000");

socket.on('message', function(foo){
    debugger;
});
