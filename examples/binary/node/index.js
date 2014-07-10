var express = require('express')
  , app = express()
  , server = require('http').createServer(app)
  , enchilada = require('enchilada')
  , io = require('engine.io').attach(server)
  , fs = require('fs');

app.use(enchilada({
  src: __dirname + '/public',
  debug: true
}));
app.use(express.static(__dirname + '/public'));
app.get('/', function(req, res, next){
  res.sendfile('index.html');
});

io.on('connection', function(socket) {
    var doge = fs.readFile('doge.png', function (err, buf) {
        setTimeout(function() {
            socket.send(buf)
        }, 1000);
    });
});

var port = process.env.PORT || 3000;
server.listen(port, function(){
  console.log('\033[96mlistening on localhost:' + port + ' \033[39m');
});
