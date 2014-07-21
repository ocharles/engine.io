/*

This example is adapted from the engine.io source code.

Copyright (c) 2014 Guillermo Rauch <guillermo@learnboost.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the 'Software'), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

// helper

function $(id){ return document.getElementById(id); }

// chart

var smoothie;
var time;

function render(){
  if (smoothie) smoothie.stop();
  $('chart').width = document.body.clientWidth;
  smoothie = new SmoothieChart({ interpolation:'step', maxValue: 30, minValue: 0 });
  smoothie.streamTo($('chart'), 1000);
  time = new TimeSeries();
  smoothie.addTimeSeries(time, {
    strokeStyle: 'rgb(255, 0, 0)',
    fillStyle: 'rgba(255, 0, 0, 0.1)',
    lineWidth: 1
  });
}

// socket
var socket = new eio.Socket("http://localhost:8000");
var last;
function send(){
  socket.send('ping');
  last = new Date;
  $('transport').innerHTML = socket.transport.name;
}
socket.on('open', function(){
  if ($('chart').getContext) {
    render();
    window.onresize = render;
  }
  send();
});
socket.on('close', function(){
  if (smoothie) smoothie.stop();
  $('transport').innerHTML = '(disconnected)';
});
socket.on('message', function(){
  var latency = new Date - last;
  $('latency').innerHTML = latency + 'ms';
  if (time) time.append(+new Date, latency);
  setTimeout(send, 100);
});
