// var wsPort is defined in getHomeR

function createWebSocket(path,onopen,onmessage) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':' + wsPort + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    var ws = new Socket(uri);
    ws.onopen = onopen;
    ws.onmessage = onmessage;
    return ws;
}

$( document ).ready(function () {
  var messages = $( '#messages' );
  var box = $( '#box' );

  box.keydown(function (event) {
    if (event.ctrlKey && 13 == event.keyCode) {
      var content = box.val();
      box.val( '' );
      content = $( '<div></div>' ).text(content).html().replace('\n','<br>');
      messages.prepend( $('<p .me>' + content + '</p>') );
      event.preventDefault();
    }
  })

  function onFirstMessage (event) {
    
    this.onmessage = onMessage;
  }

  function onMessage (event) {
  
  }

  var ws = createWebSocket('/',function () {},onFirstMessage);
})
