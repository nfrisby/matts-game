function cases(message,cases) {
  // tag and contents are the default names from the aeson Haskell package
  var tag = message.tag;
  var contents = message.contents;
  if (undefined == contents) { // record types do not have a contents field
    contents = jQuery.extend({},message);
    delete contents.tag; // now contents just has the record fields
    contents = [contents]
  }
  var fun = cases[tag];
  if (undefined == fun) fun = cases['_'];
  if (undefined == fun) throw "Non-exhaustive case";
  return fun.apply(message,contents)
}

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
