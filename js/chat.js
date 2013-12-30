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
  var mainRoom = '$main';
  var user = {};

  user.rooms = {};
  user.currentRoom = mainRoom;
  user.roomHistory = [];
  user.$messages = {};
  user.$messages[user.currentRoom] = $( '#messages' );

  function refreshOthers() {
    var room = user.currentRoom;
    user.$messages[room].children( '.others' ).html('');
    user.rooms[room].keys().forEach(function () {
      user.$messages[room].children( '.others' ).append( $('<li .nick>' + e + '</li>' ))
    })
  }

  function showRoom(room) {
    if (room != user.currentRoom) {
      user.$messages[user.currentRoom].hide();
      user.$messages[room].show();
      user.currentRoom = room;
      refreshOthers()
    }
  }

  function warn(content) {
    user.$messages[user.currentRoom].children( '.messages' ).prepend( $('<li .ui>' + content + '</li>') );
  }

  function server(content) {
    user.$messages[user.currentRoom].children( '.messages' ).prepend( $('<li .server>' + content + '</li>') );
  }

  var box = $( '#box' );

  box.keydown(function (event) {
    if (event.ctrlKey && 13 == event.keyCode) {
      var content = box.val();
      if (oncommand(content)) box.val( '' );
      event.preventDefault();
    }
  })

  function oncommand(content) {
    var match = /\s*\/(\w+)\s*(.*)/.exec(content);

    var nick = {
      info: '/nick <nickname> change your nickname',
      handler: function (name) {
        var match = /(\w+)/.exec(name);
        if (match) {
          user.ws.send(ChangeNick(match[1].trim()));
          return true
        } else {
          warn('invalid name');
          return false
        }
      }
    }

    var dispatch = {
      nick: nick,
      nickname: nick,
      leave: {
        info: '/leave leave the current room'
        handler: function () {
          var room = user.currentRoom;
          if (room != mainRoom) {
            user.ws.send(LeaveRoom(room));
            showRoom(user.roomHistory.pop());
            user.$messages[room].remove();
            delete user.rooms[room];
            return true
          } else {
            warn('cannot leave the main room');
            return false
          }
        }
      },
      new: {
        info: '/new - create a new room with a generated name'
        handler: function () { user.ws.send(NewRoom); return true }
      },
      join: {
        info: '/join <room> - join a room'
        handler: function (room) {
          room = room.trim();
          if (room in user.rooms) {
            warn('already in that room');
            return false
          }
          if (/\w+/.test(room)) {
            user.ws.send(JoinRoom(room));
            return true
          }
          return false
        }
      }
    }

    if (match) {
      var command = match[1].toLower();
      if (dispatch[command]) {
        dispatch[command].handler(match[2]);
      } else {
        warn('invalid command');
        return false
      }
    }

    // otherwise, say it
    content = $( '<div></div>' ).text(content).html().replace('\n','<br>');
    user.$messages[user.currentRoom].children( '.messages').prepend( $('<li .me>' + content + '</li>') );
    user.ws.send(Say(user.currentRoom,content));

    return true
  }

  function onmessage (event) {
    cases(event.data,{
      JoinedRoom: function (room,others) {
        user.rooms[room] = {};
        others.forEach(function (e) { user.rooms[room][e] = null });
        user.$messages[room] = $( '<div .pane><ul .messages></ul><ul .others></ul></div>' );
        user.$messages[user.currentRoom].after( user.$messages[room] );
        user.roomHistory.push(user.currentRoom);
        showRoom(room,true)
      },
      Door: function (room,joined,nick) {
        if (joined) {
          user.rooms[room][nick] = null
        } else {
          delete user.rooms[room][nick]
        }
        if (room == currentRoom) { refreshOthers() };
      },
      WasSaid: function (room,nick,content) {
        user.$messages[room].children( '.messages').prepend( $( '<li .them><span .nick>' + nick + ':</span>' + content + '</li>') )
      },
      ServerError: server
    })
  }

  user.ws = createWebSocket('/',function () { warn('WebSocket established') },onmessage);
})
