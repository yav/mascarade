

function newClient(ws,display) {

  var connected = false;
  var ourId;
  var currentPlayer;
  var playerMoney;
  var activeRoles;
  var npcNum;

  function playerName(playerId) {
    var names = [ "Alfirin"
                , "Belegorn"
                , "Cullas"
                , "Doron"
                , "Elanor"
                , "Fêr"
                , "Galenas"
                , "Halloth"
                , "Iorthon"
                , "Loth"
                , "Meril"
                , "Nínim"
                , "Ophen"
                ];
    if (playerId >= 0 && playerId < names.length) return names[playerId];
    return ('player ' + playerId);
  }


  function drawRoom(info) {
    display.empty();
    display.append($('<h1/>').text(info.name));
    var table = $('<table/>');
    display.append(table);

    jQuery.each(info.players, function(ix, player) {
      var tr = $('<tr/>');
      table.append(tr);

      var name = $('<td/>').text(playerName(player.player));
      var stat = $('<td/>').text(player.ready ? 'ready' : 'not ready');

      if (player.player === info.you_are) {
        name.css('font-weight', 'bold');
        stat.css('cursor', 'pointer')
            .click(function() {
                console.log('click');
            });
      }

      tr.append(name).append(stat);
    });
  }

  function choosePlayer(opts) {
    var me = $('<table/>');
    jQuery.each(opts, function(ix, pid) {
      var money = playerMoney[pid];
      if (money === undefined) money = '?';
      var row = $('<tr/>')
                .append($('<td/>').append($('<input/>')
                                          .attr('type','radio')
                                          .attr('name','player')
                                          .attr('value',pid)
                                          .append(pid)))
                .append($('<td/>').append(pid))
                .append($('<td/>').append('$' + money));
    });
  }


  ws.onmessage = function (msg) {
     data = JSON.parse(msg.data);

     switch (data.what) {

       case "room":
         drawRoom(data.info); break;

/*
       case "start_game":
         var s = msg.state;
         ourId = s.you_are;

         playerMoney = {};
         var playerRole = {};
         jQuery.each(s.players, function(ix,info) {
            playerMoney[info.player] = 6;
            playerRole[info.player] = info.role;
         });

         var npcRole = {};
         npcNum = s.npcs.length;
         jQuery.each(s.npcs, function(ix,info) {
           npcRole[info.npc] = info.role;
         }

         break;

       case "next_turn":
         var s = msg.state;
         ourId = s.you_are;
         activeRoles = s.roles;
         npcNum = s.npcs;
         currentPlayer = s.current_player;

         playerMoney = {};
         jQuery.each(s.players, function(ix,info) {
          playerMoney[info.player] = info.money;
         });

         break;

       case "game_over":
         break;




       case "choose_player":
         break;

       case "choose_role":
         break;

       case "choose_swap":
         break;

       case "choose_action":
         break;

       case "choose_response":
         break;

       case "are_you_ready":
         break;



       case "invalid_response":
         // hmm?
         break;
*/

       default:
         console.log('unknown message');
         console.log(msg);
     }
  };




}

