var wsUri = "ws://" + document.location.host + "/sockets/";
var output;

var moveMap;

function init() { output = document.getElementById("output"); testWebSocket(); }

function testWebSocket() { websocket = new WebSocket(wsUri + gameId);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) }; } 
	function onOpen(evt) { writeToScreen("CONNECTED"); doSend(JSON.stringify({action: "join"})); }
	function onClose(evt) { writeToScreen("DISCONNECTED"); } 
	function onMessage(evt) {


		writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');

		var obj = JSON.parse(evt.data);
        if (obj.msg != undefined) {
            var messages = $("#messages");
            messages.append("<p>" + obj.msg + "</p>");
            messages.scrollTop(messages.innerHeight());
        } else if (obj.movable != undefined) {
            moveMap = obj.movable;
            $(".piece").draggable({disabled: true});
            for (var i in obj.movable) {
                makeDraggable($("#" + i));
            }
        } else {
            if (obj.pos == undefined) {
                obj.pos = {x: -100, y: -100};
            }
            var offset = boardToScreen(obj.pos);
            var elt = $("#" + obj.id);
            if (elt.length == 0) {
                $("#checkers").append("<div id='" + obj.id + "' class='piece " + (obj.id.substr(1, 1) == "1" ? "red" : "blk") + "'><div class='marker'></div><div class='crown'></div></div>");
                elt = $("#" + obj.id);
            }
            elt.css("top", offset.top);
            elt.css("left", offset.left);
            if (obj.crowned != undefined) {
                if (obj.crowned) {
                    elt.addClass("crowned")
                } else {
                    elt.removeClass("crowned")
                }
            }
        }

	}
	function onError(evt) { writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data); }
	function doSend(message) { writeToScreen("SENT: " + message);  websocket.send(message); }
	function writeToScreen(message) { var pre = document.createElement("p"); pre.style.wordWrap = "break-word"; pre.innerHTML = message; output.appendChild(pre); output.scrollTop = output.scrollHeight }
	//window.addEventListener("load", init, false);

var colNumber = 0,vRow=0;
var columns = ["A","B","C","D","E","F","G","H","I"];

$(document).ready(function(){

    var cell = $(".cell");
    cell.each(function(index){

		//console.log(index);
		if(index % 8 == 0) {
            colNumber = 0;
            vRow++;
        }
        $(this).attr("id", columns[colNumber] + (9 - vRow));
		if((colNumber + vRow) % 2 == 0) {
            $(this).addClass("blk");
		}
        colNumber++;
	});

    $("#undo").bind("click", function () {
        doSend(JSON.stringify({action: "undo"}));
    });

	init();
});

function makeDraggable(piece) {
    //add drag/drop
    piece.draggable({
        start: function (e, ui) {
            moveMap[$(ui.helper).attr("id")].forEach(function(cell) { $("#" + cell).addClass("hl-cell") });
        },
        stop: function (e, ui) {
            $(".hl-cell").removeClass("hl-cell");
            doSend(JSON.stringify({action: "moved", id: $(ui.helper).attr("id"), pos: screenToBoard(ui.position)}));
        },
        drag: function (e, ui) {
            doSend(JSON.stringify({action: "moving", id: $(ui.helper).attr("id"), pos: screenToBoard(ui.position)}));
        },
        disabled: false,
        cursor: "move"
    });
}

function screenToBoard(offset) {
    var x = offset.left / 50 + 1;
    var y = 8 - offset.top / 50;
    return {x: x, y: y};
}

function boardToScreen(pos) {
    return {left: (pos.x - 1) * 50, top: (8 - pos.y) * 50};
}