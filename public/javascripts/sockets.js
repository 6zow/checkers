var wsUri = "ws://" + document.location.host + "/sockets";
var output;


function init() { output = document.getElementById("output"); testWebSocket(); }

function testWebSocket() { websocket = new WebSocket(wsUri);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) }; } 
	function onOpen(evt) { writeToScreen("CONNECTED"); doSend(JSON.stringify({action: "join"})); }
	function onClose(evt) { writeToScreen("DISCONNECTED"); } 
	function onMessage(evt) {


		writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');

		var obj = JSON.parse(evt.data);
        var offset = boardToScreen(obj.pos);
		$("#"+obj.id).css("top", offset.top);
		$("#"+obj.id).css("left", offset.left);


	}
	function onError(evt) { writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data); }
	function doSend(message) { writeToScreen("SENT: " + message);  websocket.send(message); }
	function writeToScreen(message) { var pre = document.createElement("p"); pre.style.wordWrap = "break-word"; pre.innerHTML = message; output.appendChild(pre); }
	//window.addEventListener("load", init, false);

var colNumber = 0,vRow=0, cols;
var columns = ["A","B","C","D","E","F","G","H","I"];
var bgr = "";
var iid;

$(document).ready(function(){

	//$("#checkers").html("TEST");
	$(".cell").each(function(index){

		//console.log(index);
		if(index % 8 == 0) {
            colNumber = 0;
            vRow++;
        }
        $(this).attr("id", columns[colNumber] + (9 - vRow));
		if((colNumber + vRow) % 2 == 1) {
			$(this).css("background-color","#333333");
		}
        colNumber++;
	});

	$(".cell").bind("click",function(){
		console.log("clicked");
		doSend("click butteon");
	});

	init();


	//add pieces
	for(var i=0; i < 24; i++){
		if(i < 12){
			$("#checkers").append("<div id='p"+i+"' class='piece red'></div>");
		}
		else{
			$("#checkers").append("<div id='p"+i+"' class='piece blk'></div>");
		}
	}
	var t = 0, l = 0, row = 0,col=0;
	for(var i=0; i < 24; i++){
		if(i % 4 == 0 && i > 0){
			row = row == 2 ? 5 : row + 1;
			col = 0;
		}

		t = row * 104;

		l = col * 208;
		l = row % 2 == 0 ? l + 104 : l;
		var obj = {top: t, left: l};
		$("#p"+i).css(obj);
		col++;
	}


	//add drag/drop
	$( ".piece" ).draggable({
//		start:function(e,ui){
//			console.log($(this).attr("id"));
//			var _this =this;
//			iid = setInterval(function(){
//              doSend(JSON.stringify({id:$(ui.helper).attr("id"),offset:ui.position}));
//			},250)
//		},
		stop:function(e,ui){
//			console.log("stopped");
//			clearInterval(iid);
            doSend(JSON.stringify({action: "moved", id: $(ui.helper).attr("id"), pos: screenToBoard(ui.position)}));
		},
        drag:function(e,ui){
            doSend(JSON.stringify({action: "moving", id: $(ui.helper).attr("id"), pos: screenToBoard(ui.position)}));
        }
	});
//    $( ".piece" ).droppable({
//      drop: function( event, ui ) {
//        console.log("dropped!");

//      }
//    });

});

function screenToBoard(offset) {
    var x = offset.left / 104 + 1;
    var y = 8 - offset.top / 104;
    return {x: x, y: y};
}

function boardToScreen(pos) {
    return {left: (pos.x - 1) * 104, top: (8 - pos.y) * 104};
}