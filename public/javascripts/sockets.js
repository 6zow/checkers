var wsUri = "ws://echo.websocket.org/";
var output;


function init() { output = document.getElementById("output"); testWebSocket(); }

function testWebSocket() { websocket = new WebSocket(wsUri);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) }; } 
	function onOpen(evt) { writeToScreen("CONNECTED"); doSend("WebSocket rocks"); }
	function onClose(evt) { writeToScreen("DISCONNECTED"); } 
	function onMessage(evt) { writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>'); }
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
		$(this).attr("id",columns[colNumber]+vRow);
		if(index % 7 == 0){
			vRow++;
			colNumber = 0;
		}

		//console.log(index);
		if(index % 8 == 0) cols = cols == 0 ? 1 : 0;
		if(index % 2 == 0 && cols == 0){
			$(this).css("background-color","#333333");
		}
		if(index % 2 == 1 && cols == 1){
			$(this).css("background-color","#333333");
		}
	});

	$(".cell").bind("click",function(){
		console.log("clicked");
		doSend("click butteon");
	});

	init();


	//add pieces
	for(var i=0; i < 24; i++){
		if(i < 12){
			$("body").append("<div id='p"+i+"' class='piece red'></div>");
		}
		else{
			$("body").append("<div id='p"+i+"' class='piece blk'></div>");
		}
	}
	var topLeft = $("#checkers").offset();
	var t = 0, l = 0, row = 0,col=0;
	for(var i=0; i < 24; i++){
		if(i % 4 == 0 && i > 0){
			row = row == 2 ? 5 : row + 1;
			col = 0;
		}

		t = topLeft.top + (row*100) + (row*4);

		l = topLeft.left+(col*200)+(col*4);
		l = row % 2 == 0 ? l+100 : l;
		var obj = {top:t,left:l}
		$("#p"+i).css(obj);
		col++;
	}


	//add drag/drop
	$( ".piece" ).draggable({
		start:function(e,ui){
			console.log($(this).attr("id"));
			iid = setInterval(function(){
				doSend({id:$(this).attr("id"),offset:$(this).offset()});
			},250)
		},
		stop:function(e,ui){
			console.log("stopped");
			clearInterval(iid);
		}
	});
    $( ".piece" ).droppable({
      drop: function( event, ui ) {
        console.log("dropped!");
          
      }
    });

});
