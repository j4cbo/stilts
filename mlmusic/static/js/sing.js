(function() {

	function curPlayer() {
		return $("#player").val();
	}

	function parseStatusObject(status) {
		$("#prevbutton").removeClass("disabled");
		$("#nextbutton").removeClass("disabled");
		$("#playbutton").removeClass("disabled");

		/* Play/pause */
		if (status.mode == "play") {
			$("#playbutton").addClass("pause");
		} else {
			$("#playbutton").removeClass("pause");
		}

		/* Repeat and shuffle */
		$("#repeatbutton").removeClass();
		$("#repeatbutton").addClass("r" + status["playlist repeat"]);
		$("#shufflebutton").removeClass();
		$("#shufflebutton").addClass("s" + status["playlist shuffle"]);
	}

	function updateStatus(postdata) {
		$.post(
			"/player/" + curPlayer() + "/status",
			postdata,	
			function(resp) {
				var j = eval("(" + resp + ")");
				parseStatusObject(j[0]);
				curSong = j[1][0];
			}
		);
	}

	$.each({
		"#repeatbutton":	"repeat",
		"#shufflebutton":	"shuffle",
		"#playbutton":		"pause",
		"#prevbutton":		"prev",
		"#nextbutton":		"next"
	}, function(k, v) {
		$(k).bind("click", function() {
			updateStatus(v);
			return false;
		});
	});

	updateStatus("");
}());
