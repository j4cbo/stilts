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
			"/player/status/" + curPlayer(),
			postdata,	
			function(resp) {
				var j = eval("(" + resp + ")");
				parseStatusObject(j[0]);
				var curSong = j[1][0];
			}
		);
	}

	$.each({
		"#repeatbutton":	"cmd=playlist_repeat&opt=",
		"#shufflebutton":	"cmd=playlist_shuffle&opt=",
		"#playbutton":		"cmd=pause",
		"#prevbutton":		"cmd=playlist_index&opt=-1",
		"#nextbutton":		"cmd=playlist_index&opt=%2b1",
	}, function(k, v) {
		$(k).bind("click", function() {
			updateStatus(v);
			return false;
		});
	});

	updateStatus("");
}());
