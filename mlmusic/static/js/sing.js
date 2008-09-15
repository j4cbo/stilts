(function() {
	function escapeHTML(s) {
		return s.replace("&", "&amp;").replace("<", "&gt;");
	}

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

	function updateCurSong(song) {
		if (song.tracknum) {
			st = song.tracknum + ". " + song.title;
		} else {
			st = song.title;
		}

		$("#cstitle").html(
			"<a target=\"browseframe\" href=\"/browse/song/" + song.id
			+ "/\">" + escapeHTML(st) + "</a>"
		);

		var alb = "<a target=\"browseframe\" href=\"/browse/albums/"
			+ song.album_id + "/\">" + escapeHTML(song.album) + "</a>";

		if (song.year) {
			y = Number(song.year);
			alb += " (<a target=\"browseframe\" href=\"/browse/years/"
			+ y + "/\">" + y + "</a>)";
		}

		$("#csalbum").html(alb);

		$("#csartist").html(
			"<a target=\"browseframe\" href=\"/browse/artists/"
			+ song.artist_id + "/\">" + escapeHTML(song.artist) + "</a>"
		);
	}

	function updateStatus(postdata) {
		$.post(
			"/player/" + curPlayer() + "/status",
			postdata,	
			function(resp) {
				var j = eval("(" + resp + ")");
				parseStatusObject(j[0]);
				updateCurSong(j[1][0]);
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

	$("#playlistul").load("/player/" + curPlayer() + "/playlist");
}());
