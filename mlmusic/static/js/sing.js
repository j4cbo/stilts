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

	function handleStatusObject(obj) {
		parseStatusObject(obj[0]);
		updateCurSong(obj[1][0]);
	}

	function updateStatus(postdata) {
		$.post(
			"/player/" + curPlayer() + "/status",
			postdata,	
			function(resp) {
				handleStatusObject(eval("(" + resp + ")"));
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

	function doCommand (cmd, selectors) {
		updateStatus(cmd + " " + selectors);
	}

	handleStatusObject(initialStatus);

	/* $("#playlistul").load("/player/" + curPlayer() + "/playlist");
	*/

	top.Sing = {
		ctl: {
			add: function(s) { doCommand ("add", s); },
			play: function(s) { doCommand ("play", s); }
		}
	};
}());
