(function() {

	var lastStatusObject = initialStatus;

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
		$("#voldownbutton").removeClass("disabled");
		$("#volupbutton").removeClass("disabled");

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

		var volumeBars = Math.floor(status["mixer volume"] / 10.0);
		$("#volumebar").css(
			"background-position",
			"0 -" + (volumeBars * 22) + "px"
		);
	}

	function updateCurSong(song) {
		if (!song) {
			$("#cstitle").html("");
			$("#csartist").html("");
			$("#csalbum").html("");
			return;
		}

		if (song.tracknum) {
			st = song.tracknum + ". " + song.title;
		} else {
			st = song.title;
		}

		$("#cstitle").html(
			"<a target=\"browseframe\" href=\"/browse/song/"
			+ song.id + "/\">" + escapeHTML(st) + "</a>"
		);

		if (song.album) {
			var alb = "<a target=\"browseframe\" href=\"/browse/albums/"
				+ song.album_id + "/\">" + escapeHTML(song.album) + "</a>";

			if (song.year) {
				y = Number(song.year);
				alb += " (<a target=\"browseframe\" href=\"/browse/years/"
				+ y + "/\">" + y + "</a>)";
			}

			$("#csalbum").html(alb);
		}

		if (song.artist) {
			$("#csartist").html(
				"<a target=\"browseframe\" href=\"/browse/artists/"
				+ song.artist_id + "/\">" + escapeHTML(song.artist) + "</a>"
			);
		}
	}

	function handleStatusObject(obj) {
		parseStatusObject(obj[0]);
		updateCurSong(obj[1][0]);

		if (obj[0].playlist_timestamp != lastStatusObject[0].playlist_timestamp) {
			$("#playlistul").load("/player/" + curPlayer() + "/playlist");
		}

		lastStatusObject = obj;
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
		"#nextbutton":		"next",
		"#volupbutton":		"volup",
		"#voldownbutton":	"voldown"
	}, function(k, v) {
		$(k).bind("click", function() {
			updateStatus(v);
			return false;
		});
	});

	function doCommand (cmd, selectors) {
		updateStatus(cmd + " " + selectors);
	}

	setInterval(function() {
		updateStatus("");
	}, 5000);

	handleStatusObject(initialStatus);

	/* $("#playlistul").load("/player/" + curPlayer() + "/playlist");
	*/

	top.Sing = {
		ctl: {
			add: function(s) { doCommand ("add", s); },
			play: function(s) { doCommand ("play", s); }
		},
		pl: {
			play: function(s) { doCommand ("pljump", s); },
			del: function(s) { doCommand ("pldel", s); }
		}
	};
}());
