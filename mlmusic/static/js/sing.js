(function() {

	var lastStatusObject = initialStatus;

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
		$("#cstitle").html(song[0]);
		$("#csalbum").html(song[1]);
		$("#csartist").html(song[2]);
	}

	function handleStatusObject(obj) {
		parseStatusObject(obj[0]);
		updateCurSong(obj[2]);

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

	$("#player").change(function() {
		document.cookie = "SqueezeCenter-player=" + escape(curPlayer());
		updateStatus("");
	});

	handleStatusObject(initialStatus);
	$("#playlistul").sortable({
		axis: "y",
		containment: "parent",
		handle: ".handle",
		cursor: "move",
		forcePlaceholderSize: true,
		opacity: 0.8,
		start: function(e, ui) { ui.helper.addClass("dragging"); },
		revert: 100,
		update: function(e, ui) {
			var item = $(ui.item);
			var newidx = item.parent().children().index(ui.item);
			var oldidx = Number(item.attr("id").substring(1));
			var offset = newidx - oldidx;
			if (offset >= 0) offset = "+" + offset;
			updateStatus("plmove " + oldidx + " " + offset);
		}
	});
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
