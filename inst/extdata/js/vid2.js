function dvjs_controller(id, type, seamless = true) {
    this.video_controller = {id: id, queue: [], current: -1, type: type, paused: false, seamless: seamless};
    // current is the pointer to the currently-being-played item in the queue
    // type is "local" or "youtube"
    // id is the id of the player HTML element
    this.video_timer = null;
    // we check the player at intervals to see if it's finished playing the current item
    this.video_timer_active = false;
    this.yt_player = null;
    this.yt_first_mute = false; // override this to true to start the YT player muted on first play
    var that = this; // to use inside functions

    this.fullscreen = function() {
	var elem = document.getElementById(that.video_controller.id);
	if (elem) {
	    if (elem.requestFullscreen) {
		elem.requestFullscreen();
	    } else if (elem.webkitRequestFullscreen) {
		elem.webkitRequestFullscreen();
	    } else if (elem.msRequestFullscreen) {
		elem.msRequestFullscreen();
	    }
	}
    }

    this.start_video_interval = function() {
	if (!that.video_timer_active) {
	    that.video_timer = setInterval(that.video_manage, 200);
	    that.video_timer_active = true;
	    //console.dir(that.video_timer_active);
	}
    }
    this.stop_video_interval = function() {
	if (that.video_timer_active) {
	    clearInterval(that.video_timer);
	    that.video_timer_active = false;
	    //console.dir(that.video_timer_active);
	}
    }

    this.set_playlist = function(items, video_id, type, seamless = true) {
	// set the player's playlist, but don't start playing
	var old_id = that.video_controller.id; // HTML element with player attached, if any
	that.video_controller = {id: video_id, queue: items, current: 0, type: type, paused: false, seamless: seamless};
	if (type == "youtube") {
	    if (old_id != null && old_id != video_id) {
		that.yt_player.destroy();
		that.yt_player = null;
	    }
	    if (that.yt_player == null) {
		that.yt_player = new YT.Player(video_id, {
		    videoId: items[0].video_src,
		    playerVars: {
			"controls": 0 //"rel": 0
		    },
		    events: {
			"onStateChange": that.yt_player_state_change
		    }
		});
	    }
	}
    }

    this.set_playlist_and_play = function(items, video_id, type, seamless = true) {
	// set the player's playlist, and start playing it
	that.stop_video_interval();
	var old_id = that.video_controller.id; // HTML element with player attached, if any
	that.video_controller = {id: video_id, queue: items, current: 0, type: type, paused: false, seamless: seamless};
	if (type == "youtube") {
	    if (old_id != null && old_id != video_id) {
		that.yt_player.destroy();
		that.yt_player = null;
	    }
	    if (that.yt_player == null) {
		//console.log("set_playlist_and_play ... new YT player");
		that.yt_player = new YT.Player(video_id, {
		    videoId: items[0].video_src,
		    playerVars: {
			"controls": 0 //"rel": 0
		    },
		    events: {
			"onReady": that.video_play,
			"onStateChange": that.yt_player_state_change
		    }
		});
	    } else {
		//console.log("set_playlist_and_play ... YT play");
		that.video_play();
	    }
	} else {
	    // local media
	    //console.log("set_playlist_and_play ... play");
	    that.video_play();
	}
    }

    this.clear_playlist = function() {
	that.video_stop();
	if (that.video_controller.type == "local") {
	    if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).removeAttribute("src"); }
	}
	if (that.yt_player != null) {
	    that.yt_player.destroy();
	    that.yt_player = null;
	}
	that.video_controller = {id: null, queue: [], current: -1, type: "local", paused: false, seamless: true};
    }

    // this function does nothing by default but can be redefined by the user
    this.video_onstart = function() { }

    this.yt_player_state_change = function(event) {
	if (event.data == YT.PlayerState.PAUSED || event.data == YT.PlayerState.ENDED) {
	    that.stop_video_interval();
	} else if (event.data == YT.PlayerState.PLAYING) {
	    that.start_video_interval();
	}
    }

    this.video_play = function() {
	//console.log("dvjs_video_play");
	that.video_onstart();
	if (that.video_controller.current >= 0 && that.video_controller.current <= (that.video_controller.queue.length - 1)) {
	    var item = that.video_controller.queue[that.video_controller.current];
	    if (item.playback_rate && item.playback_rate > 0) {
		that.set_playback_rate(item.playback_rate);
	    }
	    if (that.video_controller.type == "youtube") {
		if (that.yt_first_mute) {
		    if (that.yt_player) { that.yt_player.mute(); }
		    that.yt_first_mute = false;
		}
		that.yt_player.loadPlaylist(item.video_src, 0, item.start_time);
	    } else {
		el = document.getElementById(that.video_controller.id);
		if (el) {
		    if (el.getAttribute("src") != item.video_src) {
			el.setAttribute("src", item.video_src)
			el.currentTime = item.start_time;
			el.play();
		    } else {
			el.currentTime = item.start_time;
			el.play();
		    }
		}
	    }
	    that.start_video_interval();
	} else {
	    that.video_stop();
	}
    }

    this.video_stop = function() {
	that.stop_video_interval();
	if (that.video_controller.type != null) {
	    if (that.video_controller.type == "youtube") {
		that.yt_player.stopVideo();
	    } else {
		if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).pause(); }
	    }
	    that.video_onstop();
	}
    }

    this.video_pause = function() {
	if (that.video_controller.type != null) {
	    if (!that.video_timer_active) {
		// paused or stopped
		if (that.video_controller.paused) {
		    // restart
		    // check that we are still within the current item
		    var item = that.video_controller.queue[that.video_controller.current];
		    var current_time;
		    var current_src;
		    var this_end_time = item.start_time+item.duration;
		    if (that.video_controller.seamless && typeof item.seamless_start_time !== "undefined" && typeof item.seamless_duration !== "undefined") {
			this_end_time = item.seamless_start_time+item.seamless_duration;
		    }
		    if (that.video_controller.type == "youtube") {
			current_time = that.yt_player.getCurrentTime();
			current_src = that.yt_player.getPlaylist()[0];
		    } else {
			var el = document.getElementById(that.video_controller.id);
			if (el) {
			    current_time = el.currentTime;
			    current_src = el.getAttribute("src");
			}
		    }
		    if (current_src != item.video_src) {
			// we are out of whack somehow
			console.log("src mismatch");
			that.video_stop();
		    } else if (current_time > this_end_time) {
			// not on current item any more
			that.video_play();
		    } else {
			if (that.video_controller.type == "youtube") {
			    that.yt_player.playVideo();
			} else {
			    if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).play(); }
			}
			that.video_controller.paused = false;
			that.start_video_interval();
		    }
		} else {
		    // we were just stopped
		    that.video_play();
		}
	    } else {
		// pause
		that.stop_video_interval();
		that.video_controller.paused = true;
		if (that.video_controller.type == "youtube") {
		    that.yt_player.pauseVideo();
		} else {
		    if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).pause(); }
		}
	    }
	    that.video_afterpause();
	}
    }

    // these functions do nothing by default but can be redefined by the user
    this.video_onstop = function() { }
    this.video_onfinished = function() { }
    this.video_afterpause = function() { }

    this.video_next = function(seamless = false) {
	//console.log("dvjs_video_next");
	// seamless should be true if we want to transition seamlessly to the next clip (i.e. no stop and seek)
	if (that.video_controller.current < (that.video_controller.queue.length - 1)) {
	    that.video_controller.current++;

	    var item = that.video_controller.queue[that.video_controller.current];
	    if (item.playback_rate && item.playback_rate > 0) {
		that.set_playback_rate(item.playback_rate);
	    }
	    if (seamless) {
		that.video_onstart();
		that.start_video_interval();
	    } else {
		// if not seamless, and item is repeated, then need to seek to the clip starting time to make playback work. Otherwise the current time is past the end time of this next clip, and it immediately skips to the next
		if (that.video_controller.type == "youtube") {
		    var current_time = that.yt_player.getCurrentTime();
		    var current_src = that.yt_player.getPlaylist()[0];
		    if (current_src == item.video_src && current_time > item.start_time) {
			that.yt_player.seekTo(item.start_time, true);
		    }
		} else {
		    var el = document.getElementById(that.video_controller.id);
		    if (el) {
			var current_time = el.currentTime;
			var current_src = el.getAttribute("src");
			if (current_src == item.video_src && current_time > item.start_time) {
			    if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).currentTime = item.start_time; }
			}
		    }
		}
		that.video_play(); // next item, or stop if it was the last
	    }
	} else {
	    // end of playlist, nothing to play
	    that.video_stop();
	    that.video_onfinished();
	}
    }

    this.video_prev = function() {
	that.video_controller.current = Math.max(that.video_controller.current-1, 0); // prev or first
	that.video_play();
    }

    this.set_playback_rate = function(rate) {
	if (that.video_controller.type == "youtube") {
            if (that.yt_player) { that.yt_player.setPlaybackRate(rate); }
	} else {
            if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).playbackRate=rate; }
	}
    }

    this.jog = function(howmuch) {
	if (that.video_controller.type == "youtube") {
	    that.yt_player.seekTo(that.yt_player.getCurrentTime() + howmuch, true);
	} else {
            if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).currentTime = (el.currentTime + howmuch); }
	}
    }

    this.video_manage = function() {
	if (that.video_controller.queue.length > 0 && that.video_controller.current >= 0 && (that.video_controller.current <= (that.video_controller.queue.length - 1))) {
	    var item = that.video_controller.queue[that.video_controller.current];
	    var current_time;
	    var current_src;
	    var this_end_time = item.start_time+item.duration;
	    if (that.video_controller.seamless && typeof item.seamless_start_time !== "undefined" && typeof item.seamless_duration !== "undefined") {
		this_end_time = item.seamless_start_time+item.seamless_duration;
	    }
	    if (that.video_controller.type == "youtube") {
		current_time = that.yt_player.getCurrentTime();
		current_src = that.yt_player.getPlaylist()[0];
		if (current_time < 0.5 && item.start_time >= 0.5) {
		    // YT sometimes resets the video to the start of the video AFTER seeking to the startSeconds time. Whaaat?
		    that.yt_player.seekTo(item.start_time, true);
		}
	    } else {
		var el = document.getElementById(that.video_controller.id);
		if (el) {
		    current_time = el.currentTime;
		    current_src = el.getAttribute("src");
		}
	    }
	    if (current_src != item.video_src) {
		// we are out of whack somehow
		console.log("src mismatch");
		that.video_stop();
            } else if (current_time > this_end_time) {
		// should we transition seamlessly to the next clip? (no stop and seek)
		var this_seamless = that.video_controller.seamless;
		if (this_seamless && that.video_controller.current >= 0 && that.video_controller.current < (that.video_controller.queue.length - 1)) {
		    var item = that.video_controller.queue[that.video_controller.current];
		    var next_item = that.video_controller.queue[that.video_controller.current+1];
		    this_seamless = item.video_src == next_item.video_src && next_item.start_time <= (item.start_time + item.duration)
		}
		that.video_next(this_seamless)
            } else {
		// current item still playing, do nothing
            }
	} else {
	    // no items
            that.video_stop();
	    that.video_onfinished();
	}
    }
}
