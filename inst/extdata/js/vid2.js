function dvjs_controller(id, type, seamless = true) {
    this.video_controller = {id: id, queue: [], current: -1, type: type, paused: false, seamless: seamless, loop: false, suspended: 0};
    // current is the pointer to the currently-being-played item in the queue
    // type is "local" or "youtube" or "twitch"
    // id is the id of the player HTML element
    this.video_timer = null;
    // we check the player at intervals to see if it's finished playing the current item
    this.video_timer_active = false;
    this.yt_player = null; // yt_player is either a youtube or twitch player
    this.yt_first_mute = false; // override this to true to start the YT player muted on first play
    var that = this; // to use inside functions

    var dofullscr = function(elem) {
	if (elem) {
	    try {
		if (elem.requestFullscreen) {
		    elem.requestFullscreen();
		} else if (elem.webkitRequestFullscreen) {
		    elem.webkitRequestFullscreen();
		} else if (elem.msRequestFullscreen) {
		    elem.msRequestFullscreen();
		}
		return true;
	    } catch (error) {
		return false;
	    }
	}
	return false;
    }

    this.fullscreen = function() {
	var done = dofullscr(document.getElementById("video_holder"));
	if (!done) { // fallback
	    dofullscr(document.getElementById(that.video_controller.id));
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

    this.set_playlist = function(items, video_id, type, seamless = true, loop = false) {
	// set the player's playlist, but don't start playing
	var old_id = that.video_controller.id; // HTML element with player attached, if any
	that.video_controller = {id: video_id, queue: items, current: 0, type: type, paused: false, seamless: seamless, loop: loop, suspended: 0};
	if (type == "youtube") {
	    if (that.yt_player != null && old_id != null && old_id != video_id) {
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
	} else if (type == "twitch") {
	    if (that.yt_player != null && old_id != null && old_id != video_id) {
		that.yt_player.destroy();
		that.yt_player = null;
	    }
	    if (that.yt_player == null) {
		that.yt_player = new Twitch.Player(video_id, {
		    width: "100%",
		    height: "100%",
		    controls: false,
		    video: items[0].video_src,
		    autoplay: false,
		    muted: that.yt_first_mute
		});
		that.yt_player.addEventListener(Twitch.Player.PAUSE, that.twitch_pause_handler);
		that.yt_player.addEventListener(Twitch.Player.ENDED, that.stop_video_interval);
		that.yt_player.addEventListener(Twitch.Player.PLAY, that.twitch_play_handler);
		that.yt_player.addEventListener(Twitch.Player.PLAYING, that.twitch_playing_handler);
	    }
	}
    }

    this.twitch_play_handler = function() {
	// play started via the native twitch control, which we can't disable
	//console.log("TWITCH PLAY");
	that.start_video_interval();
	that.video_controller.paused = false;
    }
    this.twitch_playing_handler = function() {
	//console.log("TWITCH PLAYING");
	that.start_video_interval();
	that.video_controller.paused = false;
    }
    this.twitch_pause_handler = function() {
	//console.log("TWITCH PAUSED");
	that.stop_video_interval();
	that.video_controller.paused = true;
    }

    this.set_playlist_and_play = function(items, video_id, type, seamless = true, loop = false) {
	// set the player's playlist, and start playing it
	that.stop_video_interval();
	var old_id = that.video_controller.id; // HTML element with player attached, if any
	that.video_controller = {id: video_id, queue: items, current: 0, type: type, paused: false, seamless: seamless, loop: false, suspended: 0};
	if (type == "youtube") {
	    if (that.yt_player != null && old_id != null && old_id != video_id) {
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
	} else if (type == "twitch") {
	    if (that.yt_player != null && old_id != null && old_id != video_id) {
		that.yt_player.destroy();
		that.yt_player = null;
	    }
	    if (that.yt_player == null) {
		//console.log("set_playlist_and_play ... new twitch player");
		that.yt_player = new Twitch.Player(video_id, {
		    width: "100%",
		    height: "100%",
		    controls: false,
		    video: items[0].video_src,
		    time: Math.floor(items[0].start_time/3600) + "h" + Math.floor(items[0].start_time/60) + "m" + (items[0].start_time - Math.floor(items[0].start_time/60) * 60) + "s",
		    autoplay: true,
		    muted: that.yt_first_mute
		});
		that.yt_player.addEventListener(Twitch.Player.PAUSE, that.twitch_pause_handler);
		that.yt_player.addEventListener(Twitch.Player.ENDED, that.stop_video_interval);
		that.yt_player.addEventListener(Twitch.Player.PLAY, that.twitch_play_handler);
		that.yt_player.addEventListener(Twitch.Player.PLAYING, that.twitch_playing_handler);
		// play when ready ^^^ not needed if autoplay is true?
		// that.yt_player.addEventListener(Twitch.Player.READY, that.video_play);
	    } else {
		//console.log("set_playlist_and_play ... twitch player");
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
	    if (that.video_controller.type == "youtube" || that.video_controller.type == "twitch") {
		that.yt_player.destroy();
            }
	    that.yt_player = null;
	}
	that.video_controller = {id: null, queue: [], current: -1, type: "local", paused: false, seamless: true, loop: false, suspended: 0};
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
	//console.log("video_play");
	if (that.video_controller.paused || (that.video_controller.suspended > 0)) {
	    //console.log("in video_play, but controller was paused or suspended, so calling video_pause");
	    return that.video_pause();
	}
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
		if (that.yt_player && that.yt_player.getPlaylist) {
		    var current_src = that.yt_player.getPlaylist()
		    if (current_src != null) {
			current_src = current_src[0];
		    }
		    if (current_src == item.video_src) {
			that.yt_player.seekTo(item.start_time, true);
			that.yt_player.playVideo();
		    } else {
			that.yt_player.loadPlaylist(item.video_src, 0, item.start_time);
		    }
		} else {
		    that.yt_player.loadPlaylist(item.video_src, 0, item.start_time);
		}
	    } else if (that.video_controller.type == "twitch") {
		if (that.yt_first_mute) {
		    if (that.yt_player) { that.yt_player.setMuted(true); }
		    that.yt_first_mute = false;
		}
		if (that.yt_player && that.yt_player.getVideo) {
		    var current_src = that.yt_player.getVideo()
		    if (current_src == item.video_src) {
			that.yt_player.seek(item.start_time);
		    } else {
			that.yt_player.setVideo(item.video_src, item.start_time);
		    }
		} else {
		    that.yt_player.setVideo(item.video_src, item.start_time);
		}
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
	    } else if (that.video_controller.type == "twitch") {
		that.yt_player.pause();
	    } else {
		if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).pause(); }
	    }
	    that.video_onstop();
	}
    }

    this.suspend = function() {
	if (that.video_controller.type != "twitch") {
	    // not supported for twitch, because the native controls can't be disabled and it doesn't have full functionality compared to the other two
	    // don't attempt to suspend if we aren't actually playing yet
	    if (that.video_controller.type == "youtube" && (!that.yt_player || typeof(that.yt_player.getPlayerState) != "function" || that.yt_player.getPlayerState() <= 0)) { return false; }
	    if (that.video_controller.type == "local" && that.video_controller.current < 0) { return false; }
	    if (that.video_controller.paused) {
		// suspend, but we won't allow auto restart on unsuspend, user will have to unpause
		that.video_controller.suspended = 2;
	    } else {
		// suspend with unpause on unsuspend
		that.video_controller.suspended = 1;
	    }
	    // pause
	    that.stop_video_interval();
	    that.video_controller.paused = true;
	    if (that.video_controller.type == "youtube") {
		that.yt_player.pauseVideo();
	    } else if (that.video_controller.type == "twitch") {
		that.yt_player.pause();
	    } else {
		if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).pause(); }
	    }
	    that.video_afterpause();
	    that.video_onsuspend();
	    return true;
	} else {
	    return false;
	}
    }

    this.unsuspend = function() {
	if (that.video_controller.suspended < 1) { return; }
	if (that.video_controller.type != "twitch") { // not supported for twitch
	    var restart_playing = that.video_controller.suspended < 2;
	    that.video_controller.suspended = 0;
	    if (restart_playing) {
		// restart via (un)pause
		that.video_pause();
	    }
	    that.video_onunsuspend();
	}
    }

    this.video_pause = function() {
	//console.log("video_pause");
	if (that.video_controller.type != null) {
	    if (!that.video_timer_active) {
		// paused or stopped
		if (that.video_controller.paused) {
		    // paused, restart
		    // check that we are still within the current item
		    var item = that.video_controller.queue[that.video_controller.current];
		    if (typeof item !== "undefined" && typeof item !== "undefined") {
			var current_time;
			var current_src;
			var this_end_time = item.start_time+item.duration;
			if (that.video_controller.seamless && typeof item.seamless_start_time !== "undefined" && typeof item.seamless_duration !== "undefined") {
			    this_end_time = item.seamless_start_time+item.seamless_duration;
			}
			if (that.video_controller.type == "youtube") {
			    current_time = that.yt_player.getCurrentTime();
			    current_src = that.yt_player.getPlaylist()[0];
			} else if (that.video_controller.type == "twitch") {
			    current_time = that.yt_player.getCurrentTime();
			    current_src = that.yt_player.getVideo();
			} else {
			    var el = document.getElementById(that.video_controller.id);
			    if (el) {
				current_time = el.currentTime;
				current_src = el.getAttribute("src");
			    }
			}
			if (that.video_controller.suspended < 1) {
			    that.video_controller.paused = false;
			    if (current_src != item.video_src) {
				// we are out of whack somehow
				//console.log("src mismatch, current is: " + current_src + ", item is: " + item.video_src);
				that.video_play(); // needs testing
			    } else if ((that.video_controller.type != "twitch") && ((current_time > this_end_time) || current_time < item.start_time)) {
				// current time seems a bit flaky with the twitch player
				// not on current item any more
				//console.log("current time is: " + current_time + ", this_end_time is: " + this_end_time);
				that.video_play();
			    } else {
				if (that.video_controller.type == "youtube") {
				    that.yt_player.playVideo();
				} else if (that.video_controller.type == "twitch") {
				    that.yt_player.play();
				} else {
				    if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).play(); }
				}
				that.start_video_interval();
			    }
			}
		    }
		} else {
		    // we were just stopped
		    //console.log("direct to video_play");
		    that.video_play();
		}
	    } else {
		// pause
		//console.log("pausing");
		that.stop_video_interval();
		that.video_controller.paused = true;
		if (that.video_controller.type == "youtube") {
		    that.yt_player.pauseVideo();
		} else if (that.video_controller.type == "twitch") {
		    that.yt_player.pause();
		} else {
		    if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).pause(); }
		}
	    }
	    if (that.video_controller.suspended < 1) {
		that.video_afterpause();
	    }
	}
    }

    // these functions do nothing by default but can be redefined by the user
    this.video_onstop = function() { }
    this.video_onfinished = function() { } // is run after the playlist completes, even if loop is true and it is going to start playing again
    this.video_afterpause = function() { } // is run after video paused OR resumed from pause. May be fired when pausing from already-paused state
    this.video_onsuspend = function() { }
    this.video_onunsuspend = function() { }

    this.video_next = function(seamless = false) {
	//console.log("video_next");
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
		var el = document.getElementById(that.video_controller.id);
		if (el) {
		    var current_time = el.currentTime;
		    var current_src = el.getAttribute("src");
		    if (current_src == item.video_src && current_time > item.start_time) {
			if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).currentTime = item.start_time; }
		    }
		}
		that.video_play(); // next item, or stop if it was the last
	    }
	} else {
	    // end of playlist, nothing to play
	    if (that.video_controller.loop) {
		that.video_onfinished();
		that.video_controller.current=0;
		that.video_play();
	    } else {
		that.video_stop();
		that.video_onfinished();
	    }
	}
    }

    this.video_prev = function() {
	that.video_controller.current = Math.max(that.video_controller.current-1, 0); // prev or first
	that.video_play();
    }

    this.set_playback_rate = function(rate) {
	if (that.video_controller.type == "youtube") {
            if (that.yt_player) { that.yt_player.setPlaybackRate(rate); }
	} else if (that.video_controller.type == "twitch") {
            // not supported on twitch if (that.yt_player) { that.yt_player.setPlaybackRate(rate); }
	} else {
            if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).playbackRate=rate; }
	}
    }

    this.jog = function(howmuch) {
	if (that.video_controller.type == "youtube") {
	    that.yt_player.seekTo(that.yt_player.getCurrentTime() + howmuch, true);
	} else if (that.video_controller.type == "twitch") {
	    that.yt_player.seek(that.yt_player.getCurrentTime() + howmuch);
	} else {
            if (document.getElementById(that.video_controller.id)) { document.getElementById(that.video_controller.id).currentTime = (el.currentTime + howmuch); }
	}
    }

    this.video_manage = function() {
	if (that.video_controller.queue.length > 0 && that.video_controller.current >= 0 && (that.video_controller.current <= (that.video_controller.queue.length - 1))) {
	    //console.log("video_manage")
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
	    } else if (that.video_controller.type == "twitch") {
		current_time = that.yt_player.getCurrentTime();
		current_src = that.yt_player.getVideo();
	    } else {
		var el = document.getElementById(that.video_controller.id);
		if (el) {
		    current_time = el.currentTime;
		    current_src = el.getAttribute("src");
		}
	    }
	    if (current_src != item.video_src) {
		// we are out of whack somehow
		//console.log("src mismatch (2), current is: " + current_src + ", item is: " + item.video_src);
		that.video_stop();
            } else if (current_time > this_end_time) {
		// should we transition seamlessly to the next clip? (no stop and seek)
		var this_seamless = that.video_controller.seamless;
		if (this_seamless && that.video_controller.current >= 0 && that.video_controller.current < (that.video_controller.queue.length - 1)) {
		    var item = that.video_controller.queue[that.video_controller.current];
		    var next_item = that.video_controller.queue[that.video_controller.current+1];
		    this_seamless = item.video_src == next_item.video_src && next_item.start_time <= (item.start_time + item.duration);
		}
		that.video_next(this_seamless);
            } else {
		// current item still playing, do nothing
            }
	} else {
	    // no items
	    if (that.video_controller.loop) {
		that.video_onfinished();
		that.video_controller.current=0;
		that.video_play();
	    } else {
		that.video_stop();
		that.video_onfinished();
	    }
	}
    }
}
