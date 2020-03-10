var dvjs_video_controller = {id: null, queue: [], current: -1, type: "local", paused: false};
// current is the pointer to the currently-being-played item in the queue
// type is "local" or "youtube"
// id is the id of the player HTML element
var dvjs_video_timer = null;
// we check the player at intervals to see if it's finished playing the current item
var dvjs_video_timer_active = false;
var dvjs_yt_player = null;

function dvjs_start_video_interval() {
    if (!dvjs_video_timer_active) {
	dvjs_video_timer = setInterval(dvjs_video_manage, 500);
	dvjs_video_timer_active = true;
	//console.dir(dvjs_video_timer_active);
    }
}
function dvjs_stop_video_interval() {
    if (dvjs_video_timer_active) {
	clearInterval(dvjs_video_timer);
	dvjs_video_timer_active = false;
	//console.dir(dvjs_video_timer_active);
    }
}

function dvjs_set_playlist(items, video_id, type) {
    // set the player's playlist, but don't start playing
    var old_id = dvjs_video_controller.id; // HTML element with player attached, if any
    dvjs_video_controller = {id: video_id, queue: items, current: 0, type: type, paused: false};
    if (type == "youtube") {
	if (old_id != null && old_id != video_id) {
	    dvjs_yt_player.destroy();
	    dvjs_yt_player = null;
	}
	if (dvjs_yt_player == null) {
	    dvjs_yt_player = new YT.Player(video_id, {
		//height: "290",
		//width: "1200",
		videoId: items[0].video_src,
		events: {
		    "onStateChange": dvjs_yt_player_state_change
		}
	    });
	}
    }
}

function dvjs_set_playlist_and_play(items, video_id, type) {
    // set the player's playlist, and start playing it
    dvjs_stop_video_interval();
    var old_id = dvjs_video_controller.id; // HTML element with player attached, if any
    dvjs_video_controller = {id: video_id, queue: items, current: 0, type: type, paused: false};
    if (type == "youtube") {
	if (old_id != null && old_id != video_id) {
	    dvjs_yt_player.destroy();
	    dvjs_yt_player = null;
	}
	if (dvjs_yt_player == null) {
	    dvjs_yt_player = new YT.Player(video_id, {
		//height: "290",
		//width: "1200",
		videoId: items[0].video_src,
		events: {
                    "onReady": dvjs_video_play,
		    "onStateChange": dvjs_yt_player_state_change
		}
	    });
	} else {
	    dvjs_video_play();
	}
    } else {
	// local media
	dvjs_video_play();
    }
}


//function dvjs_new_player(items, video_id, type) {
//    if (type == "youtube") {
//	    dvjs_yt_player = new YT.Player(video_id, {
//		//height: "290",
//		//width: "1200",
//		videoId: items[0].video_src,
//		events: {
//		    "onStateChange": dvjs_yt_player_state_change
//		}
//	    });
//    } else {
//	// local media
//	// TODO
//    }
//}

// this function does nothing by default but can be redefined by the user
function dvjs_video_onstart() { }

//player.setSize(width:Number, height:Number):Object

function dvjs_yt_player_state_change(event) {
    if (event.data == YT.PlayerState.PAUSED || event.data == YT.PlayerState.ENDED) {
	dvjs_stop_video_interval();
    } else if (event.data == YT.PlayerState.PLAYING) {
	dvjs_start_video_interval();
    }
}

function dvjs_video_play() {
    dvjs_video_onstart();
    //console.dir(dvjs_video_controller);
    if (dvjs_video_controller.current >= 0 && dvjs_video_controller.current <= (dvjs_video_controller.queue.length - 1)) {
	var item = dvjs_video_controller.queue[dvjs_video_controller.current];
	if (dvjs_video_controller.type == "youtube") {
	    if (dvjs_yt_player.getPlaylist() != null && dvjs_yt_player.getPlaylist()[0] == item.video_src) {
		// same video, so just seek to right spot
		dvjs_yt_player.seekTo(item.start_time);
	    } else {
		dvjs_yt_player.loadPlaylist(item.video_src, 0, item.start_time);
	    }
	} else {
	    el = document.getElementById(dvjs_video_controller.id);
	    if (el.getAttribute("src") != item.video_src) {
		el.setAttribute("src", item.video_src)
	        // TO CHECK: should this wait until the file metadata is available, so that we can seek to currentTime?
	        //el.addEventListener("loadedmetadata", function() {
		//  // Video is loaded and can be played
		//  this.currentTime = dvjs_video_controller.queue[dvjs_video_controller.current].start_time;
		//  this.play();
		//}, false);
		el.currentTime = item.start_time;
		el.play();
	    } else {
		el.currentTime = item.start_time;
		el.play();
	    }
	}
	dvjs_start_video_interval();
    } else {
	dvjs_video_stop();
    }
}

function onYouTubeIframeAPIReady() {
    // don't need to do anything
}

function dvjs_video_stop() {
    dvjs_stop_video_interval();
    if (dvjs_video_controller.type != null) {
	if (dvjs_video_controller.type == "youtube") {
	    dvjs_yt_player.stopVideo();
	} else {
	    document.getElementById(dvjs_video_controller.id).pause();
	}
	dvjs_video_onstop();
    }
    dvjs_video_controller = {id:null, queue: [], current: -1, type: "", paused: false};
}

function dvjs_video_pause() {
    if (dvjs_video_controller.type != null) {
	if (dvjs_video_controller.paused) {
	    // restart
	    if (dvjs_video_controller.type == "youtube") {
		dvjs_yt_player.playVideo();
	    } else {
		document.getElementById(dvjs_video_controller.id).play();
	    }
	    dvjs_video_controller.paused = false;
	    dvjs_start_video_interval();
	} else {
	    // pause
	    dvjs_stop_video_interval();
	    dvjs_video_controller.paused = true;
	    if (dvjs_video_controller.type == "youtube") {
		dvjs_yt_player.pauseVideo();
	    } else {
		document.getElementById(dvjs_video_controller.id).pause();
	    }
	}
    }
}

// this function does nothing by default but can be redefined by the user
function dvjs_video_onstop() { }

function dvjs_video_next() {
    dvjs_video_controller.current++;
    dvjs_video_play(); // next item, or stop if it was the last
}

function dvjs_video_prev() {
    dvjs_video_controller.current = Math.max(dvjs_video_controller.current-1, 0); // prev or first
    dvjs_video_play();
}

function dvjs_set_playback_rate(rate) {
    if (dvjs_video_controller.type == "youtube") {
        dvjs_yt_player.setPlaybackRate(rate)
    } else {
        var el = document.getElementById(dvjs_video_controller.id);
        el.playbackRate=rate;
    }
}

function dvjs_jog(howmuch) {
    if (dvjs_video_controller.type == "youtube") {
      dvjs_yt_player.seekTo(dvjs_yt_player.getCurrentTime() + howmuch);
    } else {
        var el = document.getElementById(dvjs_video_controller.id);
        el.currentTime = (el.currentTime + howmuch);
    }
}

function dvjs_video_manage() {
    if (dvjs_video_controller.queue.length > 0 && dvjs_video_controller.current >= 0 && (dvjs_video_controller.current <= (dvjs_video_controller.queue.length - 1))) {
	var item = dvjs_video_controller.queue[dvjs_video_controller.current];//0];
	var current_time;
	var current_src;
	if (dvjs_video_controller.type == "youtube") {
	    current_time = dvjs_yt_player.getCurrentTime();
	    current_src = dvjs_yt_player.getPlaylist()[0];
	} else {
	    var el = document.getElementById(dvjs_video_controller.id);
	    current_time = el.currentTime;
	    current_src = el.getAttribute("src");
	}
	if (current_src != item.video_src) {
	    // we are out of whack somehow
	    console.log("src mismatch");
	    dvjs_video_stop();
        } else if (current_time > (item.start_time+item.duration)) {
	    //console.log("finished");
	    dvjs_video_next();
        } else {
	    // current item still playing, do nothing
        }
    } else {
	// no items
	//console.log("nothing to play");
        dvjs_video_stop();
    }
}
