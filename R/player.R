#' Inject javascript for an HTML video player
#'
#' @param youtube logical: set to `TRUE` to include the Youtube API javascript. This isn't necessary if you are only using local video files
#' @param twitch logical: set to `TRUE` to include the Twitch API javascript. Only with `version = 2`. Not necessary if you are only using local video files
#' @param version numeric: code version. Default = 1, experimental = 2
#'
#' @return A `head` tag containing script tags
#'
#' @seealso [ov_video_playlist()]
#'
#' @export
ov_video_js <- function(youtube = FALSE, twitch = FALSE, version = 1) {
    assert_that(is.flag(youtube), !is.na(youtube))
    assert_that(is.flag(twitch), !is.na(twitch))
    if (twitch && version < 2) warning("twitch support requires version > 1")
    assert_that(is.numeric(version), version >= 1, version <= 2)
    js <- readLines(system.file(if (version == 1) "extdata/js/vid.js" else "extdata/js/vid2.js", package = "ovideo"))
    js <- paste(js, collapse = "\n")
    out <- list(tags$script(HTML(js)), if (youtube) tags$script(src = "https://www.youtube.com/iframe_api"),
                if (twitch && version > 1) tags$script(src = "https://player.twitch.tv/js/embed/v1.js"),
                tags$script("if (typeof(Shiny) !== 'undefined') { Shiny.initializedPromise.then(function() { Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) }); }); }")) ## handler for running js code directly
    tags$head(Filter(Negate(is.null), out))
}

#' Video player tag element
#'
#' @param id string: the id of the tag
#' @param type string: either "youtube", "twitch" (only with `version = 2`), or "local"
#' @param controls logical: if `TRUE`, add "previous", "next", "pause", "stop", and "fullscreen" buttons. If `controls` is an object of class `shiny.tag` (created by [htmltools::tags()]) or `shiny.tag.list` ([htmltools::tagList()]) then those controls will added with this tag or tag list appended
#' @param version numeric: code version. Default = 1, sort-of-experimental = 2. Version 2 supports multiple players on a single page, as well as `type = "twitch"`
#' @param controller_var string: (for version 2 only) the js variable name to use for the controller object that controls this video player
#' @param with_js logical: if `TRUE`, also include the supporting javascript libraries. If `with_js = FALSE`, you must make a separate call to [ov_video_js()] (e.g. in your Shiny ui.R function)
#' @param ... : other attributes of the player element (passed to the player `tags$div` call for youtube/twitch or `tags$video` for local)
#'
#' @return HTML tags. The outermost element is a div with id `paste0(id, "_container")`, with the player and optionally buttons nested within it.
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'
#'   ## hand-crafted playlist for this example
#'   playlist <- data.frame(video_src = "NisDpPFPQwU",
#'                          start_time = c(589, 1036, 1163, 2731, 4594),
#'                          duration = 8,
#'                          type = "youtube")
#'
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE, version = 2),
#'           ov_video_player(id = "yt_player", type = "youtube",
#'                           version = 2, controller_var = "my_dv",
#'                           style = "height: 480px; background-color: black;",
#'                           controls = tags$button("Go",
#'                                      onclick = ov_playlist_as_onclick(playlist, "yt_player",
#'                                                                       controller_var = "my_dv")))
#'       ),
#'       server = function(input, output) {},
#'   )
#' }
#'
#' @export
ov_video_player <- function(id, type, controls = FALSE, version = 1, controller_var = paste0(id, "_controller"), with_js = FALSE, ...) {
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("youtube", "twitch", "local"))
    if (inherits(controls, c("shiny.tag", "shiny.tag.list"))) {
        extra_controls <- controls
        controls <- TRUE
    } else {
        if (!is.flag(controls)) stop("controls should be either TRUE/FALSE or a tags/tagList object")
        extra_controls <- NULL
    }
    assert_that(is.flag(controls), !is.na(controls))
    assert_that(is.numeric(version), version >= 1, version <= 2)
    assert_that(is.flag(with_js), !is.na(with_js))
    v2 <- version == 2
    if (v2) assert_that(is.string(controller_var), !is.na(controller_var))
    if (type %in% c("youtube", "twitch")) {
        plyr <- do.call(tags$div, c(list(id = id), list(...)))
    } else {
        plyr <- do.call(tags$video, c(list(id = id, autoplay = "false", preload = "metadata"), list(...)))
    }
    out <- if (controls) {
               cstr <- if (v2) paste0(controller_var, ".") else "dvjs_"
               list(tags$div(id = paste0(id, "_container"), plyr,
                             tags$div(tags$button(tags$span(icon("step-backward", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_prev();"), title = "Previous"),
                                      tags$button(tags$span(icon("step-forward", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_next();"), title = "Next"),
                                      tags$button(tags$span(icon("pause-circle", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_pause();"), title = "Pause"),
                                      tags$button(tags$span(icon("stop-circle", style = "vertical-align:middle;")), onclick = paste0(cstr, "video_stop();"), title = "Stop"),
                                      tags$button(tags$span(icon("expand", style = "vertical-align:middle;")), onclick = paste0(cstr, "fullscreen();"), title = "Full screen"),
                                      tags$button(tags$span(icon("volume-mute", style = "vertical-align:middle;")), onclick = paste0(cstr, "toggle_mute()"), title = "Toggle mute"), extra_controls)
                             ))
           } else {
               list(tags$div(id = paste0(id, "_container"), plyr))
           }
    if (v2) out <- c(out, list(tags$script(paste0(controller_var, " = new dvjs_controller('", id, "','", type, "',true);"))))
    if (isTRUE(with_js)) out <- c(out, list(ov_video_js(youtube = type == "youtube", twitch = type == "twitch", version = version)))
    do.call(tagList, out)
}


#' Functions for controlling the video player
#'
#' The video element and the controls provided by this function are javascript-based, and so are probably most useful in Shiny apps.
#'
#' @param what string: the command, currently one of:
#' * "play" (note that this requires that the playlist has already been loaded)
#' * "stop"
#' * "pause"
#' * "prev"
#' * "next"
#' * "jog" - move the video forward or backwards by a given number of seconds (pass this value as the `...` argument)
#' * "set_playback_rate" - set the playback rate: 1 = normal speed, 2 = double speed, etc
#' @param ... : parameters used by those commands. For version 2 of the video controller, `...` must include `controller_var = "my_controller_var"`
#'
#' @examples
#' \dontrun{
#'   ov_video_control("jog", -1) ## rewind 1s
#'   ov_video_control("jog", 10) ## jump forwards 10s
#'   ov_video_control("set_playback_rate", 0.5) ## play at half speed
#' }
#' @export
ov_video_control <- function(what, ...) {
    assert_that(is.string(what))
    myargs <- list(...)
    ## for v1, call dvjs_*; for v2 call [controller_var].*
    cstr <- if ("controller_var" %in% names(myargs)) paste0(myargs$controller_var, ".") else "dvjs_"
    what <- match.arg(tolower(what), c("play", "stop", "prev", "next", "pause", "set_playback_rate", "jog", "fullscreen"))
    if (what == "play") {
        evaljs(paste0(cstr, "video_play();"))
    } else if (what == "stop") {
        evaljs(paste0(cstr, "video_stop();"))
    } else if (what == "prev") {
        evaljs(paste0(cstr, "video_prev();"))
    } else if (what == "next") {
        evaljs(paste0(cstr, "video_next();"))
    } else if (what == "pause") {
        evaljs(paste0(cstr, "video_pause();"))
    } else if (what == "set_playback_rate") {
        if (length(myargs) < 1) stop("provide the playback rate as the second parameter to ov_video_control")
        evaljs(paste0(cstr, "set_playback_rate(", myargs[[1]], ");"))
    } else if (what == "jog") {
        if (length(myargs) < 1) stop("provide the number of seconds as the second parameter to ov_video_control")
        evaljs(paste0(cstr, "jog(", myargs[[1]], ");"))
    } else if (what == "fullscreen") {
        evaljs(paste0(cstr, "fullscreen();"))
    } else if (what == "mute") {
        evaljs(paste0(cstr, "mute();"))
    } else if (what == "unmute") {
        evaljs(paste0(cstr, "unmute();"))
    } else if (what == "toggle_mute") {
        evaljs(paste0(cstr, "toggle_mute();"))
    }
}

evaljs <- function(expr) {
    shiny::getDefaultReactiveDomain()$sendCustomMessage("evaljs", expr)
}
