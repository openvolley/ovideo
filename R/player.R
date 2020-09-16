#' Inject javascript for an HTML video player
#'
#' @param youtube logical: set to \code{TRUE} to include the Youtube API javascript. This isn't necessary if you are only using local video files
#'
#' @return A \code{head} tag containing script tags
#'
#' @seealso \code{\link{ov_video_playlist}}
#'
#' @export
ov_video_js <- function(youtube = FALSE) {
    assert_that(is.flag(youtube), !is.na(youtube))
    js <- readLines(system.file("extdata/js/vid.js", package = "ovideo"))
    js <- paste(js, collapse = "\n")
    out <- list(tags$script(HTML(js)), if (youtube) tags$script(src = "https://www.youtube.com/iframe_api"),
                tags$script("Shiny.addCustomMessageHandler('evaljs', function(jsexpr) { eval(jsexpr) });")) ## handler for running js code directly
    tags$head(Filter(Negate(is.null), out))
}

#' Video player tag element
#'
#' @param id string: the id of the tag
#' @param type string: either "youtube" or "local"
#' @param controls logical: add "previous", "next", "pause", and "stop" buttons. If `controls` is an object of class `shiny.tag` (created by `htmltools::tags`) or `shiny.tag.list` (`htmltools::tagList`) then it will be appended to the controls
#' @param ... : other attributes of the player element (passed to the player `tags$div` call for youtube or `tags$video` for local)
#'
#' @return HTML tags. The outermost element is a div with id `paste0(id, "_container")`, with the player and optionally buttons nested within it.
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'
#'   ## hand-crafted playlist for this example
#'   playlist <- data.frame(video_src = "xL7qEpOdtio",
#'                          start_time = c(5417, 7252, 6222, 7656, 7369),
#'                          duration = 8,
#'                          type = "youtube")
#'
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE),
#'           ov_video_player(id = "yt_player", type = "youtube",
#'                           style = "height: 480px; background-color: black;",
#'                           controls = tags$button("Go",
#'                                        onclick = ov_playlist_as_onclick(playlist, "yt_player")))
#'       ),
#'       server = function(input, output) {},
#'   )
#' }
#'
#' @export
ov_video_player <- function(id, type, controls = FALSE, ...) {
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("youtube", "local"))
    if (inherits(controls, c("shiny.tag", "shiny.tag.list"))) {
        extra_controls <- controls
        controls <- TRUE
    } else {
        if (!is.flag(controls)) stop("controls should be either TRUE/FALSE or a tags/tagList object")
        extra_controls <- NULL
    }
    assert_that(is.flag(controls), !is.na(controls))
    if (type == "youtube") {
        plyr <- do.call(tags$div, c(list(id = id), list(...)))
    } else {
        plyr <- do.call(tags$video, c(list(id = id, autoplay = "false", preload = "metadata"), list(...)))
        ##if (controls) plyr <- htmltools::tagAppendAttributes(plyr, controls = "controls")
    }
    if (controls) {
        tags$div(id = paste0(id, "_container"), plyr, tags$div(tags$button("Prev", onclick = "dvjs_video_prev();"), tags$button("Next", onclick = "dvjs_video_next();"), tags$button("Pause", onclick = "dvjs_video_pause();"), tags$button("Stop", onclick = "dvjs_video_stop();"), extra_controls))
    } else {
        tags$div(id = paste0(id, "_container"), plyr)
    }
}


#' Functions for controlling the video player
#'
#' The video element and the controls provided by this function are javascript-based, and so are probably most useful in Shiny apps.
#'
#' @param what string: the command, currently one of:
#' \itemize{
#'   \item "play" (note that this requires that the playlist has already been loaded)
#'   \item "stop"
#'   \item "pause"
#'   \item "prev"
#'   \item "next"
#'   \item "jog" - move the video forward or backwards by a given number of seconds (pass this value as the `...` argument)
#'   \item "set_playback_rate" - set the playback rate: 1 = normal speed, 2 = double speed, etc
#' }
#' @param ... : parameters used by those commands
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
    what <- match.arg(tolower(what), c("play", "stop", "prev", "next", "pause", "set_playback_rate", "jog"))
    if (what == "play") {
        evaljs("dvjs_video_play();")
    } else if (what == "stop") {
        evaljs("dvjs_video_stop();")
    } else if (what == "prev") {
        evaljs("dvjs_video_prev();")
    } else if (what == "next") {
        evaljs("dvjs_video_next();")
    } else if (what == "pause") {
        evaljs("dvjs_video_pause();")
    } else if (what == "set_playback_rate") {
        if (length(myargs) < 1) stop("provide the playback rate as the second parameter to ov_video_control")
        evaljs(paste0("dvjs_set_playback_rate(", myargs[[1]], ");"))
    } else if (what == "jog") {
        if (length(myargs) < 1) stop("provide the number of seconds as the second parameter to ov_video_control")
        evaljs(paste0("dvjs_jog(", myargs[[1]], ");"))
    }
}

evaljs <- function(expr) {
    shiny::getDefaultReactiveDomain()$sendCustomMessage("evaljs", expr)
}
