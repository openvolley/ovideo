#' Inject javascript for an HTML video player
#'
#' @param youtube logical: set to \code{TRUE} to include the Youtube API javascript. This isn't necessary if you are only using local video files
#'
#' @return A tag list
#'
#' @seealso \code{\link{ov_video_playlist}}
#'
#' @export
ov_video_js <- function(youtube = FALSE) {
    assert_that(is.flag(youtube), !is.na(youtube))
    js <- readLines(system.file("extdata/js/vid.js", package = "dvideo"))
    js <- paste(js, collapse = "\n")
    out <- list(tags$script(HTML(js)), if (youtube) tags$script(src = "https://www.youtube.com/iframe_api"))
    tagList(Filter(Negate(is.null), out))
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
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE),
#'           ov_video_player(id = "yt_player", type = "youtube", controls = TRUE,
#'                           style = "height: 480px; background-color: black;"),
#'           tags$button("Go", onclick = ov_playlist_as_onclick(playlist, "yt_player"))
#'       ),
#'       server = function(input, output) {},
#'   )
#'
#'   shinyApp(
#'       ui = fluidPage(
#'           ov_video_js(youtube = TRUE),
#'           ov_video_player(id = "yt_player", type = "youtube",
#'                           style = "height: 480px; background-color: black;",
#'                           controls = tags$button("Go",
#'                                        onclick = ov_playlist_as_onclick(playlist, "yt_player")))
#'       )),
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
