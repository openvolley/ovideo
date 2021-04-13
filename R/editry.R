#' Convert playlist to editry clips
#'
#' Note that in order to use `ov_editry_clips`, the `editry` package must be installed, along with `editly` (which is the underlying node JS package: see [editry::er_install_editly()]). [editry::er_install_editly()] will also attempt to install the system binaries for `node` and `ffmpeg` if needed.
#'
#' @param playlist data.frame: a playlist as returned by `ov_video_playlist`. Note that only local video sources are supported
#' @param title string: the title text (first slide). Use `NULL` to skip this slide
#' @param title2 string: the second title text (on the second slide). Use `NULL` to skip this slide
#' @param label_col string: the name of the column in `playlist` to use for a label on each clip (created with [editry::er_layer_news_title()]). Common label choices are player names, scores, or descriptions of the play being shown in the clip
#' @param pause logical: if `TRUE`, insert an [editry::er_clip_pause()] clip at the end of the sequence
#' @param seamless logical: if `TRUE`, combine overlapping/adjacent clips. Note that if a `label_col` has been specified, the label from the first clip will be used for the whole of the combined clip (so it may no longer make sense, for example if the label is the player name)
#' @param title_args list: arguments to pass to [editry::er_clip_title_background()] when creating the title slide
#' @param title2_args list: arguments to pass to [editry::er_clip_title2()] when creating the title2 slide
#' @param pause_args list: arguments to pass to [editry::er_clip_pause()] when creating the final slide
#'
#' @return A list of [editry::er_clip()] objects, suitable to pass to [editry::er_spec()]
#'
#' @seealso [editry::er_layer_news_title()], [editry::er_layer()], [editry::er_spec()]
#'
#' @examples
#' \dontrun{
#'   ## Step 1: create our playlist
#'
#'   ## use data from the ovdata package
#'   library(ovdata) ## install via remotes::install_github("openvolley/ovdata") if needed
#'   x <- ovdata_example("190301_kats_beds-clip", as = "parsed")
#'
#'   ## make sure its video element points to our local copy of the corresponding video clip
#'   dv_meta_video(x) <- ovdata_example_video("190301_kats_beds")
#'
#'   ## extract the plays
#'   px <- datavolley::plays(x)
#'   ## use just the attack rows
#'   px <- px[which(px$skill == "Attack"), ]
#'
#'   ## make a new column with player name and attack type
#'   px$label <- paste(px$player_name, px$attack_code, "attack")
#'
#'   ## make the playlist with the new label column included
#'   tm <- ov_video_timing(Attack = c(-3, 2)) ## tighter than normal timing
#'   ply <- ov_video_playlist(px, x$meta, extra_cols = "label", timing = tm)
#'
#'   ## Step 2: convert to editly clip objects and compile to mp4
#'
#'   ## create the clips, one for each row of the playlist
#'   clips <- ov_editry_clips(ply, title = "GKS Katowice\nvs\nMKS Bedzin",
#'                                 title2 = "Attacks", label_col = "label")
#'
#'
#'   ## compile to video
#'   outfile <- tempfile(fileext = ".mp4")
#'   my_spec <- er_spec(out_path = outfile, clips = clips)
#'   er_exec_wait(spec = my_spec, fast = TRUE)
#'
#'   ## and view the output
#'   if (interactive()) browseURL(outfile)
#'
#' }
#'
#' @export
ov_editry_clips <- function(playlist, title = NULL, title2 = NULL, label_col, pause = TRUE, seamless = FALSE, title_args = list(), title2_args = list(), pause_args = list()) {
    if (missing(label_col) || !label_col %in% names(playlist)) label_col <- NULL

    if (isTRUE(seamless)) {
        ## combine adjacent/overlapping clips into one
        keep <- rep(FALSE, nrow(playlist)); keep[1] <- TRUE
        last <- 1L
        for (i in seq_len(nrow(playlist))[-1]) {
            if (playlist$start_time[last] + playlist$duration[last] >= playlist$start_time[i]) {
                playlist$duration[last] <- playlist$start_time[i] + playlist$duration[i] - playlist$start_time[last]
            } else {
                keep[i] <- TRUE
                last <- i
            }
        }
        playlist <- playlist[keep, ]
    }

    play_clips <- lapply(seq_len(nrow(playlist)), function(i) {
        lyrs <- list(editry::er_layer_video(path = playlist$video_src[i], cut_from = playlist$start_time[i], cut_to = playlist$start_time[i] + playlist$duration[i]))
        if (!is.null(label_col)) lyrs <- c(lyrs, list(editry::er_layer_news_title(playlist[[label_col]][i])))
        editry::er_clip(layers = lyrs)
    })

    title_clip <- list(NULL)
    if (!is.null(title)) {
        t_args <- list(duration = 3, text = title)
        for (nm in names(title_args)) t_args[[nm]] <- title_args[[nm]]
        title_clip <- list(do.call(editry::er_clip_title_background, t_args))
    }

    title2_clip <- list(NULL)
    if (!is.null(title2)) {
        t2_args <- list(duration = 2, text = title2)
        for (nm in names(title2_args)) t2_args[[nm]] <- title2_args[[nm]]
        title2_clip <- list(do.call(editry::er_clip_title2, t2_args))
    }

    pause_clip <- list(NULL)
    if (isTRUE(pause)) {
        p_args <- list(duration = 1.5)
        for (nm in names(pause_args)) p_args[[nm]] <- pause_args[[nm]]
        pause_clip <- list(do.call(editry::er_clip_pause, p_args))
    }

    Filter(Negate(is.null), c(title_clip, title2_clip, play_clips, pause_clip))
}

