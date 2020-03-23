#' Try and locate a video file, when the path embedded in the dvw file is for another computer
#'
#' @param dvw_filename string: the full path to the DataVolley file
#' @param video_filename character: one or more video file paths. If `NULL`, the video file name embedded in the DataVolley file will be used
#'
#' @return A character vector, with one entry per `video_filename`. Video files that could not be found will be `NA` here.
#'
#' @export
ov_find_video_file <- function(dvw_filename, video_filename = NULL) {
    assert_that(is.string(dvw_filename))
    if (is.null(video_filename)) {
        video_filename <- datavolley::dv_read(dvw_filename, metadata_only = TRUE)$meta$video
        if (nrow(video_filename) > 0) {
            return(ov_find_video_file(dvw_filename = dvw_filename, video_filename = video_filename$file))
        } else {
            video_filename <- NA_character_
        }
    }
    if (length(video_filename) > 1) {
        return(vapply(seq_len(nrow(video_filename)), function(z) ov_find_video_file(dvw_filename = dvw_filename, video_filename = video_filename$file[z]), FUN.VALUE = "", USE.NAMES = FALSE))
    }
    if (length(video_filename) == 1 && !is.na(video_filename) && nzchar(video_filename)) {
        if (fs::file_exists(video_filename)) return(video_filename) ## ok, the path in the dvw file is actually correct
        ## otherwise let's go looking for it
        this_dir <- dirname(dvw_filename) ## actual file has to be under the same path
        if (!fs::dir_exists(this_dir) && !fs::link_exists(this_dir)) return(NA_character_)
        possible_paths <- c(this_dir, fs::dir_ls(this_dir, type = "dir", recurse = TRUE))
        ff <- fs::path(possible_paths, basename(video_filename))
        ff <- ff[fs::file_exists(ff)]
        if (length(ff) ==1) ff else NA_character_
    } else {
        NA_character_
    }
}
