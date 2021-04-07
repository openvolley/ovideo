#' Install ffmpeg
#'
#' This is a helper function to install ffmpeg. Currently it only works on Windows platforms. The ffmpeg bundle will be downloaded from <https://www.gyan.dev/ffmpeg/builds/> and saved to your user appdata directory.
#'
#' @references <https://www.gyan.dev/ffmpeg/builds/>
#' @param force logical: force reinstallation if ffmpeg already exists
#' @param check_hash logical: don't check the hash of the downloaded file
#'
#' @return the path to the installed executable
#'
#' @examples
#' \dontrun{
#'   ov_install_ffmpeg()
#' }
#'
#' @export
ov_install_ffmpeg <- function(force = FALSE, check_hash = TRUE) {
    assert_that(is.flag(force), !is.na(force))
    assert_that(is.flag(check_hash), !is.na(check_hash))
    my_os <- get_os()
    if (my_os != "windows" || !isTRUE(.Machine$sizeof.pointer == 8)) {
        stop("ov_install_ffmpeg only supports 64-bit windows platforms. You will need to install ffmpeg yourself and ensure that it is on the system path.")
    }
    existing_exe <- ov_ffmpeg_exe()
    path <- file.path(ovideo_app_dir(), "ffmpeg")
    if (!force) {
        if (!is.null(existing_exe)) {
            message("ffmpeg already exists and force is FALSE, not reinstalling")
            return(existing_exe)
        }
    } else {
        if (dir.exists(path)) {
            ## remove existing
            unlink(path, recursive = TRUE)
        }
    }
    ## check that archive is available
    if (!requireNamespace("archive", quietly = TRUE)) {
        msg <- "the 'archive' package is required: install it with\n"
        if (!requireNamespace("remotes", quietly = TRUE)) msg <- paste0(msg, "  install.packages(\"remotes\")\n")
        stop(paste0(msg, "  remotes::install_github(\"jimhester/archive\")"))
    }
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    if (!dir.exists(path)) stop("could not create directory ", path, " for ffmpeg")
    zipname <- file.path(path, "ffmpeg-git-essentials.7z")
    dl_url <- "https://www.gyan.dev/ffmpeg/builds/ffmpeg-git-essentials.7z"
    hash_url <- "https://www.gyan.dev/ffmpeg/builds/sha256-git-essentials"
    err <- utils::download.file(dl_url, destfile = zipname, mode = "wb")
    if (!err) {
        if (check_hash) {
            expected_hash <- tryCatch(readLines(hash_url, warn = FALSE), error = function(e) NULL)
            if (is.null(expected_hash)) {
                warning("could not download the file hash for checking, but proceeding anyway")
            } else {
                if (!identical(expected_hash, digest::digest(zipname, algo = "sha256", file = TRUE)))
                    stop("the sha-256 hash of the downloaded file does not match the expected value. The downloaded file might be incomplete or compromised. If you wish to proceed anyway, run this function again with `check_hash = FALSE`")
            }
        }
        archive::archive_extract(zipname, dir = path)
    }
    ## now we should see the executable
    chk <- ov_ffmpeg_exe()
    if (!is.null(chk)) chk else stop("Sorry, ffmpeg install failed. You will need to install it yourself and ensure that it is on the system path.")
}

ovideo_app_dir <- function() rappdirs::user_data_dir(appname = "ovideo")

## adapted from http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function() {
    if (.Platform$OS.type == "windows") return("windows")
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf["sysname"]
        if (tolower(os) == "darwin")
            os <- "osx"
    } else {
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os, ignore.case = TRUE))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os, ignore.case = TRUE))
            os <- "linux"
    }
    os <- tolower(os)
    if (!os %in% c("windows", "linux", "unix", "osx"))
        stop("unknown operating system: ", os)
    os
}
