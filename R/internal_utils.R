is_youtube_id <- function(z) {
    if (is.null(z)) {
        FALSE
    } else if (!is.character(z)) {
        rep(FALSE, length(z))
    } else {
        !is.na(z) & nchar(z) == 11 & grepl("^[[:alnum:]_\\-]+$", z)
    }
}

youtube_url_to_id <- function(z) {
    if (!is_youtube_id(z) && grepl("^https?://", z, ignore.case = TRUE)) {
        if (grepl("youtu\\.be", z, ignore.case = TRUE)) {
            ## assume https://youtu.be/xyz form
            try({
                temp <- httr::parse_url(z)
                if (!is.null(temp$path) && length(temp$path) == 1) {
                    if (is_youtube_id(temp$path)) return(temp$path)
                    warning("YouTube URL found but the video ID does not appear to be valid")
                }
            })
        } else if (grepl("youtube", z, ignore.case = TRUE)) {
            try({
                temp <- httr::parse_url(z)
                if (!is.null(temp$query$v) && length(temp$query$v) == 1) {
                    if (is_youtube_id(temp$query$v)) return(temp$query$v)
                    warning("YouTube URL found but the video ID does not appear to be valid")
                }
            })
        }
    }
    ## if we got this far, return the input unchanged
    z
}

is_twitch_video <- function(z) {
    if (is.null(z)) {
        FALSE
    } else if (!is.character(z)) {
        rep(FALSE, length(z))
    } else {
        grepl("twitch\\.tv", z)
    }
}

twitch_url_to_id <- function(z) {
    if (grepl("^https?://", z, ignore.case = TRUE) && grepl("twitch\\.tv", z, ignore.case = TRUE)) {
            ## assume https://www.twitch.tv/CHANNEL/video/ID form
            tryCatch({
                temp <- httr::parse_url(z)
                if (!is.null(temp$path) && length(temp$path) == 1) {
                    tail(fs::path_split(temp$path)[[1]], 1)
                } else {
                    z
                }
            }, error = function(e) z)
    } else {
        z
    }
}

str_first_upper <- function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

## extract a .tar.xz file without using archive pkg
extract_txz <- function(txzfile, exdir) {
    if (!dir.exists(exdir)) stop("exdir must exist")
    tf <- tempfile(fileext = ".tar")
    on.exit(unlink(tf))
    writeBin(memDecompress(readBin(txzfile, what = "raw", n = file.size(txzfile) * 1.2), type = "xz"), con = tf)
    utils::untar(tf, exdir = fs::path_real(exdir))
}
