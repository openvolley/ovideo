#' Browse a system file in the default browser
#'
#' RStudio overrides the default behaviour of browseURL on some platforms, meaning that local files are not opened as `file:///...` URLs but as `http://localhost...`. This can break some local HTML files that are expecting to be served as `file:///` URLs.
#'
#' @param url string: as for [utils::browseURL()]
#' @param browser string: as for [utils::browseURL()]
#' @param encodeIfNeeded logical: as for [utils::browseURL()]
#'
#' @examples
#' myfile <- tempfile(fileext = ".html")
#' cat("<h1>Hello!</h1>", file = myfile)
#'
#' ## in RStudio on Linux, this will be opened as a http://localhost URL
#' if (interactive()) browseURL(myfile)
#'
#' ## but this shouldn't
#' browseFile(myfile)
#'
#' @export
browseFile <- function(url, browser = getOption("browser"), encodeIfNeeded = FALSE) {
    useBU <- TRUE
    if (is.function(browser) && identical(.Platform$GUI, "RStudio")) {
        ## running within RStudio
        if (.Platform$OS.type %in% c("unix") && nzchar(Sys.getenv("R_BROWSER"))) {
            useBU <- FALSE
        }
    }
    if (useBU) utils::browseURL(url = url, browser = browser, encodeIfNeeded = encodeIfNeeded) else system2(Sys.getenv("R_BROWSER"), url)
}


## these functions also defined identically in the datavolley package

#' Get or set the video metadata in a datavolley object
#'
#' @param x datavolley: a datavolley object as returned by [datavolley::dv_read()]
#' @param value string or data.frame: a string containing the path to the video file, or a data.frame with columns "camera" and "file"
#'
#' @return For `dv_meta_video`, the existing video metadata. For `dv_meta_video<-`, the video metadata value in `x` is changed
#'
#' @examples
#' x <- dv_read(dv_example_file())
#' dv_meta_video(x) ## empty dataframe
#' dv_meta_video(x) <- "/path/to/my/videofile"
#' dv_meta_video(x)
#'
#' @export
dv_meta_video <- function(x) {
    x$meta$video
}

#' @rdname dv_meta_video
#' @export
`dv_meta_video<-` <- function(x, value) {
    if (is.character(value)) {
        x$meta$video <- data.frame(camera = seq_along(value) - 1L, file = value, stringsAsFactors = FALSE)
    } else if (is.data.frame(value)) {
        if (!"file" %in% names(value)) stop("expecting 'file' column in the new value")
        x$meta$video <- value
    } else {
        stop("unexpected input value format")
    }
    x
}
