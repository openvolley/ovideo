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
