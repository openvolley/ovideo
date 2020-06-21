is_youtube_id <- function(z) {
    if (is.null(z)) {
        FALSE
    } else if (!is.character(z)) {
        rep(FALSE, length(z))
    } else {
        !is.na(z) & nchar(z) == 11 & grepl("^[[:alnum:]_\\-]+$", z)
    }
}
