
## not exported, yet
ov_playlist_subtitles <- function(playlist, subtitle_column) {
    if (missing(subtitle_column) || !subtitle_column %in% names(playlist)) {
        stop("subtitle_column must be supplied and present in the playlist dataframe")
    }
    format_hmsms <- function(z) {
        ## expecting z in decimal seconds
        h <- floor(z/3600)
        m <- floor((z-h*3600)/60)
        s <- floor((z-h*3600-m*60))
        ms <- round((z-floor(z))*1000)
        sprintf("%02d:%02d:%02d,%03d", h, m, s, ms)
    }
    unlist(lapply(seq_len(nrow(playlist)),
                  function(z) {
                      this_times <- c(cumsum(c(0, playlist$duration))[z], cumsum(playlist$duration)[z])
                      c(z, paste0(format_hmsms(this_times[1]), " --> ", format_hmsms(this_times[2])),
                        playlist[[subtitle_column]][z], "")
                  }))
}
