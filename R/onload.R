.onLoad <- function(libname, pkgname) {
    ## do we have ffmpeg available?
    options(ovideo = list(ffmpeg_exists = ov_ffmpeg_exists()))
    invisible()
}
