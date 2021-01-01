#' Example video clips provided as part of the ovideo package
#'
#' @param choice integer: which video file to return?
#' - 1 - a clip from a match between GKS Katowice and MKS Bedzin during the 2018/19 Polish Plus Liga
#'
#' @return Path to the video file
#'
#' @export
ov_example_video <- function(choice = 1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1" = system.file("extdata/2019_03_01-KATS-BEDS-clip.mp4", package = "ovideo"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}
