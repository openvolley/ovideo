## functions for automatic homography detection of volleyball court


#' Detect court on an image
#'
#' @param image_file string: path to an image file (jpg) containing the court image (not required if `video_file` is supplied)
#' @param video_file string: path to a video file from which to extract the court image (not required if `image_file` is supplied)
#' @param t numeric: the time of the video frame to use as the court image (not required if `image_file` is supplied)
#' @param view.list vector: vector of court view
#' @param method string: either "LSD" or "Hough". See Details
#' @param score_distance string: Default to "colour-based". Calculate the likelihood of a homography based on the colour of the estimated lines locations
#' @param line_colour string: colour of the lines for the courts. Default to "white"
#' @param court_colour string: colour of the court. Default to "#c17257" (the orange-ish colour typical of synthetic-floor indoor courts)
#' @param colour.distance string: 
#' @param n.alpha numeric:
#' @param lambda numeric:
#'
#' @return A list of all possible homographies, with a score
#'
#' @seealso [ov_transform_points()], [datavolley::dv_court()],  [datavolley::ggcourt()]
#'
#' @examples
#' if (interactive()) {
#'   library(magick)
#'
#'   ## Example 1
#'   image_file <- system.file("extdata/2019_03_01-KATS-BEDS-court.jpg", package = "ovideo")
#'   court_df <- ov_detect_court(image_file = image_file, score_distance = "pattern-based", lambda = 1e3)
#'   ov_detect_court_plot(court_df, index = 1:4)
#'   ov_plot_patterns(court_df, index = 1)
#'   
#'   ## Example 2 - with players
#'   image_file <- system.file("extdata/2019_03_01-KATS-BEDS-frame.png", package = "ovideo")
#'   court_df <- ov_detect_court(image_file = image_file, score_distance = "pattern-based", lambda = 1e3)
#'   ov_detect_court_plot(court_df, index = 1:4, plot.all.endpoints = TRUE)
#'   ov_plot_patterns(court_df, index = 1)
#'   
#'   ## Example 3 - Net issue
#'   image_file <- system.file("extdata/2022_10_22M_CH_TE.png", package = "ovideo")
#'   court_df <- ov_detect_court(image_file = image_file, score_distance = "pattern-based", lambda = 1e3)
#'   ov_detect_court_plot(court_df, index = 1:9, plot.all.endpoints = TRUE)
#'  ov_plot_patterns(court_df, index = 1)
#'  
#'   ## Example 4 - Partial view and different court colour
#'   image_file <- system.file("extdata/2022_10_30M_TE_MV.png", package = "ovideo")
#'   court_df <- ov_detect_court(image_file = image_file, line_colour = "black", 
#'   court_colour = "#858582", score_distance = "pattern-based", lambda = 1e5)
#'   ov_detect_court_plot(court_df, index = 1:4, plot.all.endpoints = TRUE)
#'}
#'
#' @export
ov_detect_court <- function(image_file, video_file, t = 60,view.list = NULL,
                            method = "Hough", score_distance = "colour-based",
                            line_colour = "white", court_colour = "#c17257", 
                            colour.distance = "cie2000", n.alpha = 50, lambda = 100) {
    if (missing(image_file) || is.null(image_file)) {
        image_file <- ov_video_frame(video_file, t)
    }
    if(is.null(view.list)){
        view.list = c("full court", "full 3m", "full half", "full 3m c")
    }
    x <- magick::image_read(image_file)
    xx <- magick::image_canny(x)
    ## reduce detection to where the court is

    image_height <- magick::image_info(x)$height
    image_width <- magick::image_info(x)$width

    
    xr <- magick::image_raster(x, frame = 1, tidy = TRUE) %>%
        dplyr::mutate(dist_2_linecolour =
                          farver::compare_colour(from= farver::decode_colour(.data$col),
                                                 to = farver::decode_colour(line_colour),
                                                 "rgb",
                                                 method = colour.distance),
                      dist_2_courtcolour =
                          farver::compare_colour(from= farver::decode_colour(.data$col),
                                                 to = farver::decode_colour(court_colour),
                                                 "rgb",
                                                 method = colour.distance),
                      new_color = dplyr::case_when(.data$dist_2_courtcolour > 50 & .data$dist_2_linecolour > 50 ~ '#9F2B68',
                                                   TRUE ~ col))# %>%
    #dplyr::arrange(-.data$x, -.data$y)
    
    # x_mod = as.cimg(
    #     c(farver::decode_colour(dplyr::pull(xr, 'new_color'))[,1],
    #       farver::decode_colour(dplyr::pull(xr, 'new_color'))[,2],
    #       farver::decode_colour(dplyr::pull(xr, 'new_color'))[,3]), x= image_width,y = image_height,
    #     z = 1, cc = 3)
    # 
    # x_mod = as.cimg(
    #     dplyr::pull(xr, 'dist_2_linecolour'), x= image_width,y = image_height,
    #     z = 1, cc = 1)
    
    # ggplot(xr, aes(x=x, y=rev(y), fill = col)) +
    #     geom_tile(width = 1, height =1) + theme(legend.position = "none") +
    #     scale_fill_identity()
    # 
    # ggplot(xr, aes(x=x, y=rev(y), fill = new_color)) +
    #     geom_tile(width = 1, height =1) + theme(legend.position = "none") +
    #     scale_fill_identity()
    # 
    # ggplot(xr, aes(x=x, y=rev(y), fill = dist_2_courtcolour)) +
    #     geom_tile(width = 1, height =1)# + theme(legend.position = "none")
    
    if(method == "LSD"){
        xtm = array(c(farver::decode_colour(dplyr::pull(xr, 'new_color'))[,1],
                      farver::decode_colour(dplyr::pull(xr, 'new_color'))[,2],
                      farver::decode_colour(dplyr::pull(xr, 'new_color'))[,3]), 
                    dim = c(1280,720,3))
        #x <- imager::cimg2magick(x_mod)
        xx <- magick::image_read(xtm / 255) %>% magick::image_rotate(degrees = 90) %>% 
            magick::image_flop() %>%
            magick::image_scale("1280x720")
            
        
        mat <- magick::image_data(xx, channels = "gray")
        mat <- as.integer(mat, transpose = TRUE)
        mat <- drop(mat)
        #xm <- as.integer(xx, transpose = TRUE)
        #mat <- drop(xm)
        linesegments <- image_line_segment_detector(mat, 
                                                    union = TRUE)
        
        # plot(xx)
        # plot(x_mod)
        # plot(linesegments, add =TRUE, col ="red")

        LS <- as.data.frame(linesegments$lines)
        LS <- LS %>% mutate(Length = sqrt((.data$x1 - .data$x2)^2 + (.data$y1 - .data$y2)^2)) %>%
            dplyr::filter(abs(.data$y1 - .data$y2) < 150, .data$width < 25) %>%
            mutate(xs = case_when(.data$x1 > .data$x2 ~ .data$x2,
                                  TRUE ~ .data$x1),
                   ys = case_when(.data$x1 > .data$x2 ~ .data$y2,
                                  TRUE ~ .data$y1),
                   xe = case_when(.data$x1 > .data$x2 ~ .data$x1,
                                  TRUE ~ .data$x2),
                   ye = case_when(.data$x1 > .data$x2 ~ .data$y1,
                                  TRUE ~ .data$y2),
                   a = abs((.data$ye - .data$ys) / (.data$xe - .data$xs))) %>% dplyr::filter(.data$a < 0.1) %>%
            dplyr::top_n(n = 3, wt = .data$Length)
    }

    if(method == "Hough"){
        
        # img = as.cimg(
        #     c(farver::decode_colour(dplyr::pull(xr, 'new_color'))[,1],
        #       farver::decode_colour(dplyr::pull(xr, 'new_color'))[,2],
        #       farver::decode_colour(dplyr::pull(xr, 'new_color'))[,3]),
        #     x= image_width,y = image_height,
        #     z = 1, cc = 3)
        img <- imager::magick2cimg(xx)
        # plot(img)
        ht <- imager::hough_line(img, data.frame = TRUE, ntheta = 1000)

        temp <- dplyr::filter(ht, .data$score > quantile(.data$score, .9995))
        # imager::nfline(temp$theta, temp$rho, col = "blue")

        ht_h <- ht %>% mutate(
            st = sin(.data$theta),
            stf = case_when(st == 0 ~ 1e-4,
                            abs(st) < 1e-4 ~ 1e-4 * sign(st),
                            TRUE ~ st),
            a = -cos(.data$theta) / .data$stf,
            b = .data$rho / .data$stf) %>%
            dplyr::filter(dplyr::between(.data$theta,pi/2 -0.75,pi/2+ 0.75) | dplyr::between(.data$theta,3*pi/2 - 0.75, 3*pi/2 + 0.75)) %>%
            dplyr::filter(.data$score > quantile(.data$score, .9995)) %>% dplyr::arrange(-.data$score)

        ht_v <- ht %>% mutate(
            st = sin(.data$theta),
            stf = case_when(st == 0 ~ 1e-4,
                            abs(st) < 1e-4 ~ 1e-4 * sign(st),
                            TRUE ~ st),
            a = -cos(.data$theta) / .data$stf,
                              b = .data$rho / .data$stf) %>%
            dplyr::filter(dplyr::between(.data$theta, -0.95, 0.95) | dplyr::between(.data$theta, pi - 0.95, pi + 0.95)) %>%
            dplyr::filter(.data$score > quantile(.data$score, .9995)) %>% dplyr::arrange(-.data$score)
# 
#         imager::nfline(ht_h$theta, ht_h$rho, col = "lightblue")
#         imager::nfline(ht_v$theta, ht_v$rho, col = "green")

        mat_dist = matrix(xr$dist_2_linecolour, byrow = TRUE, nrow = image_height)
        mat_dist_2 = matrix(xr$dist_2_courtcolour, byrow = TRUE, nrow = image_height)
        
        col_dist = function(a,b){
            xi = seq_len(image_width)
            yi = floor(xi * a + b)
            ddh <- cbind(xi, yi)[which(yi %in% seq_len(image_height)), ]
            mean(mat_dist[ddh[,c(2,1)]])
        }
        
        htc_v <- ov_cluster(ht_v) %>% 
            dplyr::top_n(n = 10, wt = .data$score) %>% dplyr::rowwise() %>%
                          dplyr::mutate(dist2line_colour = col_dist(.data$a, .data$b)) %>% 
            ungroup() %>%  dplyr::mutate(full_score = .data$score - .data$dist2line_colour) %>%
            dplyr::top_n(n = 5, wt = .data$full_score)
        htc_h <- ov_cluster(ht_h) %>% dplyr::top_n(n = 10, wt = .data$score) %>% dplyr::rowwise() %>%
                         dplyr::mutate(dist2line_colour = col_dist(.data$a, .data$b)) %>% ungroup() %>%
            dplyr::mutate(full_score = .data$score - .data$dist2line_colour) %>%
            dplyr::top_n(n = 5, wt = .data$full_score)
        
        # plot(img)
        # imager::nfline(htc_h$theta, htc_h$rho, col = "cyan")
        # imager::nfline(htc_v$theta, htc_v$rho, col = "darkgreen")

        # NOW NEED TO CALCULATE CROSSLINE PT and get LS out of that. TODO

        #LS <- NULL # PLACEHOLDER
        LS_tmp <- NULL
        list_combn = combn(1:nrow(htc_v), 2)
        for(ih in 1:nrow(htc_h)){
         for(iv in 1:ncol(list_combn)){
             ivi = list_combn[1,iv]
             ivii = list_combn[2,iv]
             x_tmps = (htc_h$b[ih] - htc_v$b[ivi]) / (-htc_h$a[ih] + htc_v$a[ivi])
             if(!dplyr::between(x_tmps, 1, image_width)) next
             y_tmps = htc_h$a[ih] * x_tmps + htc_h$b[ih]
             if(!dplyr::between(y_tmps, 1, image_height)) next
             x_tmpe = (htc_h$b[ih] - htc_v$b[ivii]) / (-htc_h$a[ih] + htc_v$a[ivii])
             if(!dplyr::between(x_tmpe, 1, image_width)) next
             y_tmpe = htc_h$a[ih] * x_tmpe + htc_h$b[ih]
             if(!dplyr::between(y_tmpe, 1, image_height)) next
             LS_tmp = bind_rows(LS_tmp, data.frame(x1 = x_tmps, y1 = image_height - y_tmps,
                                                   x2 = x_tmpe, y2 = image_height - y_tmpe, 
                                                   LH=ih, score = htc_h$full_score[ih] + 
                                                       htc_v$full_score[ivi] + 
                                                       htc_v$full_score[ivii]))
         }   
        }
        
        LS <- LS_tmp %>% mutate(Length = sqrt((.data$x1 - .data$x2)^2 + (.data$y1 - .data$y2)^2), 
                                dy = abs(.data$y1 - .data$y2), dx=abs(.data$x1 - .data$x2)) %>%
           dplyr::filter((.data$dy < 150) & ( .data$dx > image_width / 3)) %>%
           mutate(xs = case_when(.data$x1 > .data$x2 ~ .data$x2,
                                 TRUE ~ .data$x1),
                  ys = case_when(.data$x1 > .data$x2 ~ .data$y2,
                                 TRUE ~ .data$y1),
                  xe = case_when(.data$x1 > .data$x2 ~ .data$x1,
                                 TRUE ~ .data$x2),
                  ye = case_when(.data$x1 > .data$x2 ~ .data$y1,
                                 TRUE ~ .data$y2),
                  a = abs((.data$ye - .data$ys) / (.data$xe - .data$xs))) %>% 
            dplyr::filter(.data$a < 0.1) %>% dplyr::arrange(.data$ys) %>% 
            group_by(.data$LH) %>%
            dplyr::top_n(n = 2, wt = .data$score) %>% ungroup()
        
    }
    # segments(x0 = LS$xs, y0 = image_height - LS$ys, x1 = LS$xe, y1 = image_height - LS$ye, col = "red")

    crt_ref <- data.frame(court_x = c(0.5, 3.5, 0.5, 3.5,
                                      0.5, 3.5,
                                      0.5, 3.5, 0.5, 3.5),
                       court_y = c(0.5, 0.5, 2.5, 2.5,
                                   3.5,3.5,
                                   4.5, 4.5, 6.5, 6.5),
                       side = c("left", "right", "left", "right","left", "right", "left", "right", "left", "right"),
                       depth = c("close", "close", "close", "close", "mid", "mid", "far", "far", "far", "far"),
                       linetype = c("serve-line", "serve-line",
                                 "3m-line", "3m-line",
                                 "center-line", "center-line",
                                 "3m-line", "3m-line",
                                 "serve-line", "serve-line"))

    homography_transform = function(x, X) {
        matrix(c(-x[1], -x[2], -1, 0, 0, 0, x[1]*X[1], x[2]*X[1], X[1], 0, 0, 0, -x[1], -x[2], -1, x[1]*X[2], x[2]*X[2], X[2]), nrow = 2, byrow = TRUE)
    }

    alpha <- seq(0, 1, length.out = n.alpha)

    court_df <- list()
    for(i in seq_len(nrow(LS) - 1)){
        for(j in seq(i, nrow(LS))){
            if(j == i) next
            if(abs(LS$ys[i] - LS$ys[j]) < 150) next
            if(LS$ys[i] >= LS$ys[j]) next
            # FULL COURT
            for(cc in view.list){
                if(cc == "full court") ref <- crt_ref[c(1, 2, 10, 9), ]
                if(cc == "full 3m") ref <- crt_ref[c(1, 2, 8, 7), ]
                if(cc == "full half") ref <- crt_ref[c(1, 2, 6, 5), ]
                if(cc == "full 3m c") ref <- crt_ref[c(1, 2, 4, 3), ]

            ref$image_x = c(LS$xs[i], LS$xe[i], LS$xe[j], LS$xs[j])/image_width
            ref$image_y = c(LS$ys[i], LS$ye[i], LS$ye[j], LS$ys[j])/image_height

            # plot(x)
            # polygon(x = ref$image_x*1280, y = ref$image_y*720)

            idx <- if (nrow(ref) == 4) 1:4 else c(1, 2, 8, 9)
            PH <- do.call(rbind, lapply(idx, function(i) homography_transform(unlist(ref[i, c("court_x", "court_y")]), unlist(ref[i, c("image_x","image_y")]))))
            PH <- rbind(PH, c(rep(0, 8), 1))
            Hres <- solve(a = PH, b = matrix(c(rep(0, 8), 1), ncol = 1))
            H <- matrix(Hres, ncol = 3, byrow = TRUE)
            tmp <- apply(cbind(crt_ref$court_x, crt_ref$court_y, 1), 1, function(xx) H %*% matrix(xx, ncol = 1))
            crt_ref_e<- cbind(crt_ref, data.frame(x = tmp[1, ] / tmp[3, ] * image_width, y = tmp[2, ] / tmp[3, ] * image_height))
            estimated_lines <- crt_ref_e %>% 
                tidyr::pivot_wider(names_from = "side", values_from = c("x", "y", "court_x", "court_y"))

            
            estimated_lines_v <-crt_ref_e[c(1,2,9,10),] %>% tidyr::pivot_wider(names_from = "depth", values_from = c("x", "y", "court_x", "court_y"))
            # plot(x)
            # polygon(crt_ref_e$x[c(1,2,10,9)], crt_ref_e$y[c(1,2,10,9)], density = 25)
            # polygon(crt_ref_e$x[c(3,4,8,7)], crt_ref_e$y[c(3,4,8,7)], density = 50)
            # segments(x0 = estimated_lines$x_left[3], y0 = estimated_lines$y_left[3], x1 = estimated_lines$x_right[3], y1 = estimated_lines$y_right[3], col = "white")

            if(score_distance %in% c("colour-based", "color-based")) {
                
                px_h <- do.call(c, lapply(seq_len(nrow(estimated_lines)), function(jjj) 
                    round(estimated_lines$x_left[jjj]):round(estimated_lines$x_right[jjj])))
                
                py_h <- do.call(c, lapply(seq_len(nrow(estimated_lines)), function(jjj) {
                    alpha = seq(0,1, length.out = length(round(estimated_lines$x_left[jjj]):round(estimated_lines$x_right[jjj])))
                    floor((image_height - estimated_lines$y_right[jjj])*alpha + (image_height - estimated_lines$y_left[jjj]) * (1-alpha))}))

                #py_h <- image_height - py_h
            
                py_v <- do.call(c, lapply(seq_len(nrow(estimated_lines_v)), function(jjj) 
                    round(image_height - estimated_lines_v$y_far[jjj]):round(image_height - estimated_lines_v$y_close[jjj])))
                px_v <- do.call(c, lapply(seq_len(nrow(estimated_lines_v)), function(jjj) {
                    alpha = seq(0,1, length.out = length(round(estimated_lines_v$y_close[jjj]):round(estimated_lines_v$y_far[jjj])))
                    floor(estimated_lines_v$x_close[jjj]*alpha + estimated_lines_v$x_far[jjj] * (1-alpha))}))
                
                #py_v <- image_height - py_v
                
                ddh <- cbind(px_h, py_h)[which(px_h %in% seq_len(image_width) & py_h %in% seq_len(image_height)), ]

                ddv <- cbind(px_v, py_v)[which(px_v %in% seq_len(image_width) & py_v %in% seq_len(image_height)), ]
                
                # image_ggplot(xr) + geom_point(data =data.frame(x= px_h, y = py_h),
                #                               aes(x=x,y=y), col = "red")
                score = mean(mat_dist[ddh[,c(2,1)]]) + mean(mat_dist[ddv[,c(2,1)]])
                
                # # Check court colour is close to theoretical court colour:
                # alpha_c = seq(0,1,length.out = 50)
                # 
                # inside_court = data.frame(x_close = estimated_lines_v$x_close[1] * alpha_c + 
                #                               estimated_lines_v$x_close[2] * (1-alpha_c), 
                #                           y_close = estimated_lines_v$y_close[1] * alpha_c + 
                #                               estimated_lines_v$y_close[2] * (1-alpha_c), 
                #                           x_far = estimated_lines_v$x_far[1] * alpha_c + 
                #                               estimated_lines_v$x_far[2] * (1-alpha_c), 
                #                           y_far = estimated_lines_v$y_far[1] * alpha_c + 
                #                               estimated_lines_v$y_far[2] * (1-alpha_c))
                # py_c <- do.call(c, lapply(seq_len(nrow(inside_court)), function(jjj) 
                #     round(inside_court$y_close[jjj]):round(inside_court$y_far[jjj])))
                # px_c <- do.call(c, lapply(seq_len(nrow(inside_court)), function(jjj) {
                #     alpha = seq(0,1, length.out = length(round(inside_court$y_close[jjj]):round(inside_court$y_far[jjj])))
                #     floor(inside_court$x_close[jjj]*alpha + inside_court$x_far[jjj] * (1-alpha))}))
                # 
                # ddc <- cbind(px_c, py_c)[which(px_c %in% seq_len(image_width) & py_c %in% seq_len(image_height)), ]
                # 
                # score = score + mean(mat_dist_2[ddc[,c(2,1)]])
                
                # pch <- apply(ddh, 1, function(x) xr$col[which(xr$x == x[1] & xr$y == x[2])])
                # pcv <- apply(ddv, 1, function(x) xr$col[which(xr$x == x[1] & xr$y == x[2])])
                
                # browser()
                # 
                # ggplot(xr) +
                #          geom_tile(aes(x=x, y=(y), fill = col) ,
                #                    width = 1, height =1) + theme(legend.position = "none") +
                #          scale_fill_identity() +
                #     geom_point(data =data.frame(x= px_h, y = py_h),
                #                  aes(x=x,y=y), col = "pink") + 
                #     geom_point(data =data.frame(x= px_h, y = py_h, col = pch),
                #               aes(x=x,y=y, col = col), size = 0.5)+
                #     scale_color_identity()
                #     # geom_polygon(data = ref, aes(x = image_x*image_width, y = image_y*image_height)) + 
                #     # geom_segment(data = LS, aes(x = xs, y = ys, xend = xe, yend = ye), col = "red")
                
                
                #dd = data.frame(x=px, y=py, col = pc)

                #ggplot(dd, aes(x=x, y=rev(y), fill = col)) +
                #    geom_tile(width = 10, height =10) + theme(legend.position = "none") +
                #    scale_fill_identity() + coord_fixed() + theme_void()

                # rch <- farver::decode_colour(pch)
                # rcv <- farver::decode_colour(pcv)
                # hc <- farver::decode_colour(line_colour)
                # 
                # score = mean(farver::compare_colour(rch, hc, "rgb", method = colour.distance)) + 
                #     mean(farver::compare_colour(rcv, hc, "rgb", method = colour.distance))
                    
            } else if (score_distance %in% c("pattern-based")) {

                px_h <- do.call(c, lapply(seq_len(nrow(estimated_lines)), function(jjj) 
                    round(estimated_lines$x_left[jjj]):round(estimated_lines$x_right[jjj])))
                px_h <- c(px_h, px_h, px_h, px_h - 1, px_h - 1)
                py_h <- do.call(c, lapply(seq_len(nrow(estimated_lines)), function(jjj) {
                    alpha = seq(0,1, length.out = length(round(estimated_lines$x_left[jjj]):round(estimated_lines$x_right[jjj])))
                    floor((image_height - estimated_lines$y_right[jjj])*alpha + (image_height - estimated_lines$y_left[jjj]) * (1-alpha))}))
                py_h <- c(py_h, py_h + 1, py_h - 1, py_h, py_h)
                #py_h <- image_height - py_h
                
                py_v <- do.call(c, lapply(seq_len(nrow(estimated_lines_v)), function(jjj) 
                    round(image_height - estimated_lines_v$y_far[jjj]):round(image_height - estimated_lines_v$y_close[jjj])))
                px_v <- do.call(c, lapply(seq_len(nrow(estimated_lines_v)), function(jjj) {
                    alpha = seq(0,1, length.out = length(round(estimated_lines_v$y_close[jjj]):round(estimated_lines_v$y_far[jjj])))
                    floor(estimated_lines_v$x_close[jjj]*alpha + estimated_lines_v$x_far[jjj] * (1-alpha))}))
                
                #test0 <- data.frame(x = px_v, y = py_v)
                
                px_v <- c(px_v, px_v, px_v, px_v - 1, px_v - 1)
                py_v <- c(py_v, py_v + 1, py_v - 1, py_v, py_v)
                
                #test = data.frame(x = px_v, y = py_v)
                
                #py_v <- image_height - py_v
                
                ddh <- cbind(px_h, py_h)[which(px_h %in% seq_len(image_width) & py_h %in% seq_len(image_height)), ]
                
                ddv <- cbind(px_v, py_v)[which(px_v %in% seq_len(image_width) & py_v %in% seq_len(image_height)), ]
                
                # par(mfrow=c(3,2), mar = c(2,2,2,1))
                # image(x = 1:image_height, y = 1:image_width ,z=  mat_dist, xlab = NULL, ylab = NULL)
                # text(image_height - crt_ref_e$y, crt_ref_e$x ,labels = 1:nrow(crt_ref_e))
                
                pattern_i = matrix(farver::compare_colour(farver::decode_colour(court_colour), 
                                                          farver::decode_colour(line_colour), "rgb", method = colour.distance), 
                                   ncol = image_width, nrow = image_height)
                
                pattern_i[ddh[,c(2,1)]] <- 0
                pattern_i[ddv[,c(2,1)]] <- 0
                
                # image(x = 1:image_height, y = 1:image_width ,z=  pattern_i, xlab = NULL, ylab = NULL)
                # text(image_height - crt_ref_e$y, crt_ref_e$x ,labels = 1:nrow(crt_ref_e))
                
                length.x = image_width / 40
                length.y = image_height / 40
                score.tmp = 0
                for(kk in 1:nrow(crt_ref_e)){
                    cell_y = floor(seq.int(image_height -crt_ref_e$y[kk]-length.y/2, image_height -crt_ref_e$y[kk]+length.y/2))
                    cell_x = floor(seq.int(crt_ref_e$x[kk]-length.x/2, crt_ref_e$x[kk] + length.x/2))
                    
                    cell_y = cell_y[cell_y %in% seq_len(image_height)]
                    cell_x = cell_x[cell_x %in% seq_len(image_width)]
                    
                    cc_xy <- as.matrix(expand.grid(cell_y, cell_x))
                    #cc_xy <-cc_xy[which(cc_xy[,1] %in% seq_len(image_width) & cc_xy[,2] %in% seq_len(image_height)), ]
                    
                    est_pat = matrix(mat_dist[cc_xy], ncol = length(cell_x), byrow = FALSE)
                    
                    #image(x = cell_y, y = cell_x, z= est_pat)
                    #text(image_height - crt_ref_e$y[kk], crt_ref_e$x[kk], labels = kk)
                    
                    the_pat = matrix(pattern_i[cc_xy], ncol = length(cell_x), byrow = FALSE)
                    
                    #contour(x = cell_y, y = cell_x, z= the_pat, add = TRUE, levels = 30, drawlabels = FALSE, lwd = 3)
                    
                    OTdist = tryCatch({T4transport::ipot(est_pat, the_pat, lambda = lambda)$distance}, 
                                  error = function(cond) {
                                      return(1e4)
                                  })
                    score.tmp = c(score.tmp, OTdist)
                }
                
                score = mean(sort(score.tmp)[1:6])
                
                # image_ggplot(xr) + geom_point(data =data.frame(x= px_h, y = py_h),
                #                               aes(x=x,y=y), col = "red")
                # score = mean(mat_dist[ddh[,c(2,1)]]) + mean(mat_dist[ddv[,c(2,1)]])
                # 
                # # Check court colour is close to theoretical court colour:
                # alpha_c = seq(0,1,length.out = 50)
                # 
                # inside_court = data.frame(x_close = estimated_lines_v$x_close[1] * alpha_c + 
                #                               estimated_lines_v$x_close[2] * (1-alpha_c), 
                #                           y_close = estimated_lines_v$y_close[1] * alpha_c + 
                #                               estimated_lines_v$y_close[2] * (1-alpha_c), 
                #                           x_far = estimated_lines_v$x_far[1] * alpha_c + 
                #                               estimated_lines_v$x_far[2] * (1-alpha_c), 
                #                           y_far = estimated_lines_v$y_far[1] * alpha_c + 
                #                               estimated_lines_v$y_far[2] * (1-alpha_c))
                # py_c <- do.call(c, lapply(seq_len(nrow(inside_court)), function(jjj) 
                #     round(inside_court$y_close[jjj]):round(inside_court$y_far[jjj])))
                # px_c <- do.call(c, lapply(seq_len(nrow(inside_court)), function(jjj) {
                #     alpha = seq(0,1, length.out = length(round(inside_court$y_close[jjj]):round(inside_court$y_far[jjj])))
                #     floor(inside_court$x_close[jjj]*alpha + inside_court$x_far[jjj] * (1-alpha))}))
                # 
                # ddc <- cbind(px_c, py_c)[which(px_c %in% seq_len(image_width) & py_c %in% seq_len(image_height)), ]
                # 
                # score = score + mean(mat_dist_2[ddc[,c(2,1)]])
                
                
            } else {
                score = 0
                idxLS = seq_len(nrow(LS))
                LSt = LS
                for(k in seq_len(nrow(estimated_lines))){
                    score_tmp_all = (estimated_lines$x_left[k] - LSt$xs)^2 + (estimated_lines$y_left[k] - LSt$ys)^2 +
                        (estimated_lines$x_right[k] - LSt$xe)^2 + (estimated_lines$y_right[k] - LSt$ye)^2
                    idxM = which.min(score_tmp_all)
                    score = score + score_tmp_all[idxM]
                    LSt = LSt[-idxM,]
                }
            }

            tmp = list(score = score, H = H, ref = ref, court_ref = crt_ref_e, view = cc)
            court_df = append(court_df, list(tmp))
            }
        }
    }

    court_df <- court_df[rlist::list.order(court_df, score)]
    
    return(list(image_file = image_file, court_list = court_df, lines = LS, line_colour = line_colour, 
                court_colour = court_colour, lambda = lambda, colour.distance = colour.distance))
}

ov_cluster <- function(lines, d_a = 5, d_b = 0.05){
    C = 1
    L = list()
    for(i in seq_len(nrow(lines))){
        if(i == 1){
            L[1] <- i
            data_cl = data.frame(cluster = 1, a = lines$a[i], b = lines$b[i], 
                                 theta = lines$theta[i], rho = lines$rho[i], 
                                 score = lines$score[i])
            next
        }
        clustered  =FALSE
        for(j in seq_len(C)){
            da = abs(data_cl$a[j] - lines$a[i])
            db = abs(data_cl$b[j] - lines$b[i]) / max(abs(data_cl$b[j]), abs(lines$b[i]))
            if(da < d_a & db < d_b){
                clustered = TRUE
                data_cl$score[j] = data_cl$score[j] + lines$score[i]
                #data_cl$theta[j] = (data_cl$theta[j]*length(L[[j]]) + lines$theta[i]) / (length(L[[j]]) + 1)
                #data_cl$a[j] = (data_cl$a[j]*length(L[[j]]) + lines$a[i])  / (length(L[[j]]) + 1)
                #data_cl$rho[j] = (data_cl$rho[j]*length(L[[j]]) + lines$rho[i])  / (length(L[[j]]) + 1)
                L[[j]] <- c(L[[j]], i)
                break
            }
        }
        if(!clustered){
            C <- C + 1
            data_cl = dplyr::bind_rows(data_cl, 
                                       data.frame(cluster = C, 
                                                  a = lines$a[i], 
                                                  b = lines$b[i], 
                                                  theta = lines$theta[i], 
                                                  rho = lines$rho[i], 
                                                  score = lines$score[i]))
            L[C] <- i
        }
    }
    return(data_cl)
}

ov_detect_court_plot <- function(obj, index = 1, model.filter = NULL, plot.segments = FALSE, plot.all.segments = FALSE, 
                                 plot.endpoints = FALSE, plot.all.endpoints = FALSE){
    
    xi <- magick::image_read(obj$image_file)
    ## reduce detection to where the court is
    
    image_height <- magick::image_info(xi)$height
    image_width <- magick::image_info(xi)$width
    
    plot_fun <- function(xtmp){
        estimated_lines <- xtmp$court_ref %>% tidyr::pivot_wider(names_from = "side", 
                                                           values_from = c("x", "y", "court_x", "court_y"))
        used_lines <- xtmp$ref %>% 
            dplyr::mutate(image_x = .data$image_x *image_width, 
                          image_y = .data$image_y *image_height) %>%
            tidyr::pivot_wider(names_from = "side", values_from = c("image_x", "image_y", "court_x", "court_y"))
      gP<- 
          magick::image_ggplot(xi) + 
          geom_polygon(data = xtmp$court_ref[c(1,2,10,9),], aes_string("x", "y"), fill = "white", alpha = 0.25, col = "white") + 
          geom_polygon(data = xtmp$court_ref[c(3,4,8,7),], aes_string("x", "y"), fill = "white", alpha = 0.25, col = "white") + 
          geom_segment(data = estimated_lines[3,], aes_string(x = "x_left", y = "y_left", 
                                                       xend = "x_right", yend = "y_right"), col = "white") + 
        {if(plot.all.segments) geom_segment(data = obj$lines, aes_string(x = "xs", y = "ys", xend = "xe", yend = "ye"), col = "red")} + 
          {if(plot.all.endpoints) geom_point(data = obj$lines, 
                                         aes_string(x = "xs", y = "ys"), col = "blue")} + 
          {if(plot.all.endpoints) geom_point(data = obj$lines, 
                                         aes_string(x = "xe", 
                                                    y = "ye"), col = "green")} + 
          {if(plot.segments) geom_segment(data = used_lines, 
                                        aes_string(x = "image_x_left", y = "image_y_left", 
                                              xend = "image_x_right", 
                                              yend = "image_y_right"), col = "orange")} + 
          {if(plot.endpoints) geom_point(data = used_lines, 
                                          aes_string(x = "image_x_left", y = "image_y_left"), col = "blue")} + 
          {if(plot.endpoints) geom_point(data = used_lines, 
                                         aes_string(x = "image_x_right", 
                                                    y = "image_y_right"), col = "green")} + 
          coord_fixed(xlim=c(-10, image_width + 10), ylim = c(-10, image_height + 10))+
          ggtitle(label = paste0("Model: ",xtmp$view,"\n Score: " ,round(xtmp$score,2))) + 
          theme(plot.title = element_text(hjust = 0.5))
    }
    
    to_plot = obj$court_list
    
    
    if(!is.null(index)) to_plot = to_plot[unique(sapply(index, function(x) min(x, length(obj$court_list))))]
    
    plot_list = lapply(to_plot, FUN = plot_fun)
    
    cowplot::plot_grid(plotlist = plot_list)
}


ov_plot_patterns <- function(obj, index = 1){
    
    xi <- magick::image_read(obj$image_file)
    ## reduce detection to where the court is
    
    line_colour = obj$line_colour
    court_colour = obj$court_colour
    
    xr <- magick::image_raster(xi, frame = 1, tidy = TRUE) %>%
        dplyr::mutate(dist_2_linecolour =
                          farver::compare_colour(from= farver::decode_colour(.data$col),
                                                 to = farver::decode_colour(line_colour),
                                                 "rgb",
                                                 method = obj$colour.distance),
                      dist_2_courtcolour =
                          farver::compare_colour(from= farver::decode_colour(.data$col),
                                                 to = farver::decode_colour(court_colour),
                                                 "rgb",
                                                 method = obj$colour.distance),
                      new_color = dplyr::case_when(dist_2_courtcolour > 50 & dist_2_linecolour > 50 ~ '#9F2B68',
                                                   TRUE ~ col))# %>%
    
    image_height <- magick::image_info(xi)$height
    image_width <- magick::image_info(xi)$width
    
    to_plot = obj$court_list
    
    to_plot = to_plot[unique(sapply(index, function(x) min(x, length(obj$court_list))))]
    
    crt_ref_e <- to_plot[[1]]$court_ref
    
    estimated_lines <- crt_ref_e %>% tidyr::pivot_wider(names_from = "side", values_from = c("x", "y", "court_x", "court_y"))
    
    
    estimated_lines_v <-crt_ref_e[c(1,2,9,10),] %>% tidyr::pivot_wider(names_from = "depth", values_from = c("x", "y", "court_x", "court_y"))
    
    px_h <- do.call(c, lapply(seq_len(nrow(estimated_lines)), function(jjj) 
        round(estimated_lines$x_left[jjj]):round(estimated_lines$x_right[jjj])))
    px_h <- c(px_h, px_h, px_h, px_h - 1, px_h - 1)
    py_h <- do.call(c, lapply(seq_len(nrow(estimated_lines)), function(jjj) {
        alpha = seq(0,1, length.out = length(round(estimated_lines$x_left[jjj]):round(estimated_lines$x_right[jjj])))
        floor((image_height - estimated_lines$y_right[jjj])*alpha + (image_height - estimated_lines$y_left[jjj]) * (1-alpha))}))
    py_h <- c(py_h, py_h + 1, py_h - 1, py_h, py_h)
    #py_h <- image_height - py_h
    
    py_v <- do.call(c, lapply(seq_len(nrow(estimated_lines_v)), function(jjj) 
        round(image_height - estimated_lines_v$y_far[jjj]):round(image_height - estimated_lines_v$y_close[jjj])))
    px_v <- do.call(c, lapply(seq_len(nrow(estimated_lines_v)), function(jjj) {
        alpha = seq(0,1, length.out = length(round(estimated_lines_v$y_close[jjj]):round(estimated_lines_v$y_far[jjj])))
        floor(estimated_lines_v$x_close[jjj]*alpha + estimated_lines_v$x_far[jjj] * (1-alpha))}))
    
    #test0 <- data.frame(x = px_v, y = py_v)
    
    px_v <- c(px_v, px_v, px_v, px_v - 1, px_v - 1)
    py_v <- c(py_v, py_v + 1, py_v - 1, py_v, py_v)
    
    #test = data.frame(x = px_v, y = py_v)
    
    #py_v <- image_height - py_v
    
    ddh <- cbind(px_h, py_h)[which(px_h %in% seq_len(image_width) & py_h %in% seq_len(image_height)), ]
    
    ddv <- cbind(px_v, py_v)[which(px_v %in% seq_len(image_width) & py_v %in% seq_len(image_height)), ]
    
    pattern_i = matrix(farver::compare_colour(farver::decode_colour(court_colour), 
                                              farver::decode_colour(line_colour), "rgb", method = obj$colour.distance), 
                       ncol = image_width, nrow = image_height)
    
    pattern_i[ddh[,c(2,1)]] <- 0
    pattern_i[ddv[,c(2,1)]] <- 0
    
    mat_dist = matrix(xr$dist_2_linecolour, byrow = TRUE, nrow = image_height)
    
    length.x = image_width / 40
    length.y = image_height / 40

    par(mfrow=c(5,3), mar = c(1,1,1,1))

    layout.matrix <- matrix(c(9,7,5,3,1, 11,11,11,11,11, 10,8,6,4,2), nrow = 5, ncol = 3)
    layout(mat = layout.matrix)
    for(kk in 1:nrow(crt_ref_e)){
        cell_y = floor(seq.int(image_height -crt_ref_e$y[kk]-length.y/2, image_height -crt_ref_e$y[kk]+length.y/2))
        cell_x = floor(seq.int(crt_ref_e$x[kk]-length.x/2, crt_ref_e$x[kk] + length.x/2))
        
        cell_y = cell_y[cell_y %in% seq_len(image_height)]
        cell_x = cell_x[cell_x %in% seq_len(image_width)]
        
        cc_xy <- as.matrix(expand.grid(cell_y, cell_x))
        #cc_xy <-cc_xy[which(cc_xy[,2] %in% seq_len(image_width) & cc_xy[,1] %in% seq_len(image_height)), ]
        
        est_pat = matrix(mat_dist[cc_xy], ncol = length(cell_x), byrow = FALSE)
        
        OTdist = tryCatch({T4transport::ipot(est_pat, the_pat, lambda = obj$lambda)$distance}, 
                          error = function(cond) {
                              return(1e4)
                          })
        
        image(x = cell_x, y = cell_y, z= t(est_pat)[,nrow(est_pat):1],xaxt= "n", yaxt= "n", main = round(OTdist,2))
        text( crt_ref_e$x[kk],image_height - crt_ref_e$y[kk], labels = kk)
        
        the_pat = matrix(pattern_i[cc_xy], ncol = length(cell_x), byrow = FALSE)
        
        contour(x = cell_x, y = cell_y, z= t(the_pat)[,nrow(the_pat):1], add = TRUE, levels = 30, drawlabels = FALSE, lwd = 3)
        
        
    }
    image(1:image_width, 1:image_height,t(mat_dist)[,nrow(mat_dist):1],xaxt= "n", yaxt= "n" )
    contour(1:image_width, 1:image_height,t(pattern_i)[,nrow(pattern_i):1], add = TRUE, levels = 30, drawlabels = FALSE, lwd = 1)
    text( crt_ref_e$x, crt_ref_e$y, labels = 1:nrow(crt_ref_e))
}
