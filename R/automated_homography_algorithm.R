## functions for automatic homography detection of volleyball court


#' Detect court on an image
#'
#' @param image_file string: path to an image file (jpg) containing the court image (not required if `video_file` is supplied)
#' @param video_file string: path to a video file from which to extract the court image (not required if `image_file` is supplied)
#' @param t numeric: the time of the video frame to use as the court image (not required if `image_file` is supplied)
#' @param method string: either "LSD" or "Hough". See Details
#' @param score_distance string: Default to "colour-based". Calculate the likelihood of a homography based on the colour of the estimated lines locations
#' @param line_colour string: colour of the lines for the courts. Default to "white"
#' @param court_colour string: colour of the court. Default to "#c17257" (the orange-ish colour typical of synthetic-floor indoor courts)
#' @param image_width numeric: width of the image in pixels. Default to 1280
#' @param image_height numeric: height of the image in pixels. Default to 720
#'
#' @return A list of all possible homographies, with a score
#'
#' @seealso [ov_transform_points()], [datavolley::dv_court()],  [datavolley::ggcourt()]
#'
#' @examples
#' if (interactive()) {
#' image_file = system.file("extdata/2019_03_01-KATS-BEDS-court.jpg", package = "ovideo")
#' court_df = ov_detect_court(image_file = image_file)
#'
#'
#' image_file <- ov_video_frame("~/Documents/Donnees/VolleyBall/Dropbox/server_videos/AVL/Men/2022/20221022M_CH_vs_TE.m4v", t=800) 
#' plot(image_read(image_file))
#' court_df = ov_detect_court(image_file = image_file)
#' idxBS = which.min(do.call(c,lapply(court_df, function(x) x$score)))
#' crt_ref_e = court_df[[idxBS]]$court_ref
#' estimated_lines <- crt_ref_e %>% tidyr::pivot_wider(names_from = 'side', values_from = c("x", "y", "court_x", "court_y"))
#' plot(image_read(image_file))
#' polygon(crt_ref_e$x[c(1,2,10,9)], crt_ref_e$y[c(1,2,10,9)], density = 25, col = "blue")
#' polygon(crt_ref_e$x[c(3,4,8,7)], crt_ref_e$y[c(3,4,8,7)], density = 50, col = "cyan")
#' segments(x0 = estimated_lines$x_left[3], y0 = estimated_lines$y_left[3], x1 = estimated_lines$x_right[3], y1 = estimated_lines$y_right[3], col = "white")
#'}
#'
#' @export
ov_detect_court <- function(image_file, video_file, t = 60,
                            method = "LSD", score_distance = "colour-based",
                            line_colour = "white", court_colour = "#c17257",
                            image_width = 1280, image_height = 720){
    if (missing(image_file) || is.null(image_file)) {
        image_file <- ov_video_frame(video_file, t)
    }

    x <- image_read(image_file)
    ## reduce detection to where the court is

    if(method == "LSD"){

        mat <- image_data(x, channels = "gray")
        mat <- as.integer(mat, transpose = TRUE)
        mat <- drop(mat)
        linesegments <- image_line_segment_detector(mat, union = TRUE)

        # plot(x)
        # plot(linesegments, add =TRUE, col ="red")

        LS <- as.data.frame(linesegments$lines)
        LS <- LS %>% dplyr::mutate(Length = sqrt((x1 - x2)^2 + (y1-y2)^2)) %>%
            dplyr::filter(abs(y1 - y2) < 150, width < 25) %>%
            dplyr::mutate(xs = dplyr::case_when(x1 > x2 ~ x2,
                                                TRUE ~ x1),
                          ys = dplyr::case_when(x1 > x2 ~ y2,
                                                TRUE ~ y1),
                          xe = dplyr::case_when(x1 > x2 ~ x1,
                                                TRUE ~ x2),
                          ye = dplyr::case_when(x1 > x2 ~ y1,
                                                TRUE ~ y2),
                          a = abs((ye - ys) / (xe - xs))) %>% dplyr::filter(a < 0.1) %>%
            dplyr::top_n(n = 4, wt = .data$Length)
    }

    if(method == "Hough"){

        img <- magick2cimg(x)

        ht = imager::hough_line(img, data.frame = TRUE, ntheta = 1000)
        plot(img)
        temp <- subset(ht, score > quantile(score, .99995))
        nfline(temp$theta, temp$rho, col = "red")

        ht_h <- ht %>% dplyr::mutate(a = -cos(theta) / sin(theta)) %>%
            dplyr::filter(between(a, -0.5,0.5))%>%
            dplyr::filter(score > quantile(score, .9995))  %>% arrange(-.data$score)

        ht_v <- ht %>% dplyr::mutate(a = -cos(theta) / sin(theta)) %>%
            dplyr::filter(between(theta, -0.75, 0.75) | between(theta,pi -0.75,pi+ 0.75)) %>%
            dplyr::filter(score > quantile(score, .9995)) %>% arrange(-.data$score)

        nfline(ht_h$theta, ht_h$rho, col = "blue")
        nfline(ht_v$theta, ht_v$rho, col = "green")

        htc_v <- ov_cluster(ht_v)%>% top_n(n = 3, wt = .data$score)
        htc_h <- ov_cluster(ht_h) %>% top_n(n = 3, wt = .data$score)
        # plot(x)
        # plot(linesegments, add =TRUE, col ="red")
        nfline(htc_h$theta, htc_h$rho, col = "cyan")
        nfline(htc_v$theta, htc_v$rho, col = "darkgreen")

        # NOW NEED TO CALCULATE CROSSLINE PT and get LS out of that. TODO

        LS = NULL # PLACEHOLDER

        LS <- LS %>% dplyr::mutate(Length = sqrt((x1 - x2)^2 + (y1-y2)^2)) %>% 
            dplyr::filter(abs(y1 - y2) < 150, width < 25) %>%
            dplyr::mutate(xs = dplyr::case_when(x1 > x2 ~ x2,
                                                TRUE ~ x1), 
                          ys = dplyr::case_when(x1 > x2 ~ y2,
                                                TRUE ~ y1), 
                          xe = dplyr::case_when(x1 > x2 ~ x1,
                                                TRUE ~ x2), 
                          ye = dplyr::case_when(x1 > x2 ~ y1,
                                                TRUE ~ y2), 
                          a = abs((ye - ys) / (xe - xs))) %>% dplyr::filter(a < 0.1) %>%
            dplyr::top_n(n = 4, wt = .data$Length)
    }
    #segments(x0 = LS$xs, y0 = LS$ys, x1 = LS$xe, y1 =LS$ye, col = "blue")

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
        matrix(c(-x[1],-x[2], -1, 0, 0, 0, x[1]*X[1], x[2]*X[1], X[1],0,0, 0, -x[1],-x[2], -1, x[1]*X[2], x[2]*X[2], X[2]), nrow = 2, byrow = TRUE)
    }

    # xr = image_raster(x, frame = 1, tidy = TRUE) %>% dplyr::filter(x %in% seq.int(1,1280,1), 
    #                                                                y %in% seq.int(1,720,1))
    # ggplot(xr, aes(x=x, y=rev(y), fill = col)) + 
    #     geom_tile(width = 1, height =1) + theme(legend.position = "none") + 
    #     scale_fill_identity()

    alpha = seq(0,1,0.1)

    court_df = list()
    for(i in seq_len(nrow(LS) - 1)){
        for(j in seq(i, nrow(LS))){
            if(j == i) next
            if(abs(LS$ys[i] - LS$ys[j]) < 150) next

            # FULL COURT
            for(cc in c("full court", "full 3m", "full half", "full 3m c")){
                if(cc == "full court") ref = crt_ref[c(1,2,10,9),]
                if(cc == "full 3m") ref = crt_ref[c(1,2,8,7),]
                if(cc == "full half") ref = crt_ref[c(1,2,6,5),]
                if(cc == "full 3m c") ref = crt_ref[c(1,2,4,3),]

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
            estimated_lines <- crt_ref_e %>% tidyr::pivot_wider(names_from = "side", values_from = c("x", "y", "court_x", "court_y"))

            # plot(x)
            # polygon(crt_ref_e$x[c(1,2,10,9)], crt_ref_e$y[c(1,2,10,9)], density = 25)
            # polygon(crt_ref_e$x[c(3,4,8,7)], crt_ref_e$y[c(3,4,8,7)], density = 50)
            # segments(x0 = estimated_lines$x_left[3], y0 = estimated_lines$y_left[3], x1 = estimated_lines$x_right[3], y1 = estimated_lines$y_right[3], col = "white")

            if(score_distance %in% c("colour-based", "color-based")) {
                px = do.call(c,lapply(seq_len(nrow(estimated_lines)), function(jjj) floor(estimated_lines$x_left[jjj]*alpha + estimated_lines$x_right[jjj] * (1-alpha))))
                py = do.call(c,lapply(seq_len(nrow(estimated_lines)), function(jjj) floor(estimated_lines$y_left[jjj]*alpha + estimated_lines$y_right[jjj] * (1-alpha))))

                dd = cbind(px,py)[which(px %in% seq_len(image_width) & py %in% seq_len(image_height)), ]

                pc = apply(dd, 1,function(x) xr$col[which(xr$x == x[1] & xr$y == x[2])])

                #dd = data.frame(x=px, y=py, col = pc)

                #ggplot(dd, aes(x=x, y=rev(y), fill = col)) + 
                #    geom_tile(width = 10, height =10) + theme(legend.position = "none") + 
                #    scale_fill_identity() + coord_fixed() + theme_void()

                rc <- farver::decode_colour(pc)
                hc <- farver::decode_colour(line_colour)

                score = sum(farver::compare_colour(rc, hc, "rgb", method = "cie2000"))
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

            tmp = list(score = score, H = H, ref = ref, court_ref = crt_ref_e)
            court_df = append(court_df, list(tmp))
            }
        }
    }

    return(court_df)
}

ov_cluster <- function(lines, d_a = 5, d_b = 50){
    C = 1
    L = list()
    for(i in seq_len(nrow(lines))){
        if(i == 1){
            L[1] <- i
            data_cl = data.frame(cluster = 1, a = lines$a[i], 
                                 theta = lines$theta[i], rho = lines$rho[i], 
                                 score = lines$score[i])
            next
        }
        clustered  =FALSE
        for(j in seq_len(C)){
            da = abs(data_cl$theta[j] - lines$theta[i])
            db = abs(data_cl$rho[j] - lines$rho[i])
            if(da < d_a & db < d_b){
                clustered = TRUE
                L[[j]] <- c(L[[j]], i)
                data_cl$score[j] = data_cl$score[j] + lines$score[i]
                break
            }
        }
        if(!clustered){
            C <- C + 1
            data_cl = dplyr::bind_rows(data_cl, 
                                       data.frame(cluster = C, 
                                                  a = lines$a[i], 
                                                  theta = lines$theta[i], rho = lines$rho[i], 
                                                  score = lines$score[i]))
            L[C] <- i
        }
    }
    return(data_cl)
}
