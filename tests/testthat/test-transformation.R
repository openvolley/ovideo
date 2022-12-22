context("Transformation functions")
test_that("transformations work as expected", {
    refpts1 <- dplyr::tribble(~image_x, ~image_y, ~court_x, ~court_y, ~z,
                              0.0533,   0.0326,      3.5,      6.5,  0,
                              0.974,   0.0572,      0.5,      6.5,  0,
                              0.683,    0.566,      0.5,      0.5,  0,
                              0.283,    0.560,      3.5,      0.5,  0,
                              0.214,    0.401,      3.5,      3.5,  0,
                              0.776,    0.412,      0.5,      3.5,  0,
                              0.780,    0.680,      0.5,      3.5,  2.43,
                              0.206,    0.670,      3.5,      3.5,  2.43)

    C1 <- ov_cmat_estimate(x = refpts1[, c("image_x", "image_y")],
                           X = refpts1[, c("court_x", "court_y", "z")])

    refpts2 <- dplyr::tribble(~image_x, ~image_y, ~court_x, ~court_y, ~z,
                              0.045,   0.0978,      0.5,      0.5,  0,
                              0.963,   0.0920,      3.5,      0.5,  0,
                              0.753,    0.617,      3.5,      6.5,  0,
                              0.352,    0.609,      0.5,      6.5,  0,
                              0.255,    0.450,      0.5,      3.5,  0,
                              0.817,    0.456,      3.5,      3.5,  0,
                              0.821,    0.731,      3.5,      3.5,  2.43,
                              0.246,    0.720,      0.5,      3.5,  2.43)
    C2 <- ov_cmat_estimate(x = refpts2[, c("image_x", "image_y")],
                           X = refpts2[, c("court_x", "court_y", "z")])

    xyz <- matrix(c(2.85, 1.4, 2.90), ncol = 3)
    uv1 <- ov_cmat_apply(C1, xyz) ## object position in image 1
    uv2 <- ov_cmat_apply(C2, xyz) ## object position in image 1

    ## if our measurements are perfect (no noise), the "dlt" method will reconstruct xyz exactly:
    expect_equal(as.numeric(xyz), ov_3dpos_multicamera(rbind(uv1, uv2), list(C1, C2), method = "dlt")$xyz)
    expect_equal(as.numeric(xyz), ov_3dpos_multicamera(rbind(uv1, uv2), list(C1, C2), method = "nls")$xyz)
})
