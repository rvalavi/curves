test_that("interactive_map_curves returns a shiny app object", {
    skip_if_not_installed("terra")
    skip_if_not_installed("shiny")

    r <- terra::rast(
        ncols = 8,
        nrows = 8,
        nlyrs = 2,
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 1
    )
    values <- cbind(
        rep(seq(0, 1, length.out = 8), each = 8),
        rep(seq(0, 1, length.out = 8), times = 8)
    )
    terra::values(r) <- values
    names(r) <- c("x1", "x2")

    dat <- terra::as.data.frame(r)
    dat$y <- 1 + 2 * dat$x1 - dat$x2
    model <- lm(y ~ x1 + x2, data = dat)

    pred_map <- r[[1]]
    terra::values(pred_map) <- dat$y
    names(pred_map) <- "prediction"

    app <- interactive_map_curves(
        model,
        map = pred_map,
        predictors = r,
        launch = FALSE
    )

    expect_s3_class(app, "shiny.appobj")
})


test_that("interactive helpers resolve clicked raster values", {
    skip_if_not_installed("terra")

    r <- terra::rast(
        ncols = 5,
        nrows = 5,
        nlyrs = 2,
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 1
    )
    values <- cbind(
        rep(seq(1, 5), each = 5),
        rep(seq(10, 50, by = 10), times = 5)
    )
    terra::values(r) <- values
    names(r) <- c("x1", "x2")

    pred_map <- r[[1]]
    terra::values(pred_map) <- rowSums(values)
    names(pred_map) <- "prediction"

    xy <- terra::xyFromCell(r, 13)
    selected <- curves:::resolve_map_selection(
        map = pred_map,
        predictors = r,
        x_coord = xy[1, 1],
        y_coord = xy[1, 2]
    )

    expect_equal(selected$cell, 13L)
    expect_equal(selected$prediction, sum(values[13, ]))
    expect_equal(selected$values$x1, values[13, 1])
    expect_equal(selected$values$x2, values[13, 2])
})


test_that("interactive_map_curves checks predictor layers against plotted variables", {
    skip_if_not_installed("terra")
    skip_if_not_installed("shiny")

    predictors <- terra::rast(
        ncols = 6,
        nrows = 6,
        nlyrs = 2,
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 1
    )
    terra::values(predictors) <- cbind(
        rep(seq(0, 1, length.out = 6), each = 6),
        rep(seq(0, 1, length.out = 6), times = 6)
    )
    names(predictors) <- c("x1", "x2")

    dat <- terra::as.data.frame(predictors)
    dat$y <- dat$x1 + dat$x2
    model <- lm(y ~ x1 + x2, data = dat)

    pred_map <- predictors[[1]]
    terra::values(pred_map) <- dat$y
    names(pred_map) <- "prediction"

    incomplete_predictors <- predictors[[1]]
    names(incomplete_predictors) <- "x1"

    expect_error(
        interactive_map_curves(
            model,
            map = pred_map,
            predictors = incomplete_predictors,
            predict_data = dat[, c("x1", "x2")],
            launch = FALSE
        ),
        "Missing layers: x2"
    )
})
