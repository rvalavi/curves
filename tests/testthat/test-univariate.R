test_that("univariate works for numeric predictors without attached plotting packages", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- univariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
    )

    expect_s3_class(plot, "ggplot")
})


test_that("univariate handles factor predictors", {
    model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

    plot <- univariate(
        model,
        x = iris[, c("Species", "Petal.Width")]
    )

    expect_s3_class(plot, "ggplot")
})


test_that("univariate supports SpatRaster inputs", {
    skip_if_not_installed("terra")

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

    predictors <- terra::as.data.frame(r)
    response <- 1 + 2 * predictors$x1 - predictors$x2
    model <- lm(response ~ x1 + x2, data = predictors)

    plot <- univariate(model, x = r)

    expect_s3_class(plot, "ggplot")
})
