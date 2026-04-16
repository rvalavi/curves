test_that("bivariate creates static plots for numeric predictor pairs", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
    )

    expect_s3_class(plot, "ggplot")
})


test_that("bivariate lets predict type flow through dots", {
    captured_type <- NULL
    predictor_data <- iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]

    predict_with_type <- function(model, newdata, type = NULL) {
        captured_type <<- type
        newdata[[1]] + newdata[[2]]
    }

    plot <- bivariate(
        model = NULL,
        x = predictor_data,
        fun = predict_with_type,
        pairs = c(1, 2),
        type = "response"
    )

    expect_identical(captured_type, "response")
    expect_s3_class(plot, "ggplot")
})


test_that("bivariate handles mixed numeric and factor predictors", {
    model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

    plot <- bivariate(
        model,
        x = iris[, c("Species", "Petal.Width")],
        pairs = c("Species", "Petal.Width")
    )

    expect_s3_class(plot, "ggplot")
})


test_that("bivariate accepts predictor indices in pairs", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c(1, 2)
    )

    expect_s3_class(plot, "ggplot")
})


test_that("bivariate validates contour and surface mode requirements", {
    numeric_model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )
    factor_model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

    expect_error(
        bivariate(
            numeric_model,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            plot_type = "surface"
        ),
        "requires a single predictor pair"
    )

    expect_error(
        bivariate(
            factor_model,
            x = iris[, c("Species", "Petal.Width")],
            pairs = c("Species", "Petal.Width"),
            plot_type = "contour"
        ),
        "only supported for numeric predictor pairs"
    )

    expect_error(
        bivariate(
            factor_model,
            x = iris[, c("Species", "Petal.Width")],
            pairs = c("Species", "Petal.Width"),
            plot_type = "surface"
        ),
        "only supported for numeric predictor pairs"
    )
})


test_that("bivariate supports SpatRaster inputs", {
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

    plot <- bivariate(model, x = r, pairs = c("x1", "x2"))

    expect_s3_class(plot, "ggplot")
})


test_that("bivariate can return a plotly surface", {
    skip_if_not_installed("plotly")

    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c("Sepal.Width", "Petal.Length"),
        plot_type = "surface"
    )

    expect_s3_class(plot, "plotly")
})
