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


test_that("univariate supports pdp and ice-based methods", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    predictors <- iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]

    expect_s3_class(
        univariate(model, x = predictors, method = "pdp", n = 20),
        "ggplot"
    )
    expect_s3_class(
        univariate(model, x = predictors, method = "ice", n = 20),
        "ggplot"
    )
    expect_s3_class(
        univariate(model, x = predictors, method = "ice+pdp", n = 20),
        "ggplot"
    )
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


test_that("ice helpers use sampled predictor rows and average correctly", {
    x_df <- data.frame(
        x1 = c(1, 2, 3, 4, 5),
        x2 = c(10, 20, 30, 40, NA)
    )

    sampled <- curves:::sample_background_rows(x_df, n = 3)
    expect_equal(nrow(sampled), 3)
    expect_false(anyNA(sampled))

    curves_df <- curves:::build_ice_curve_table(
        model = NULL,
        background_rows = sampled,
        column = "x1",
        values = c(0, 5),
        fun = function(model, newdata) newdata$x1 + newdata$x2,
        response = NULL
    )

    expect_equal(nrow(curves_df), 6)
    expect_equal(length(unique(curves_df$curve)), 3)

    summary_df <- curves:::average_curve_table(curves_df)
    expect_equal(summary_df$y, c(0, 5) + mean(sampled$x2))
})


test_that("univariate defaults to the second column for binary probability output", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- univariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        n = 25,
        fun = function(model, newdata) {
            prob <- stats::plogis(
                0.4 * newdata$Sepal.Width -
                    0.3 * newdata$Petal.Length +
                    0.2 * newdata$Petal.Width
            )
            cbind(absent = 1 - prob, present = prob)
        }
    )

    expect_s3_class(plot, "ggplot")
    expect_equal(
        curves:::extract_prediction_vector(
            cbind(absent = c(0.8, 0.3), present = c(0.2, 0.7)),
            n = 2
        ),
        c(0.2, 0.7)
    )
})


test_that("univariate can select a named prediction column", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- univariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        n = 25,
        response = "absent",
        fun = function(model, newdata) {
            prob <- stats::plogis(
                0.4 * newdata$Sepal.Width -
                    0.3 * newdata$Petal.Length +
                    0.2 * newdata$Petal.Width
            )
            data.frame(absent = 1 - prob, present = prob)
        }
    )

    expect_s3_class(plot, "ggplot")
    expect_equal(
        curves:::extract_prediction_vector(
            data.frame(absent = c(0.8, 0.3), present = c(0.2, 0.7)),
            n = 2,
            response = "absent"
        ),
        c(0.8, 0.3)
    )
})


test_that("univariate errors on ambiguous multi-column prediction output", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    expect_error(
        univariate(
            model,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            fun = function(model, newdata) {
                data.frame(a = seq_len(nrow(newdata)),
                           b = seq_len(nrow(newdata)) + 1,
                           c = seq_len(nrow(newdata)) + 2)
            }
        ),
        "Supply `response`"
    )
})
