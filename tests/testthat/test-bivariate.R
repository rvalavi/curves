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


test_that("bivariate heatmaps use viridis and omit contour overlays", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c("Sepal.Width", "Petal.Length")
    )

    expect_identical(formals(bivariate)$palette, "viridis")
    expect_length(plot$layers, 1L)
    expect_s3_class(plot$layers[[1]]$geom, "GeomRaster")
    expect_null(plot$labels$title)
})


test_that("bivariate supports pdp surfaces and optional rugs", {
    set.seed(123)
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c("Sepal.Width", "Petal.Length"),
        method = "pdp",
        background_n = 40,
        rug = TRUE
    )

    expect_s3_class(plot, "ggplot")
    expect_length(plot$layers, 2L)
    expect_s3_class(plot$layers[[2]]$geom, "GeomRug")
})


test_that("bivariate pdp helper averages over background rows", {
    background_rows <- data.frame(
        x1 = c(0, 1, 2),
        x2 = c(10, 20, 30),
        x3 = c(5, 7, 9)
    )
    spec <- list(
        x_name = "x1",
        y_name = "x2",
        x_values = c(0, 2),
        y_values = c(10, 30)
    )

    table <- curves:::build_pdp_surface_table(
        model = NULL,
        background_rows = background_rows,
        spec = spec,
        fun = function(model, newdata) {
            newdata$x1 + 2 * newdata$x2 + newdata$x3
        },
        response = NULL
    )

    expect_equal(
        table$z,
        c(27, 29, 67, 69)
    )
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


test_that("bivariate ale helper returns zero surface for additive models", {
    predictors <- expand.grid(
        x1 = seq(0, 1, length.out = 8),
        x2 = seq(1, 2, length.out = 7)
    )
    predictors$x3 <- seq_len(nrow(predictors)) / 10

    spec <- curves:::build_pair_specs(
        predictors,
        pairs = c("x1", "x2"),
        n = 5,
        method = "ale"
    )[[1]]

    table <- curves:::build_ale_surface_table(
        model = NULL,
        ale_rows = predictors,
        spec = spec,
        fun = function(model, newdata) {
            3 * newdata$x1 - 2 * newdata$x2 + newdata$x3
        },
        response = NULL
    )

    expect_true(max(abs(table$z)) < 1e-10)
})


test_that("bivariate ale heatmaps use cell bounds without raster warnings", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c("Sepal.Width", "Petal.Length"),
        method = "ale",
        plot_type = "heatmap",
        n = 8
    )

    expect_s3_class(plot$layers[[1]]$geom, "GeomRect")
    expect_no_warning(ggplot2::ggplot_build(plot))
})


test_that("bivariate ale filters to numeric predictor pairs", {
    model <- lm(
        Sepal.Length ~ Species + Sepal.Width + Petal.Length,
        data = iris
    )

    expect_warning(
        plot <- bivariate(
            model,
            x = iris[, c("Species", "Sepal.Width", "Petal.Length")],
            pairs = list(
                c("Species", "Sepal.Width"),
                c("Sepal.Width", "Petal.Length")
            ),
            method = "ale",
            n = 8
        ),
        "numeric predictor pairs only"
    )

    expect_s3_class(plot, "ggplot")
})


test_that("bivariate handles binary probability output", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c("Sepal.Width", "Petal.Length"),
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
})


test_that("bivariate ale requires at least one numeric pair", {
    model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

    expect_error(
        suppressWarnings(
            bivariate(
                model,
                x = iris[, c("Species", "Petal.Width")],
                pairs = c("Species", "Petal.Width"),
                method = "ale"
            )
        ),
        "ALE requires at least one numeric predictor pair"
    )
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


test_that("bivariate can return an ale surface plotly widget", {
    skip_if_not_installed("plotly")

    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    plot <- bivariate(
        model,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        pairs = c("Sepal.Width", "Petal.Length"),
        plot_type = "surface",
        method = "ale",
        n = 8
    )

    expect_s3_class(plot, "plotly")
})
