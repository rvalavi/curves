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


test_that("univariate resolves default predict from the caller context", {
    predictor_data <- data.frame(x1 = c(0, 1, 2))
    model <- structure(list(offset = 10), class = "dummy_predict_model")

    predict <- function(object, newdata, ...) {
        newdata$x1 + object$offset
    }

    fun <- curves:::resolve_predict_fun(NULL, env = environment())
    expect_equal(fun(model, predictor_data), c(10, 11, 12))
})


test_that("univariate supports pdp and ice-based methods", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    predictors <- iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]

    expect_s3_class(
        univariate(
            model,
            x = predictors,
            method = "pdp",
            n = 20,
            background_n = 15
        ),
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
    expect_s3_class(
        univariate(model, x = predictors, method = "ale", n = 20),
        "ggplot"
    )
})


test_that("univariate uses a method-specific default y-axis label", {
    expect_equal(curves:::default_univariate_ylab("profile"), "Prediction")
    expect_equal(
        curves:::default_univariate_ylab("ice+pdp"),
        "Prediction"
    )
    expect_equal(
        curves:::default_univariate_ylab("ale"),
        "Accumulated local effect"
    )
})


test_that("univariate ale includes factor predictors without warning", {
    model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

    expect_no_warning(
        plot <- univariate(
            model,
            x = iris[, c("Species", "Petal.Width")],
            method = "ale"
        )
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


test_that("ice helpers use sampled predictor rows and average correctly", {
    x_df <- data.frame(
        x1 = c(1, 2, 3, 4, 5),
        x2 = c(10, 20, 30, 40, NA)
    )

    set.seed(123)
    sampled <- curves:::sample_background_rows(x_df, background_n = 3)
    expect_equal(nrow(sampled), 3)
    expect_false(anyNA(sampled))
    expect_false(identical(rownames(sampled), c("1", "2", "3")))

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

    band_df <- curves:::average_curve_table(curves_df, band = 0.5)
    expected_bounds <- stats::quantile(
        sampled$x2,
        probs = c(0.25, 0.75),
        names = FALSE
    )
    expect_equal(band_df$ymin, c(0, 5) + expected_bounds[1])
    expect_equal(band_df$ymax, c(0, 5) + expected_bounds[2])
})


test_that("interactive curve helpers separate grid and background counts", {
    x_df <- data.frame(
        x1 = 1:10,
        x2 = seq(10, 100, by = 10)
    )

    set.seed(456)
    curve_data <- curves:::prepare_interactive_curve_data(
        model = NULL,
        x_source = x_df,
        fun = function(model, newdata) newdata$x1 + newdata$x2,
        n = 3,
        background_n = 4,
        interval = "none",
        interval_level = 0.8,
        ylab = "Prediction",
        rug = FALSE,
        ylim = NULL,
        color = "black",
        response = NULL,
        nrows = NULL,
        ncols = 1,
        method = "ice+pdp"
    )

    table <- curve_data$tables$x1
    expect_equal(length(unique(table$curves$x)), 3)
    expect_equal(length(unique(table$curves$curve)), 4)
})


test_that("interactive curve helpers add a PDP band only for pdp", {
    x_df <- data.frame(
        x1 = 1:10,
        x2 = seq(10, 100, by = 10)
    )

    set.seed(789)
    curve_data <- curves:::prepare_interactive_curve_data(
        model = NULL,
        x_source = x_df,
        fun = function(model, newdata) newdata$x1 + newdata$x2,
        n = 4,
        background_n = 5,
        interval = "quantile",
        interval_level = 0.8,
        ylab = "Prediction",
        rug = FALSE,
        ylim = NULL,
        color = "black",
        response = NULL,
        nrows = NULL,
        ncols = 1,
        method = "pdp"
    )

    expect_true(all(c("ymin", "ymax") %in% names(curve_data$tables$x1$curves)))
})


test_that("ale helper accumulates and centers local effects", {
    x_df <- data.frame(
        x1 = c(1, 2, 3, 4, 5),
        x2 = c(10, 20, 30, 40, 50)
    )

    ale_df <- curves:::build_ale_curve_table(
        model = NULL,
        ale_rows = x_df,
        column = "x1",
        n = 2,
        fun = function(model, newdata) 2 * newdata$x1 + newdata$x2,
        response = NULL
    )

    expect_equal(ale_df$x, c(2, 4))
    expect_equal(ale_df$y, c(-2.4, 1.6))
})


test_that("univariate ale works with only factor predictors", {
    model <- lm(Sepal.Length ~ Species, data = iris)

    expect_no_warning(
        plot <- univariate(
            model,
            x = iris["Species"],
            method = "ale"
        )
    )

    expect_s3_class(plot, "ggplot")
})


test_that("unordered factor summaries are not drawn with connecting lines", {
    plot <- curves:::plot_1D(
        df = data.frame(
            x = factor(c("a", "b")),
            y = c(1, 2)
        ),
        dat = NULL,
        fact = TRUE,
        ordered_factor = FALSE,
        rug = FALSE,
        se = FALSE,
        x_name = "x",
        y_name = "y",
        ylim = c(0, 3),
        color = "black",
        summary_df = data.frame(
            x = factor(c("a", "b")),
            y = c(1, 2)
        )
    )

    geom_classes <- vapply(plot$layers, function(layer) class(layer$geom)[1], character(1))
    expect_false("GeomLine" %in% geom_classes)
})


test_that("ordered factor summaries still use connecting lines", {
    ordered_x <- ordered(c("low", "high"), levels = c("low", "high"))
    plot <- curves:::plot_1D(
        df = data.frame(
            x = ordered_x,
            y = c(1, 2)
        ),
        dat = NULL,
        fact = TRUE,
        ordered_factor = TRUE,
        rug = FALSE,
        se = FALSE,
        x_name = "x",
        y_name = "y",
        ylim = c(0, 3),
        color = "black",
        summary_df = data.frame(
            x = ordered_x,
            y = c(1, 2)
        )
    )

    geom_classes <- vapply(plot$layers, function(layer) class(layer$geom)[1], character(1))
    expect_true("GeomLine" %in% geom_classes)
})


test_that("thin grouped curves can use a separate colour from summaries", {
    plot <- curves:::plot_1D(
        df = data.frame(
            x = c(1, 2, 1, 2),
            y = c(1, 2, 2, 3),
            curve = c(1, 1, 2, 2)
        ),
        dat = NULL,
        fact = FALSE,
        rug = FALSE,
        se = FALSE,
        x_name = "x",
        y_name = "y",
        ylim = c(0, 4),
        color = "deepskyblue4",
        curve_color = "gray70",
        curve_linewidth = 0.35,
        summary_df = data.frame(x = c(1, 2), y = c(1.5, 2.5)),
        summary_linewidth = 1
    )

    expect_equal(plot$layers[[1]]$aes_params$colour, "gray70")
    expect_equal(plot$layers[[1]]$aes_params$linewidth, 0.35)
    expect_equal(plot$layers[[2]]$aes_params$colour, "deepskyblue4")
    expect_equal(plot$layers[[2]]$aes_params$linewidth, 1)
})


test_that("numeric curves draw a ribbon when ymin and ymax are supplied", {
    plot <- curves:::plot_1D(
        df = data.frame(
            x = c(1, 2),
            y = c(3, 4),
            ymin = c(2, 3),
            ymax = c(4, 5)
        ),
        dat = NULL,
        fact = FALSE,
        rug = FALSE,
        se = FALSE,
        x_name = "x",
        y_name = "y",
        ylim = c(1, 6),
        color = "black"
    )

    geom_classes <- vapply(plot$layers, function(layer) class(layer$geom)[1], character(1))
    expect_true("GeomRibbon" %in% geom_classes)
})


test_that("interval is only accepted for pdp and valid quantile widths", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    expect_error(
        univariate(
            model,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            method = "ice",
            interval = "quantile"
        ),
        "only supported when method = \"pdp\""
    )

    expect_error(
        univariate(
            model,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            method = "pdp",
            interval = "quantile",
            interval_level = NA_real_
        ),
        "single number between 0 and 1"
    )
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
