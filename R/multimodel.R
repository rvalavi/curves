#' Creating a multimodel response curve plot
#'
#' This function generates response curves for several models by varying one
#' predictor at a time while keeping others constant.
#'
#' @param models A list object of fitted models that support prediction.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param fun A function used to generate predictions from the model. Defaults
#'   to `predict`.
#' @param ... Additional arguments passed to `fun`.
#' @param n Integer, number of points to sample for each numeric predictor
#'   variable (default: 100).
#' @param ylab Character, label for the y-axis (default: `"Prediction"`).
#' @param nrows Integer, number of rows in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param ncols Integer, number of columns in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param rug Logical, whether to include a rug plot along the x-axis (default:
#'   `TRUE`).
#' @param ylim Numeric vector of length 2, specifying the limits of the y-axis.
#'   If `NULL`, limits are automatically set.
#' @param color Character, colour of the response curve (default:
#'   `"deepskyblue2"`).
#' @param se Standard deviation ribbon for the averaged curve.
#' @param se_color Fill colour of the standard deviation ribbon.
#' @param response Optional column name or index to select when `fun` returns
#'   multiple predictions per row. If `NULL` and exactly two prediction columns
#'   are returned, the second column is used.
#'
#' @return A `ggplot2` object containing the response curves arranged in a grid.
#'
#' @export
#'
#' @examples
#' data(iris)
#' models <- list(
#'   lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris),
#'   lm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris)
#' )
#' response_plot <- multimodel(
#'   models,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
#' )
#' print(response_plot)
multimodel <- function(models, x = NULL, predict_data = NULL,
                       fun = stats::predict, ..., n = 100, ylab = "Prediction",
                       se = TRUE,
                       rug = TRUE, ylim = NULL,
                       color = "deepskyblue2",
                       se_color = "grey80",
                       response = NULL,
                       nrows = NULL, ncols = NULL) {

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
        x_source <- x
    } else {
        x_source <- predict_data
    }

    if (!length(models)) {
        stop("models must contain at least one fitted model")
    }

    x_df <- validate_predictors(x_source, sample_size = 5000L)
    nms <- names(x_df)
    nvars <- ncol(x_df)
    nmod <- length(models)

    ncols <- if (is.null(ncols)) ceiling(sqrt(nvars)) else ncols
    nrows <- if (is.null(nrows)) ceiling(nvars / ncols) else nrows

    reference_row <- build_reference_row(x_df)
    predictor_specs <- lapply(nms, function(name) {
        list(
            name = name,
            is_factor = is.factor(x_df[[name]]),
            values = curve_values(x_df[[name]], n = n)
        )
    })
    names(predictor_specs) <- nms

    tables <- lapply(predictor_specs, function(spec) {
        grid <- build_curve_grid(reference_row, spec$name, spec$values)
        mat <- vapply(
            models,
            function(model) extract_prediction_vector(
                fun(model, grid, ...),
                n = nrow(grid),
                response = response
            ),
            numeric(nrow(grid))
        )

        std <- if (nmod > 1L) {
            apply(mat, 1, stats::sd)
        } else {
            rep(0, nrow(grid))
        }

        data.frame(
            x = grid[[spec$name]],
            y = rowMeans(mat),
            std = std
        )
    })

    limits <- if (is.null(ylim)) {
        curve_limits(unlist(lapply(tables, function(table) {
            if (se) {
                c(table$y - table$std, table$y + table$std)
            } else {
                table$y
            }
        })))
    } else {
        ylim
    }

    plots <- lapply(predictor_specs, function(spec) {
        plot_1D(
            df = tables[[spec$name]],
            dat = if (rug && !spec$is_factor) {
                sample_rug_values(x_df, spec$name)
            } else {
                NULL
            },
            fact = spec$is_factor,
            rug = rug && !spec$is_factor,
            se = se,
            x_name = spec$name,
            y_name = ylab,
            color = color,
            ribcol = se_color,
            ylim = limits
        )
    })

    cowplot::plot_grid(plotlist = plots, nrow = nrows, ncol = ncols)
}
