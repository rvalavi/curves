#' Creating a multimodel response curve plot
#'
#' This function generates profile, partial dependence, or ALE response curves
#' for several models by varying one predictor at a time and aggregating the
#' fitted curves across models.
#'
#' @param models A list object of fitted models that support prediction.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param fun A function used to generate predictions from the model, or a list
#'   of functions the same length as `models`. Defaults to `predict`.
#' @param ... Additional arguments passed to each prediction function. For
#'   mixed model types with different prediction interfaces, prefer supplying
#'   model-specific wrappers through `fun`.
#' @param method Character, the curve type to plot. `"profile"` uses a single
#'   reference profile, `"pdp"` averages over sampled predictor rows before
#'   aggregating across models, and `"ale"` draws accumulated local effects
#'   curves for numeric predictors and ignores factor predictors with a warning.
#' @param n Integer, number of points to sample for each numeric predictor
#'   variable (default: 100). For `"ale"`, `n` sets the maximum number of
#'   intervals used to estimate local effects for numeric predictors.
#' @param background_n Integer, number of randomly sampled background rows used
#'   for `"pdp"` (default: `n`).
#' @param agg Function used to aggregate model-specific predictions at each
#'   point along the curve. Defaults to `mean`.
#' @param weights Optional numeric vector of model weights with the same length
#'   as `models`.
#' @param interval Character, interval type drawn around the aggregated curve.
#'   `"sd"` draws a standard deviation ribbon, `"quantile"` draws a central
#'   quantile ribbon using `interval_level`, and `"none"` disables the ribbon.
#' @param interval_level Numeric in `(0, 1)` giving the central quantile width
#'   used when `interval = "quantile"`. Ignored otherwise.
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
#'   `"deepskyblue4"`).
#' @param response Optional column name or index to select when `fun` returns
#'   multiple predictions per row. If `NULL` and exactly two prediction columns
#'   are returned, the second column is used.
#' @param show_models Logical, whether to overlay individual model curves
#'   beneath the aggregated curve (default: `FALSE`).
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
                       fun = stats::predict, ...,
                       method = c("profile", "pdp", "ale"),
                       n = 100,
                       background_n = n,
                       agg = mean,
                       weights = NULL,
                       interval = c("sd", "none", "quantile"),
                       interval_level = 0.8,
                       ylab = "Prediction",
                       rug = TRUE, ylim = NULL,
                       color = "deepskyblue4",
                       response = NULL,
                       nrows = NULL, ncols = NULL,
                       show_models = FALSE) {

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

    method <- match.arg(method)
    interval <- match.arg(interval)
    n <- validate_curve_n(n)
    background_n <- validate_background_n(background_n)

    if (!is.logical(show_models) || length(show_models) != 1L ||
        is.na(show_models)) {
        stop("show_models must be TRUE or FALSE")
    }

    nmod <- length(models)
    funs <- normalize_multimodel_funs(fun, n_models = nmod)
    agg <- validate_multimodel_agg(agg)
    weights <- validate_multimodel_weights(weights, n_models = nmod)

    if (interval == "quantile") {
        interval_level <- validate_interval_level(interval_level)
    }

    sample_size <- curve_sample_size(
        x_source,
        n = n,
        background_n = background_n,
        method = method
    )

    x_df <- validate_predictors(x_source, sample_size = sample_size)
    predictor_names <- names(x_df)

    if (method == "ale") {
        factor_predictors <- names(x_df)[vapply(x_df, is.factor, logical(1))]

        if (length(factor_predictors)) {
            warning(
                "ALE currently supports numeric predictors only. Ignoring factor predictors: ",
                paste(factor_predictors, collapse = ", "),
                call. = FALSE
            )
        }

        predictor_names <- names(x_df)[vapply(x_df, is.numeric, logical(1))]

        if (!length(predictor_names)) {
            stop("ALE requires at least one numeric predictor to plot")
        }
    }

    nvars <- length(predictor_names)

    ncols <- if (is.null(ncols)) ceiling(sqrt(nvars)) else ncols
    nrows <- if (is.null(nrows)) ceiling(nvars / ncols) else nrows

    reference_row <- if (method == "profile") build_reference_row(x_df) else NULL
    background_rows <- if (method == "pdp") {
        sample_background_rows(x_df, background_n = background_n)
    } else {
        NULL
    }
    ale_rows <- if (method == "ale") {
        complete_predictor_rows(x_df, context = "ALE methods")
    } else {
        NULL
    }

    predictor_specs <- lapply(predictor_names, function(name) {
        list(
            name = name,
            is_factor = is.factor(x_df[[name]]),
            is_ordered = is.ordered(x_df[[name]]),
            values = curve_values(x_df[[name]], n = n)
        )
    })
    names(predictor_specs) <- predictor_names

    tables <- lapply(predictor_specs, function(spec) {
        model_curves <- build_multimodel_curve_matrix(
            models = models,
            funs = funs,
            method = method,
            spec = spec,
            reference_row = reference_row,
            background_rows = background_rows,
            ale_rows = ale_rows,
            n = n,
            response = response,
            ...
        )
        summary_df <- summarize_multimodel_predictions(
            x = model_curves$x,
            mat = model_curves$mat,
            agg = agg,
            weights = weights,
            interval = interval,
            interval_level = interval_level
        )

        if (show_models) {
            list(
                curves = multimodel_curve_df(
                    x = model_curves$x,
                    mat = model_curves$mat
                ),
                summary = summary_df
            )
        } else {
            list(curves = summary_df, summary = NULL)
        }
    })
    names(tables) <- predictor_names

    limits <- if (is.null(ylim)) {
        curve_limits(unlist(lapply(tables, function(table) {
            values <- table$curves$y
            if (all(c("ymin", "ymax") %in% names(table$curves))) {
                values <- c(values, table$curves$ymin, table$curves$ymax)
            }

            if (!is.null(table$summary)) {
                values <- c(values, table$summary$y)
                if (all(c("ymin", "ymax") %in% names(table$summary))) {
                    values <- c(values, table$summary$ymin, table$summary$ymax)
                }
            }

            values
        })))
    } else {
        ylim
    }

    plots <- lapply(predictor_specs, function(spec) {
        table <- tables[[spec$name]]
        plot_1D(
            df = table$curves,
            dat = if (rug && !spec$is_factor) {
                sample_rug_values(x_df, spec$name)
            } else {
                NULL
            },
            fact = spec$is_factor,
            ordered_factor = spec$is_ordered,
            rug = rug && !spec$is_factor,
            se = FALSE,
            x_name = spec$name,
            y_name = ylab,
            color = color,
            ribcol = "grey80",
            ylim = limits,
            curve_alpha = if (show_models) 0.2 else 1,
            curve_linewidth = if (show_models) 0.35 else 0.7,
            summary_df = table$summary,
            summary_linewidth = 1
        )
    })

    cowplot::plot_grid(plotlist = plots, nrow = nrows, ncol = ncols)
}


validate_multimodel_agg <- function(agg) {
    if (!is.function(agg)) {
        stop("agg must be a function")
    }

    agg
}


validate_multimodel_weights <- function(weights, n_models) {
    if (is.null(weights)) {
        return(NULL)
    }

    if (!is.numeric(weights) || length(weights) != n_models ||
        anyNA(weights) || any(!is.finite(weights)) || any(weights < 0) ||
        !any(weights > 0)) {
        stop(
            "weights must be NULL or a numeric vector the same length as models ",
            "with at least one positive value"
        )
    }

    as.numeric(weights)
}


build_multimodel_curve_matrix <- function(models, funs, method, spec,
                                          reference_row, background_rows,
                                          ale_rows, n, response, ...) {
    curve_tables <- lapply(seq_along(models), function(index) {
        if (method == "profile") {
            return(build_profile_curve_table(
                model = models[[index]],
                reference_row = reference_row,
                column = spec$name,
                values = spec$values,
                fun = funs[[index]],
                response = response,
                ...
            ))
        }

        if (method == "pdp") {
            curve_df <- build_ice_curve_table(
                model = models[[index]],
                background_rows = background_rows,
                column = spec$name,
                values = spec$values,
                fun = funs[[index]],
                response = response,
                ...
            )

            return(average_curve_table(curve_df))
        }

        build_ale_curve_table(
            model = models[[index]],
            ale_rows = ale_rows,
            column = spec$name,
            n = n,
            fun = funs[[index]],
            response = response,
            ...
        )
    })

    x_values <- curve_tables[[1]]$x

    if (length(curve_tables) > 1L) {
        valid_x <- vapply(curve_tables[-1L], function(table) {
            identical(table$x, x_values)
        }, logical(1))

        if (!all(valid_x)) {
            stop("All multimodel curves must share the same x values")
        }
    }

    mat <- vapply(curve_tables, function(table) table$y, numeric(length(x_values)))

    if (is.null(dim(mat))) {
        mat <- matrix(mat, ncol = 1L)
    }

    list(x = x_values, mat = mat)
}


summarize_multimodel_predictions <- function(x, mat, agg, weights = NULL,
                                             interval = "sd",
                                             interval_level = 0.8) {
    y <- apply(mat, 1L, aggregate_multimodel_values, agg = agg, weights = weights)

    summary_df <- data.frame(x = x, y = y)

    if (interval == "sd") {
        spread <- apply(mat, 1L, multimodel_sd, weights = weights)
        summary_df$ymin <- y - spread
        summary_df$ymax <- y + spread
    } else if (interval == "quantile") {
        bounds <- t(apply(
            mat,
            1L,
            multimodel_quantile_bounds,
            weights = weights,
            interval_level = interval_level
        ))
        summary_df$ymin <- bounds[, 1]
        summary_df$ymax <- bounds[, 2]
    }

    if (is.factor(x)) {
        summary_df$x <- factor(
            as.character(summary_df$x),
            levels = levels(x),
            ordered = is.ordered(x)
        )
    }

    summary_df
}


aggregate_multimodel_values <- function(values, agg, weights = NULL) {
    result <- if (is.null(weights)) {
        agg(values)
    } else if (identical(agg, mean) || identical(agg, base::mean)) {
        stats::weighted.mean(values, w = weights)
    } else {
        formal_names <- names(formals(agg))

        if ("weights" %in% formal_names || "..." %in% formal_names) {
            agg(values, weights = weights)
        } else if ("w" %in% formal_names) {
            agg(values, w = weights)
        } else {
            stop(
                "When weights are supplied, agg must be mean or accept a ",
                "`weights` or `w` argument"
            )
        }
    }

    if (!is.numeric(result) || length(result) != 1L || is.na(result) ||
        !is.finite(result)) {
        stop("agg must return a single finite numeric value")
    }

    as.numeric(result)
}


multimodel_sd <- function(values, weights = NULL) {
    if (length(values) <= 1L) {
        return(0)
    }

    if (is.null(weights)) {
        return(stats::sd(values))
    }

    center <- stats::weighted.mean(values, w = weights)
    sqrt(sum((values - center)^2 * weights) / sum(weights))
}


multimodel_quantile_bounds <- function(values, weights = NULL, interval_level) {
    probs <- c((1 - interval_level) / 2, 1 - ((1 - interval_level) / 2))

    if (is.null(weights)) {
        return(stats::quantile(values, probs = probs, names = FALSE))
    }

    weighted_quantile(values, weights = weights, probs = probs)
}


weighted_quantile <- function(x, weights, probs) {
    order_index <- order(x)
    x <- x[order_index]
    weights <- weights[order_index]

    keep <- weights > 0
    x <- x[keep]
    weights <- weights[keep]

    cumulative_weights <- cumsum(weights) / sum(weights)

    as.numeric(stats::approx(
        x = c(0, cumulative_weights),
        y = c(x[1L], x),
        xout = probs,
        method = "linear",
        ties = "ordered",
        rule = 2
    )$y)
}


multimodel_curve_df <- function(x, mat) {
    data.frame(
        curve = rep(seq_len(ncol(mat)), each = length(x)),
        x = if (is.factor(x)) {
            factor(
                rep(as.character(x), times = ncol(mat)),
                levels = levels(x),
                ordered = is.ordered(x)
            )
        } else {
            rep(x, times = ncol(mat))
        },
        y = as.vector(mat)
    )
}
