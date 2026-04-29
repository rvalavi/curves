#' Ensemble response curves across fitted models
#'
#' Plot profile, partial dependence, or accumulated local effects curves for
#' several fitted models on a common predictor grid, then combine the
#' model-specific curves into an ensemble curve.
#'
#' @details
#' `multimodel()` is intended for ensemble modelling workflows, such as species
#' distribution models where alternative algorithms or model specifications are
#' fit to the same response and predictor set. Each ensemble member is first
#' converted to the same one-dimensional curve type used by [univariate()]. For
#' model \eqn{r}, write this curve as \eqn{h_r(z)}. The displayed ensemble
#' curve is
#' \deqn{H(z) = A\{h_1(z), \ldots, h_R(z)\},}
#' where \eqn{A} is `agg`. With the default `agg = mean` and no weights, this is
#' the arithmetic ensemble mean. If `weights` are supplied and `agg` is `mean`,
#' the package uses
#' \deqn{H(z) = \frac{\sum_{r=1}^{R} w_r h_r(z)}
#'                {\sum_{r=1}^{R} w_r}.}
#'
#' `method = "profile"` uses a single reference row,
#' \eqn{\hat{f}_r(z, x_{-j}^{ref})}; `method = "pdp"` averages each model's
#' predictions over sampled background rows,
#' \deqn{h_r(z) = \frac{1}{m}\sum_{i=1}^{m}
#'   \hat{f}_r(z, x_{-j}^{(i)});}
#' and `method = "ale"` accumulates centred local prediction differences using
#' the same univariate ALE definitions as [univariate()]. These definitions
#' follow the model-agnostic PDP and ALE notation summarized by Molnar (2025).
#'
#' The default `interval = "sd"` draws \eqn{H(z) \pm s(z)}, using the ordinary
#' standard deviation across model curves or, with weights,
#' \deqn{s(z) = \sqrt{\frac{\sum_{r=1}^{R} w_r\{h_r(z) - H(z)\}^2}
#'                         {\sum_{r=1}^{R} w_r}}.}
#' `interval = "quantile"` instead draws central pointwise quantiles of the
#' model-specific curve values.
#'
#' @param models A list of fitted ensemble member models that support
#'   prediction. Models should be fitted to compatible predictor variables and
#'   return predictions on the same response scale.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param fun A function used to generate predictions from the model, or a list
#'   of functions the same length as `models`. If `NULL`, the generic
#'   `predict()` is used for every model.
#' @param ... Additional arguments passed to each prediction function. For
#'   mixed model types with different prediction interfaces, prefer supplying
#'   model-specific wrappers through `fun`.
#' @param method Character, the curve type to plot. `"profile"` uses a single
#'   reference profile, `"pdp"` averages over sampled predictor rows before
#'   combining ensemble members, and `"ale"` draws accumulated local effects
#'   curves.
#' @param n Integer, number of points to sample for each numeric predictor
#'   variable (default: 100). For `"ale"`, `n` sets the maximum number of
#'   intervals used to estimate local effects for numeric predictors.
#' @param background_n Integer, number of randomly sampled background rows used
#'   for `"pdp"` (default: `n`).
#' @param agg Function used to combine model-specific predictions at each point
#'   along the curve. Defaults to `mean`.
#' @param weights Optional numeric vector of model weights with the same length
#'   as `models`.
#' @param interval Character, interval type drawn around the ensemble curve.
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
#' @param show_models Logical, whether to overlay individual ensemble member
#'   curves beneath the ensemble curve (default: `FALSE`).
#'
#' @return A `ggplot2` object containing the response curves arranged in a grid.
#'
#' @references
#' Molnar, C. (2025). *Interpretable Machine Learning: A Guide for Making Black
#' Box Models Explainable* (3rd ed.). <https://christophm.github.io/interpretable-ml-book/>
#'
#' Friedman, J. H. (2001). Greedy function approximation: A gradient boosting
#' machine. *The Annals of Statistics*, 29(5), 1189-1232.
#'
#' Apley, D. W., & Zhu, J. (2020). Visualizing the effects of predictor
#' variables in black box supervised learning models. *Journal of the Royal
#' Statistical Society: Series B*, 82(4), 1059-1086.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   data(iris)
#'   predictors <- iris[, c("Sepal.Width", "Petal.Length")]
#'
#'   models <- list(
#'     lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris),
#'     mgcv::gam(
#'       Sepal.Length ~ s(Sepal.Width) + s(Petal.Length),
#'       data = iris
#'     )
#'   )
#'
#'   response_plot <- multimodel(
#'     models,
#'     predictors,
#'     method = "pdp",
#'     background_n = 50,
#'     show_models = TRUE
#'   )
#'   print(response_plot)
#' }
multimodel <- function(models, x = NULL, predict_data = NULL,
                       fun = NULL, ...,
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
    funs <- normalize_multimodel_funs(
        fun,
        n_models = nmod,
        env = parent.frame()
    )
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
    nvars <- length(predictor_specs)

    ncols <- if (is.null(ncols)) ceiling(sqrt(nvars)) else ncols
    nrows <- if (is.null(nrows)) ceiling(nvars / ncols) else nrows
    ale_level_orders <- if (method == "ale") {
        derive_multimodel_ale_level_orders(
            models = models,
            funs = funs,
            predictor_specs = predictor_specs,
            ale_rows = ale_rows,
            agg = agg,
            weights = weights,
            response = response,
            ...
        )
    } else {
        NULL
    }

    tables <- lapply(predictor_specs, function(spec) {
        model_curves <- build_multimodel_curve_matrix(
            models = models,
            funs = funs,
            method = method,
            spec = spec,
            reference_row = reference_row,
            background_rows = background_rows,
            ale_rows = ale_rows,
            ale_level_orders = ale_level_orders,
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
            curve_color = if (show_models) "gray70" else color,
            curve_alpha = if (show_models) 0.4 else 1,
            curve_linewidth = if (show_models) 0.6 else 0.7,
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
                                          ale_rows, ale_level_orders,
                                          n, response, ...) {
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
            level_order = ale_level_orders[[spec$name]],
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


derive_multimodel_ale_level_orders <- function(models, funs, predictor_specs,
                                               ale_rows, agg, weights,
                                               response, ...) {
    factor_specs <- predictor_specs[vapply(
        predictor_specs,
        function(spec) spec$is_factor && !spec$is_ordered,
        logical(1)
    )]

    if (!length(factor_specs)) {
        return(list())
    }

    level_orders <- lapply(factor_specs, function(spec) {
        multimodel_ale_level_order(
            models = models,
            funs = funs,
            ale_rows = ale_rows,
            column = spec$name,
            agg = agg,
            weights = weights,
            response = response,
            ...
        )
    })
    names(level_orders) <- names(factor_specs)

    level_orders
}


multimodel_ale_level_order <- function(models, funs, ale_rows, column,
                                       agg, weights, response, ...) {
    x <- ale_rows[[column]]
    observed_levels <- levels(x)[levels(x) %in% unique(as.character(x))]

    if (!length(observed_levels)) {
        return(character(0))
    }

    level_scores <- vapply(seq_along(models), function(index) {
        preds <- extract_prediction_vector(
            funs[[index]](models[[index]], ale_rows, ...),
            n = nrow(ale_rows),
            response = response
        )

        vapply(observed_levels, function(level) {
            mean(preds[x == level])
        }, numeric(1))
    }, numeric(length(observed_levels)))

    if (is.null(dim(level_scores))) {
        level_scores <- matrix(level_scores, ncol = 1L)
    }

    aggregate_scores <- apply(
        level_scores,
        1L,
        aggregate_multimodel_values,
        agg = agg,
        weights = weights
    )

    observed_levels[order(aggregate_scores, seq_along(aggregate_scores))]
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
