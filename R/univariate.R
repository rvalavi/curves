#' Univariate model-agnostic response curves
#'
#' Plot how a fitted model's predictions change across one predictor at a time
#' using single-profile, partial dependence, individual conditional expectation,
#' or accumulated local effects curves.
#'
#' @details
#' Let \eqn{\hat{f}} denote the fitted prediction function, \eqn{x_j} the
#' predictor being plotted, and \eqn{x_{-j}} all other predictors. The
#' `"profile"` method builds one reference row, using the mean numeric value and
#' modal factor level of each predictor, and plots
#' \deqn{g_j(z) = \hat{f}(z, x_{-j}^{ref}).}
#' This is a single ceteris paribus curve: it is easy to read, but it describes
#' the model only around the chosen reference profile.
#'
#' For `"ice"`, the curve for sampled row \eqn{i} is
#' \deqn{g_j^{(i)}(z) = \hat{f}(z, x_{-j}^{(i)}).}
#' ICE curves show row-level heterogeneity. `"ice+pdp"` overlays their average.
#' For `"pdp"`, the plotted partial dependence curve is the Monte Carlo
#' average over \eqn{m} sampled background rows,
#' \deqn{\hat{f}_{j,PDP}(z) = \frac{1}{m}\sum_{i=1}^{m}
#'   \hat{f}(z, x_{-j}^{(i)}).}
#' This follows the marginal-distribution PDP definition in Molnar (2025).
#' When predictors are strongly dependent, PDP and ICE curves can include
#' feature combinations that are rare or outside the observed data distribution.
#'
#' For `"ale"`, numeric \eqn{x_j} is split into intervals with breakpoints
#' \eqn{z_0 < \cdots < z_K}. For interval
#' \eqn{N_j(k) = \{i: x_j^{(i)} \in (z_{k-1}, z_k]\}}, the local effect is
#' estimated by
#' \deqn{\Delta_{j,k} = \frac{1}{n_j(k)}\sum_{i \in N_j(k)}
#'   \left[\hat{f}(z_k, x_{-j}^{(i)}) -
#'   \hat{f}(z_{k-1}, x_{-j}^{(i)})\right].}
#' The reported value at the interval centre
#' \eqn{c_k = (z_{k-1} + z_k) / 2} is accumulated and centred,
#' \deqn{\hat{f}_{j,ALE}(c_k) =
#'   \left(\sum_{\ell=1}^{k}\Delta_{j,\ell} -
#'   \frac{1}{2}\Delta_{j,k}\right) -
#'   \frac{\sum_{k=1}^{K} n_j(k)\tilde{f}_{j,ALE}(c_k)}
#'        {\sum_{k=1}^{K} n_j(k)},}
#' where \eqn{\tilde{f}_{j,ALE}(c_k)} is the uncentred accumulated value. ALE
#' averages local prediction differences within observed intervals and then
#' centres the curve so its sample-weighted mean effect is zero.
#'
#' @param model A fitted model object that supports prediction.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param fun A function used to generate predictions from the model. If
#'   `NULL`, the generic `predict()` is used.
#' @param ... Additional arguments passed to `fun`.
#' @param n Integer, number of points to sample for each numeric predictor
#'   variable (default: 100). For `"ale"`, `n` sets the maximum number of
#'   intervals used to estimate local effects for numeric predictors.
#' @param background_n Integer, number of randomly sampled background rows used
#'   for `"pdp"`, `"ice"`, and `"ice+pdp"` (default: `n`).
#' @param interval Character, interval type used to draw a PDP ribbon for
#'   numeric predictors. Only `"quantile"` is currently supported and only when
#'   `method = "pdp"`. Defaults to `"none"`.
#' @param interval_level Numeric in `(0, 1)` giving the central quantile width
#'   used when `interval = "quantile"`. Ignored otherwise.
#' @param nrows Integer, number of rows in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param ncols Integer, number of columns in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param rug Logical, whether to include a rug plot along the x-axis (default:
#'   `TRUE`).
#' @param ylim Numeric vector of length 2, specifying the limits of the y-axis.
#'   If `NULL`, limits are automatically set.
#' @param ylab Optional character label for the y-axis. If `NULL`, the default
#'   is `"Prediction"` for profile, PDP, and ICE methods, and
#'   `"Accumulated local effect"` for `method = "ale"`.
#' @param color Character, colour of the response curve (default:
#'   `"deepskyblue4"`).
#' @param response Optional column name or index to select when `fun` returns
#'   multiple predictions per row. If `NULL` and exactly two prediction columns
#'   are returned, the second column is used.
#' @param method Character, the curve type to plot. `"profile"` uses a single
#'   reference profile, `"pdp"` averages over sampled predictor rows,
#'   `"ice"` draws individual conditional expectation curves, and `"ice+pdp"`
#'   overlays the averaged PDP on top of the ICE curves. `"ale"` draws
#'   accumulated local effects curves.
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
#' Goldstein, A., Kapelner, A., Bleich, J., & Pitkin, E. (2015). Peeking inside
#' the black box: Visualizing statistical learning with plots of individual
#' conditional expectation. *Journal of Computational and Graphical Statistics*,
#' 24(1), 44-65.
#'
#' Apley, D. W., & Zhu, J. (2020). Visualizing the effects of predictor
#' variables in black box supervised learning models. *Journal of the Royal
#' Statistical Society: Series B*, 82(4), 1059-1086.
#'
#' @export
#'
#' @examples
#' data(iris)
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' response_plot <- univariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
#' )
#' print(response_plot)
#'
#' pdp_plot <- univariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'   method = "pdp",
#'   n = 25,
#'   background_n = 50,
#'   interval = "quantile",
#'   interval_level = 0.8
#' )
#' print(pdp_plot)
#'
#' ice_plot <- univariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'   method = "ice+pdp",
#'   n = 25,
#'   background_n = 50
#' )
#' print(ice_plot)
#'
#' ale_plot <- univariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'   method = "ale",
#'   n = 20
#' )
#' print(ale_plot)
univariate <- function(model, x = NULL,
                       predict_data = NULL,
                       fun = NULL, ...,
                       n = 100,
                       background_n = n,
                       interval = c("none", "quantile"),
                       interval_level = 0.8,
                       rug = TRUE,
                       ylim = NULL,
                       ylab = NULL,
                       color = "deepskyblue4",
                       response = NULL,
                       nrows = NULL,
                       ncols = NULL,
                       method = c("profile", "pdp", "ice", "ice+pdp", "ale")) {

    method <- match.arg(method)
    interval <- match.arg(interval)
    fun <- resolve_predict_fun(fun, env = parent.frame())
    n <- validate_curve_n(n)
    background_n <- validate_background_n(background_n)

    if (interval != "none" && method != "pdp") {
        stop("interval is only supported when method = \"pdp\"")
    }

    if (interval == "quantile") {
        interval_level <- validate_interval_level(interval_level)
    }

    if (is.null(ylab)) {
        ylab <- default_univariate_ylab(method)
    }

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
        x_source <- x
    } else {
        x_source <- predict_data
    }

    sample_size <- curve_sample_size(
        x_source,
        n = n,
        background_n = background_n,
        method = method
    )

    x_df <- validate_predictors(x_source, sample_size = sample_size)
    nms <- names(x_df)
    predictor_names <- nms

    reference_row <- if (method == "profile") build_reference_row(x_df) else NULL
    background_rows <- if (method %in% c("pdp", "ice", "ice+pdp")) {
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

    tables <- lapply(predictor_specs, function(spec) {
        if (method == "profile") {
            curve_df <- build_profile_curve_table(
                model = model,
                reference_row = reference_row,
                column = spec$name,
                values = spec$values,
                fun = fun,
                response = response,
                ...
            )

            return(list(curves = curve_df, summary = NULL))
        }

        if (method == "ale") {
            curve_df <- build_ale_curve_table(
                model = model,
                ale_rows = ale_rows,
                column = spec$name,
                n = n,
                fun = fun,
                response = response,
                ...
            )

            return(list(curves = curve_df, summary = NULL))
        }

        curve_df <- build_ice_curve_table(
            model = model,
            background_rows = background_rows,
            column = spec$name,
            values = spec$values,
            fun = fun,
            response = response,
            ...
        )

        summary_df <- if (method %in% c("pdp", "ice+pdp")) {
            average_curve_table(
                curve_df,
                band = if (method == "pdp" &&
                    !spec$is_factor &&
                    interval == "quantile") {
                    interval_level
                } else {
                    NULL
                }
            )
        } else {
            NULL
        }

        if (method == "pdp") {
            list(curves = summary_df, summary = NULL)
        } else {
            list(curves = curve_df, summary = summary_df)
        }
    })

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
            ylim = limits,
            curve_color = if (identical(method, "ice+pdp")) {
                "gray70"
            } else {
                color
            },
            curve_alpha = if (method %in% c("ice", "ice+pdp")) 0.15 else 1,
            curve_linewidth = if (method %in% c("ice", "ice+pdp")) 0.35 else 0.7,
            summary_df = table$summary,
            summary_linewidth = 1
        )
    })

    cowplot::plot_grid(plotlist = plots, nrow = nrows, ncol = ncols)
}


default_univariate_ylab <- function(method) {
    if (identical(method, "ale")) {
        return("Accumulated local effect")
    }

    "Prediction"
}


build_profile_curve_table <- function(model, reference_row, column, values, fun,
                                      response, ...) {
    grid <- build_curve_grid(reference_row, column, values)

    data.frame(
        x = grid[[column]],
        y = extract_prediction_vector(
            fun(model, grid, ...),
            n = nrow(grid),
            response = response
        )
    )
}


build_ice_curve_table <- function(model, background_rows, column, values, fun,
                                  response, ...) {
    n_background <- nrow(background_rows)
    grid <- build_curve_stack(background_rows, column, values)

    data.frame(
        curve = rep(seq_len(n_background), each = length(values)),
        x = grid[[column]],
        y = extract_prediction_vector(
            fun(model, grid, ...),
            n = nrow(grid),
            response = response
        )
    )
}


build_ale_curve_table <- function(model, ale_rows, column, n, fun,
                                  response, level_order = NULL, ...) {
    x <- ale_rows[[column]]

    if (is.factor(x)) {
        return(build_categorical_ale_curve_table(
            model    = model,
            ale_rows = ale_rows,
            column   = column,
            fun      = fun,
            response = response,
            level_order = level_order,
            ...
        ))
    }

    if (!is.numeric(x)) {
        stop("ALE supports numeric and factor predictors only. Unsupported column: ", column)
    }

    breaks <- ale_breaks(x, n = n)

    if (length(breaks) < 2L) {
        return(data.frame(x = breaks[1], y = 0))
    }

    interval <- findInterval(
        x,
        vec = breaks,
        rightmost.closed = TRUE,
        all.inside = TRUE
    )
    n_intervals <- length(breaks) - 1L
    lower_grid <- ale_rows
    upper_grid <- ale_rows

    lower_grid[[column]] <- breaks[interval]
    upper_grid[[column]] <- breaks[interval + 1L]

    diffs <- extract_prediction_vector(
        fun(model, upper_grid, ...),
        n = nrow(upper_grid),
        response = response
    ) - extract_prediction_vector(
        fun(model, lower_grid, ...),
        n = nrow(lower_grid),
        response = response
    )

    counts <- tabulate(interval, nbins = n_intervals)
    keep <- counts > 0L
    lower_breaks <- breaks[-length(breaks)][keep]
    upper_breaks <- breaks[-1L][keep]
    counts <- counts[keep]
    mean_diffs <- vapply(
        which(keep),
        function(index) mean(diffs[interval == index]),
        numeric(1)
    )
    ale_values <- cumsum(mean_diffs) - (mean_diffs / 2)
    ale_values <- ale_values - stats::weighted.mean(ale_values, w = counts)

    data.frame(
        x = (lower_breaks + upper_breaks) / 2,
        y = ale_values
    )
}


build_categorical_ale_curve_table <- function(model, ale_rows, column, fun,
                                               response, level_order = NULL,
                                               ...) {
    x <- ale_rows[[column]]
    all_levels <- levels(x)

    if (length(all_levels) < 2L) {
        return(data.frame(
            x = factor(all_levels, levels = all_levels, ordered = is.ordered(x)),
            y = 0
        ))
    }

    ordered_levels <- if (!is.null(level_order)) {
        valid_levels <- if (is.ordered(x)) {
            all_levels
        } else {
            all_levels[all_levels %in% unique(as.character(x))]
        }

        if (!is.character(level_order) ||
            !setequal(level_order, valid_levels) ||
            length(level_order) != length(valid_levels)) {
            stop(
                "level_order must contain each plotted factor level exactly once for column ",
                column
            )
        }

        level_order
    } else if (is.ordered(x)) {
        all_levels
    } else {
        preds <- extract_prediction_vector(
            fun(model, ale_rows, ...),
            n = nrow(ale_rows),
            response = response
        )
        level_means <- tapply(preds, x, mean)
        names(sort(level_means))
    }

    K <- length(ordered_levels)
    counts <- tabulate(match(x, ordered_levels), nbins = K)

    deltas <- vapply(seq(2L, K), function(k) {
        lk_prev <- ordered_levels[k - 1L]
        lk      <- ordered_levels[k]
        mask    <- x %in% c(lk_prev, lk)

        if (!any(mask)) {
            return(0)
        }

        rows_sub <- ale_rows[mask, , drop = FALSE]
        rows_upper <- rows_sub
        rows_lower <- rows_sub
        rows_upper[[column]] <- factor(lk,      levels = all_levels, ordered = is.ordered(x))
        rows_lower[[column]] <- factor(lk_prev, levels = all_levels, ordered = is.ordered(x))

        mean(
            extract_prediction_vector(
                fun(model, rows_upper, ...),
                n = nrow(rows_upper),
                response = response
            ) - extract_prediction_vector(
                fun(model, rows_lower, ...),
                n = nrow(rows_lower),
                response = response
            )
        )
    }, numeric(1))

    ale_values <- c(0, cumsum(deltas))

    if (sum(counts) > 0L) {
        ale_values <- ale_values - stats::weighted.mean(ale_values, w = counts)
    }

    output_levels <- if (is.ordered(x)) all_levels else ordered_levels

    data.frame(
        x = factor(ordered_levels, levels = output_levels, ordered = is.ordered(x)),
        y = ale_values
    )
}


ale_breaks <- function(x, n) {
    x <- sort(stats::na.omit(x))

    if (!length(x)) {
        stop("Predictors must contain at least one non-missing value")
    }

    if (!is.numeric(x)) {
        stop("ALE currently supports numeric predictors only")
    }

    unique_x <- sort(unique(x))

    if (length(unique_x) == 1L) {
        return(unique_x)
    }

    n_intervals <- min(n, length(unique_x) - 1L)
    break_index <- unique(round(seq(1, length(x), length.out = n_intervals + 1L)))
    unique(x[break_index])
}


average_curve_table <- function(df, band = NULL) {
    summary <- stats::aggregate(y ~ x, data = df[, c("x", "y"), drop = FALSE],
                                FUN = mean)

    if (!is.null(band)) {
        probs <- c((1 - band) / 2, 1 - ((1 - band) / 2))
        bounds <- stats::aggregate(
            y ~ x,
            data = df[, c("x", "y"), drop = FALSE],
            FUN = function(values) {
                stats::quantile(values, probs = probs, names = FALSE)
            }
        )
        bounds_mat <- if (is.matrix(bounds$y)) {
            bounds$y
        } else {
            do.call(rbind, bounds$y)
        }
        summary$ymin <- bounds_mat[, 1]
        summary$ymax <- bounds_mat[, 2]
    }

    if (is.factor(df$x)) {
        summary$x <- factor(
            as.character(summary$x),
            levels = levels(df$x),
            ordered = is.ordered(df$x)
        )
    }

    summary
}


plot_1D <- function(df, dat, fact, ordered_factor = FALSE, rug, se,
                    x_name, y_name, ylim, color,
                    ribcol = "grey85", curve_color = color,
                    curve_alpha = 1,
                    curve_linewidth = 0.7, summary_df = NULL,
                    summary_linewidth = 1) {

    has_curve_groups <- "curve" %in% names(df)

    if (!fact) {
        if (has_curve_groups) {
            df <- df[order(df$curve, df$x), , drop = FALSE]
        } else {
            df <- df[order(df$x), , drop = FALSE]
        }
    }

    if (!is.null(summary_df) && !fact) {
        summary_df <- summary_df[order(summary_df$x), , drop = FALSE]
    }

    plt <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))

    has_band <- !fact && !has_curve_groups && all(c("ymin", "ymax") %in% names(df))
    summary_has_band <- !is.null(summary_df) &&
        !fact &&
        all(c("ymin", "ymax") %in% names(summary_df))

    if (has_band) {
        plt <- plt + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = get("ymin"), ymax = get("ymax")),
            fill = ribcol,
            alpha = 0.35
        )
    } else if (summary_has_band) {
        plt <- plt + ggplot2::geom_ribbon(
            data = summary_df,
            ggplot2::aes(
                x = get("x"),
                ymin = get("ymin"),
                ymax = get("ymax")
            ),
            fill = ribcol,
            alpha = 0.35,
            inherit.aes = FALSE
        )
    } else if (ncol(df) > 2L && !fact && se && !has_curve_groups) {
        plt <- plt + ggplot2::geom_ribbon(
            ggplot2::aes(
                ymin = get("y") - get("std"),
                ymax = get("y") + get("std")
            ),
            fill = ribcol,
            alpha = 0.6
        )
    }

    if (fact) {
        if (has_curve_groups) {
            if (ordered_factor) {
                plt <- plt +
                    ggplot2::geom_line(
                        ggplot2::aes(group = get("curve")),
                        color = curve_color,
                        alpha = curve_alpha,
                        linewidth = curve_linewidth
                    )
            }

            plt <- plt +
                ggplot2::geom_point(
                    ggplot2::aes(group = get("curve")),
                    color = curve_color,
                    alpha = curve_alpha,
                    size = 1.2
                )
        } else {
            plt <- plt +
                ggplot2::geom_point(color = color, size = 2.5)
        }

        plt <- plt + ggplot2::scale_x_discrete(drop = FALSE)
    } else {
        if (has_curve_groups) {
            plt <- plt + ggplot2::geom_line(
                ggplot2::aes(group = get("curve")),
                color = curve_color,
                alpha = curve_alpha,
                linewidth = curve_linewidth
            )
        } else {
            plt <- plt + ggplot2::geom_line(
                color = curve_color,
                linewidth = curve_linewidth
            )
        }
    }

    if (!is.null(summary_df)) {
        if (fact) {
            if (ordered_factor) {
                plt <- plt +
                    ggplot2::geom_line(
                        data = summary_df,
                        ggplot2::aes(x = x, y = y, group = 1),
                        color = color,
                        linewidth = summary_linewidth,
                        inherit.aes = FALSE
                    )
            }

            plt <- plt +
                ggplot2::geom_point(
                    data = summary_df,
                    ggplot2::aes(x = x, y = y),
                    color = color,
                    size = 2.5,
                    inherit.aes = FALSE
                )
        } else {
            plt <- plt + ggplot2::geom_line(
                data = summary_df,
                ggplot2::aes(x = x, y = y),
                color = color,
                linewidth = summary_linewidth,
                inherit.aes = FALSE
            )
        }
    }

    if (rug && !fact && !is.null(dat) && nrow(dat) > 0L) {
        plt <- plt + ggplot2::geom_rug(
            data = dat,
            ggplot2::aes(x = var),
            sides = "b",
            color = "black",
            alpha = 0.5,
            inherit.aes = FALSE
        )
    }

    plt +
        ggplot2::coord_cartesian(ylim = ylim) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = x_name, y = y_name)
}
