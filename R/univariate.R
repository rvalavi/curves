#' Univariate response curve plot
#'
#' This function generates response curves for a given model by varying one
#' predictor at a time while keeping others constant.
#'
#' @param model A fitted model object that supports prediction.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param fun A function used to generate predictions from the model. Defaults
#'   to `predict`.
#' @param ... Additional arguments passed to `fun`.
#' @param n Integer, number of points to sample for each numeric predictor
#'   variable (default: 100). For `"pdp"`, `"ice"`, and `"ice+pdp"`, `n` also
#'   controls how many predictor rows are sampled from `predict_data`.
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
#' @param method Character, the curve type to plot. `"profile"` uses a single
#'   reference profile, `"pdp"` averages over sampled predictor rows,
#'   `"ice"` draws individual conditional expectation curves, and `"ice+pdp"`
#'   overlays the averaged PDP on top of the ICE curves.
#'
#' @return A `ggplot2` object containing the response curves arranged in a grid.
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
#' ice_plot <- univariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'   method = "ice+pdp",
#'   n = 25
#' )
#' print(ice_plot)
univariate <- function(model, x = NULL,
                       predict_data = NULL,
                       fun = stats::predict, ...,
                       n = 100,
                       ylab = "Prediction",
                       rug = TRUE,
                       ylim = NULL,
                       color = "deepskyblue4",
                       response = NULL,
                       nrows = NULL,
                       ncols = NULL,
                       method = c("profile", "pdp", "ice", "ice+pdp")) {

    method <- match.arg(method)
    n <- validate_curve_n(n)

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
        x_source <- x
    } else {
        x_source <- predict_data
    }

    sample_size <- if (.is_rast(x_source)) {
        max(5000L, n * 50L)
    } else {
        5000L
    }

    x_df <- validate_predictors(x_source, sample_size = sample_size)
    nms <- names(x_df)
    nvars <- ncol(x_df)

    ncols <- if (is.null(ncols)) ceiling(sqrt(nvars)) else ncols
    nrows <- if (is.null(nrows)) ceiling(nvars / ncols) else nrows

    reference_row <- if (method == "profile") build_reference_row(x_df) else NULL
    background_rows <- if (method == "profile") {
        NULL
    } else {
        sample_background_rows(x_df, n = n)
    }

    predictor_specs <- lapply(nms, function(name) {
        list(
            name = name,
            is_factor = is.factor(x_df[[name]]),
            values = curve_values(x_df[[name]], n = n)
        )
    })
    names(predictor_specs) <- nms

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
            average_curve_table(curve_df)
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

            if (!is.null(table$summary)) {
                values <- c(values, table$summary$y)
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
            rug = rug && !spec$is_factor,
            se = FALSE,
            x_name = spec$name,
            y_name = ylab,
            color = color,
            ylim = limits,
            curve_alpha = if (method %in% c("ice", "ice+pdp")) 0.15 else 1,
            curve_linewidth = if (method %in% c("ice", "ice+pdp")) 0.35 else 0.7,
            summary_df = table$summary,
            summary_linewidth = 1
        )
    })

    cowplot::plot_grid(plotlist = plots, nrow = nrows, ncol = ncols)
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


average_curve_table <- function(df) {
    summary <- stats::aggregate(y ~ x, data = df[, c("x", "y"), drop = FALSE],
                                FUN = mean)

    if (is.factor(df$x)) {
        summary$x <- factor(
            as.character(summary$x),
            levels = levels(df$x),
            ordered = is.ordered(df$x)
        )
    }

    summary
}


plot_1D <- function(df, dat, fact, rug, se, x_name, y_name, ylim, color,
                    ribcol = "grey85", curve_alpha = 1,
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

    if (ncol(df) > 2L && !fact && se && !has_curve_groups) {
        plt <- plt + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = y - std, ymax = y + std),
            fill = ribcol,
            alpha = 0.6
        )
    }

    if (fact) {
        if (has_curve_groups) {
            plt <- plt +
                ggplot2::geom_line(
                    ggplot2::aes(group = curve),
                    color = color,
                    alpha = curve_alpha,
                    linewidth = curve_linewidth
                ) +
                ggplot2::geom_point(
                    ggplot2::aes(group = curve),
                    color = color,
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
                ggplot2::aes(group = curve),
                color = color,
                alpha = curve_alpha,
                linewidth = curve_linewidth
            )
        } else {
            plt <- plt + ggplot2::geom_line(
                color = color,
                linewidth = curve_linewidth
            )
        }
    }

    if (!is.null(summary_df)) {
        if (fact) {
            plt <- plt +
                ggplot2::geom_line(
                    data = summary_df,
                    ggplot2::aes(x = x, y = y, group = 1),
                    color = color,
                    linewidth = summary_linewidth,
                    inherit.aes = FALSE
                ) +
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
