#' Bivariate model-agnostic response surfaces
#'
#' Plot how a fitted model's predictions change across pairs of predictors using
#' reference-profile, partial dependence, or second-order accumulated local
#' effects surfaces.
#'
#' @details
#' Let \eqn{\hat{f}} denote the fitted prediction function, let
#' \eqn{S = \{a, b\}} be the two plotted predictors, and let \eqn{x_C} contain
#' the remaining predictors. The `"profile"` method evaluates a single
#' reference profile over the two-dimensional grid,
#' \deqn{g_S(z_a, z_b) = \hat{f}(z_a, z_b, x_C^{ref}).}
#' This is a direct response surface around the mean/modal reference row.
#'
#' For `"pdp"`, the surface is the Monte Carlo partial dependence estimate over
#' \eqn{m} sampled background rows,
#' \deqn{\hat{f}_{S,PDP}(z_a, z_b) =
#'   \frac{1}{m}\sum_{i=1}^{m}\hat{f}(z_a, z_b, x_C^{(i)}).}
#' This marginalizes over the non-plotted predictors as described for PDPs by
#' Molnar (2025). The result includes interactions with other predictors, but it
#' may evaluate uncommon predictor combinations when predictors are dependent.
#'
#' For `"ale"`, both predictors must be numeric. The two features are divided
#' into rectangular cells with x-breaks \eqn{z_0 < \cdots < z_K} and y-breaks
#' \eqn{w_0 < \cdots < w_L}. For observations in cell \eqn{(k, l)}, the
#' second-order local effect is the mean corner contrast
#' \deqn{\Delta_{k,l} = \frac{1}{n_{k,l}}\sum_{i \in N(k,l)}
#'   [\hat{f}(z_k, w_l, x_C^{(i)}) -
#'    \hat{f}(z_{k-1}, w_l, x_C^{(i)}) -
#'    \hat{f}(z_k, w_{l-1}, x_C^{(i)}) +
#'    \hat{f}(z_{k-1}, w_{l-1}, x_C^{(i)})].}
#' The cell effects are accumulated over the grid and centred by removing the
#' row, column, and overall means. The resulting surface is a second-order ALE
#' estimate: it is intended to show the additional interaction effect of the two
#' predictors after their main effects have been removed.
#'
#' @param model A fitted model object that supports prediction.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param pairs Optional specification of predictor pairs to plot. Supply
#'   `NULL` to plot all unique pairs, a character or numeric vector of length 2
#'   for a single pair, or a list/data frame/matrix of pairs. Numeric pairs are
#'   interpreted as predictor column indices.
#' @param fun A function used to generate predictions from the model. Defaults
#'   to `predict`.
#' @param ... Additional arguments passed to `fun`.
#' @param n Integer, number of points to sample for each numeric predictor
#'   variable (default: 40). For `"ale"`, `n` sets the maximum number of
#'   intervals used to estimate local effects for each numeric predictor.
#' @param background_n Integer, number of randomly sampled background rows used
#'   for `"pdp"` (default: `n`).
#' @param rug Logical, whether to add a marginal rug for numeric predictor pairs
#'   in static plots (default: `FALSE`).
#' @param plot_type Character, plot type. Use `"heatmap"` for a static surface,
#'   `"contour"` for filled contours, or `"surface"` for an interactive 3D
#'   surface. The 3D surface requires the suggested `plotly` package and a
#'   single numeric predictor pair.
#' @param zlab Character, label for the response legend or z-axis (default:
#'   `"Prediction"`).
#' @param bins Integer, number of contour bins for `"contour"` plots.
#' @param palette Either a viridis option name (default: `"viridis"`) or a
#'   character vector of colours used for the response scale.
#' @param response Optional column name or index to select when `fun` returns
#'   multiple predictions per row. If `NULL` and exactly two prediction columns
#'   are returned, the second column is used.
#' @param nrows Integer, number of rows in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param ncols Integer, number of columns in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param method Character, the surface type to plot. `"profile"` uses a single
#'   reference profile, `"pdp"` averages over sampled predictor rows, and
#'   `"ale"` draws a centred second-order accumulated local effects surface for
#'   numeric predictor pairs. Non-numeric pairs are ignored with a warning for
#'   `"ale"`.
#'
#' @return A `ggplot2` object for static plot types or a `plotly` widget for
#'   `plot_type = "surface"`.
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
#' data(iris)
#' model <- lm(
#'   Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
#'   data = iris
#' )
#' response_plot <- bivariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
#' )
#' print(response_plot)
#'
#' pdp_plot <- bivariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'   pairs = c("Sepal.Width", "Petal.Length"),
#'   method = "pdp",
#'   n = 25,
#'   background_n = 50,
#'   rug = TRUE
#' )
#' print(pdp_plot)
#'
#' ale_plot <- bivariate(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'   pairs = c("Sepal.Width", "Petal.Length"),
#'   method = "ale",
#'   n = 10
#' )
#' print(ale_plot)
#'
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   surface_plot <- bivariate(
#'     model,
#'     x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
#'     pairs = c("Sepal.Width", "Petal.Length"),
#'     plot_type = "surface"
#'   )
#'   surface_plot
#' }
bivariate <- function(model, x = NULL, predict_data = NULL, pairs = NULL,
                      fun = stats::predict, ..., n = 40,
                      background_n = n, rug = FALSE,
                      plot_type = c("heatmap", "contour", "surface"),
                      zlab = "Prediction", bins = 8,
                      palette = "viridis",
                      response = NULL,
                      nrows = NULL, ncols = NULL,
                      method = c("profile", "pdp", "ale")) {

    plot_type <- match.arg(plot_type)
    method <- match.arg(method)
    n <- validate_curve_n(n)
    background_n <- validate_background_n(background_n)

    if (missing(zlab) && method == "ale") {
        zlab <- "ALE"
    }
    if (missing(palette) && method == "ale") {
        palette <- default_ale_palette()
    }

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
        x_source <- x
    } else {
        x_source <- predict_data
    }

    if (!is.numeric(bins) || length(bins) != 1L || bins < 2) {
        stop("bins must be a single number greater than or equal to 2")
    }

    sample_size <- curve_sample_size(
        x_source,
        n = n,
        background_n = background_n,
        method = method
    )

    x_df <- validate_predictors(x_source, sample_size = sample_size)
    pair_specs <- build_pair_specs(x_df, pairs = pairs, n = n, method = method)
    pair_specs <- if (method == "ale") {
        filter_ale_pair_specs(pair_specs)
    } else {
        pair_specs
    }
    npairs <- length(pair_specs)

    if (!npairs) {
        if (method == "ale") {
            stop("ALE requires at least one numeric predictor pair to plot")
        }

        stop("No valid predictor pairs remain to plot")
    }

    if (plot_type == "surface" && npairs != 1L) {
        stop("plot_type = \"surface\" requires a single predictor pair")
    }

    if (rug && plot_type == "surface") {
        warning("rug is ignored when plot_type = \"surface\"", call. = FALSE)
    }

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

    tables <- lapply(pair_specs, function(spec) {
        if (method == "profile") {
            return(build_profile_surface_table(
                model = model,
                reference_row = reference_row,
                spec = spec,
                fun = fun,
                response = response,
                ...
            ))
        }

        if (method == "pdp") {
            return(build_pdp_surface_table(
                model = model,
                background_rows = background_rows,
                spec = spec,
                fun = fun,
                response = response,
                ...
            ))
        }

        build_ale_surface_table(
            model = model,
            ale_rows = ale_rows,
            spec = spec,
            fun = fun,
            response = response,
            ...
        )
    })

    if (plot_type == "surface") {
        return(plot_surface_3D(
            spec = pair_specs[[1]],
            df = tables[[1]],
            zlab = zlab
        ))
    }

    z_values <- unlist(lapply(tables, function(table) table$z))
    z_limits <- if (method == "ale") {
        ale_surface_limits(z_values, padding = 0.02)
    } else {
        curve_limits(z_values, padding = 0.02)
    }
    contour_breaks <- response_breaks(z_limits, bins = bins)

    plots <- Map(
        function(spec, table) {
            plot_2D(
                df = table,
                x_name = spec$x_name,
                y_name = spec$y_name,
                x_factor = spec$x_factor,
                y_factor = spec$y_factor,
                plot_type = plot_type,
                z_name = zlab,
                palette = palette,
                z_limits = z_limits,
                contour_breaks = contour_breaks,
                rug_df = if (rug &&
                    !spec$x_factor &&
                    !spec$y_factor &&
                    plot_type != "surface") {
                    sample_bivariate_rug_data(
                        x_df,
                        x_name = spec$x_name,
                        y_name = spec$y_name
                    )
                } else {
                    NULL
                }
            )
        },
        pair_specs,
        tables
    )

    if (is.null(ncols)) {
        ncols <- ceiling(sqrt(npairs))
    }
    if (is.null(nrows)) {
        nrows <- ceiling(npairs / ncols)
    }

    combine_bivariate_plots(plots, nrow = nrows, ncol = ncols)
}


build_pair_specs <- function(x_df, pairs, n, method = "profile") {
    pair_list <- validate_pairs(x_df, pairs = pairs)

    lapply(pair_list, function(pair) {
        x_name <- pair[1]
        y_name <- pair[2]
        x_breaks <- if (method == "ale" && is.numeric(x_df[[x_name]])) {
            ale_breaks(x_df[[x_name]], n = n)
        } else {
            NULL
        }
        y_breaks <- if (method == "ale" && is.numeric(x_df[[y_name]])) {
            ale_breaks(x_df[[y_name]], n = n)
        } else {
            NULL
        }

        list(
            x_name = x_name,
            y_name = y_name,
            x_factor = is.factor(x_df[[x_name]]),
            y_factor = is.factor(x_df[[y_name]]),
            x_values = if (!is.null(x_breaks)) {
                ale_interval_centres(x_breaks)
            } else {
                curve_values(x_df[[x_name]], n = n)
            },
            y_values = if (!is.null(y_breaks)) {
                ale_interval_centres(y_breaks)
            } else {
                curve_values(x_df[[y_name]], n = n)
            },
            x_breaks = x_breaks,
            y_breaks = y_breaks
        )
    })
}


filter_ale_pair_specs <- function(pair_specs) {
    keep <- vapply(pair_specs, function(spec) {
        !spec$x_factor && !spec$y_factor
    }, logical(1))

    if (!all(keep)) {
        warning(
            "ALE currently supports numeric predictor pairs only. Ignoring pairs: ",
            paste(vapply(pair_specs[!keep], pair_label, character(1)),
                  collapse = ", "),
            call. = FALSE
        )
    }

    pair_specs[keep]
}


pair_label <- function(spec) {
    paste(spec$x_name, spec$y_name, sep = " vs ")
}


validate_pairs <- function(x_df, pairs = NULL) {
    nms <- names(x_df)

    if (length(nms) < 2L) {
        stop("x must contain at least two predictors for bivariate plots")
    }

    pair_list <- if (is.null(pairs)) {
        utils::combn(nms, 2, simplify = FALSE)
    } else if (is.numeric(pairs)) {
        list(pairs)
    } else if (is.character(pairs)) {
        list(pairs)
    } else if (is.list(pairs)) {
        pairs
    } else if (is.matrix(pairs) || is.data.frame(pairs)) {
        if (ncol(pairs) != 2L) {
            stop("pairs matrices and data frames must have exactly two columns")
        }

        split(
            as.data.frame(pairs, stringsAsFactors = FALSE),
            seq_len(nrow(pairs))
        )
    } else {
        stop(
            "pairs must be NULL, a character vector, a list, a matrix, or a data frame"
        )
    }

    if (!length(pair_list)) {
        stop("pairs must contain at least one predictor pair")
    }

    lapply(pair_list, function(pair) {
        normalize_pair_selection(pair, nms = nms)
    })
}


normalize_pair_selection <- function(pair, nms) {
    raw_pair <- unname(unlist(pair, use.names = FALSE))

    if (length(raw_pair) != 2L) {
        stop("Each predictor pair must contain exactly two entries")
    }

    if (is.numeric(raw_pair)) {
        if (any(!is.finite(raw_pair)) ||
            any(raw_pair != as.integer(raw_pair))) {
            stop("Numeric predictor pairs must use whole-number indices")
        }

        index_pair <- as.integer(raw_pair)
        if (any(index_pair < 1L | index_pair > length(nms))) {
            stop(
                "Predictor indices must be between 1 and ",
                length(nms)
            )
        }

        name_pair <- nms[index_pair]
    } else {
        name_pair <- as.character(raw_pair)
        missing <- setdiff(name_pair, nms)
        if (length(missing)) {
            stop(
                "Unknown predictor(s) in pairs: ",
                paste(missing, collapse = ", ")
            )
        }
    }

    if (name_pair[1] == name_pair[2]) {
        stop("Predictor pairs must contain two different entries")
    }

    name_pair
}


ale_interval_centres <- function(breaks) {
    if (length(breaks) < 2L) {
        return(breaks)
    }

    (breaks[-1L] + breaks[-length(breaks)]) / 2
}


build_profile_surface_table <- function(model, reference_row, spec, fun,
                                        response, ...) {
    grid <- build_bivariate_grid(
        reference_row = reference_row,
        x_name = spec$x_name,
        x_values = spec$x_values,
        y_name = spec$y_name,
        y_values = spec$y_values
    )

    data.frame(
        x = grid[[spec$x_name]],
        y = grid[[spec$y_name]],
        z = extract_prediction_vector(
            fun(model, grid, ...),
            n = nrow(grid),
            response = response
        )
    )
}


build_pdp_surface_table <- function(model, background_rows, spec, fun,
                                    response, ...) {
    grid <- build_bivariate_stack(
        background_rows = background_rows,
        x_name = spec$x_name,
        x_values = spec$x_values,
        y_name = spec$y_name,
        y_values = spec$y_values
    )
    cell_index <- build_bivariate_index_grid(spec$x_values, spec$y_values)
    n_cells <- nrow(cell_index)

    prediction_matrix <- matrix(
        extract_prediction_vector(
            fun(model, grid, ...),
            n = nrow(grid),
            response = response
        ),
        nrow = nrow(background_rows),
        ncol = n_cells,
        byrow = TRUE
    )

    data.frame(
        x = spec$x_values[cell_index$x],
        y = spec$y_values[cell_index$y],
        z = colMeans(prediction_matrix)
    )
}


build_ale_surface_table <- function(model, ale_rows, spec, fun, response, ...) {
    if (is.null(spec$x_breaks) || is.null(spec$y_breaks)) {
        stop("ALE surface specs must include numeric breakpoints")
    }

    if (length(spec$x_breaks) < 2L || length(spec$y_breaks) < 2L) {
        return(data.frame(
            x = spec$x_values[1],
            y = spec$y_values[1],
            z = 0,
            xmin = spec$x_values[1],
            xmax = spec$x_values[1],
            ymin = spec$y_values[1],
            ymax = spec$y_values[1]
        ))
    }

    x_interval <- findInterval(
        ale_rows[[spec$x_name]],
        vec = spec$x_breaks,
        rightmost.closed = TRUE,
        all.inside = TRUE
    )
    y_interval <- findInterval(
        ale_rows[[spec$y_name]],
        vec = spec$y_breaks,
        rightmost.closed = TRUE,
        all.inside = TRUE
    )

    lower_lower <- ale_rows
    lower_upper <- ale_rows
    upper_lower <- ale_rows
    upper_upper <- ale_rows

    lower_lower[[spec$x_name]] <- spec$x_breaks[x_interval]
    lower_upper[[spec$x_name]] <- spec$x_breaks[x_interval]
    upper_lower[[spec$x_name]] <- spec$x_breaks[x_interval + 1L]
    upper_upper[[spec$x_name]] <- spec$x_breaks[x_interval + 1L]

    lower_lower[[spec$y_name]] <- spec$y_breaks[y_interval]
    upper_lower[[spec$y_name]] <- spec$y_breaks[y_interval]
    lower_upper[[spec$y_name]] <- spec$y_breaks[y_interval + 1L]
    upper_upper[[spec$y_name]] <- spec$y_breaks[y_interval + 1L]

    diffs <- extract_prediction_vector(
        fun(model, upper_upper, ...),
        n = nrow(upper_upper),
        response = response
    ) - extract_prediction_vector(
        fun(model, lower_upper, ...),
        n = nrow(lower_upper),
        response = response
    ) - extract_prediction_vector(
        fun(model, upper_lower, ...),
        n = nrow(upper_lower),
        response = response
    ) + extract_prediction_vector(
        fun(model, lower_lower, ...),
        n = nrow(lower_lower),
        response = response
    )

    nx <- length(spec$x_breaks) - 1L
    ny <- length(spec$y_breaks) - 1L
    cell_id <- (x_interval - 1L) * ny + y_interval
    counts <- tabulate(cell_id, nbins = nx * ny)
    mean_diffs <- rep(NA_real_, length(counts))
    keep <- counts > 0L
    mean_diffs[keep] <- vapply(which(keep), function(index) {
        mean(diffs[cell_id == index])
    }, numeric(1))

    counts <- matrix(counts, nrow = nx, ncol = ny, byrow = TRUE)
    mean_diffs <- matrix(mean_diffs, nrow = nx, ncol = ny, byrow = TRUE)
    filled_diffs <- fill_missing_ale_cells(mean_diffs)
    raw_surface <- accumulate_ale_surface(filled_diffs)

    row_means <- vapply(seq_len(nx), function(index) {
        if (!sum(counts[index, ])) {
            return(0)
        }

        stats::weighted.mean(raw_surface[index, ], w = counts[index, ])
    }, numeric(1))
    col_means <- vapply(seq_len(ny), function(index) {
        if (!sum(counts[, index])) {
            return(0)
        }

        stats::weighted.mean(raw_surface[, index], w = counts[, index])
    }, numeric(1))
    overall_mean <- if (sum(counts)) {
        stats::weighted.mean(as.vector(raw_surface), w = as.vector(counts))
    } else {
        0
    }

    centred_surface <- sweep(raw_surface, 1, row_means, FUN = "-")
    centred_surface <- sweep(centred_surface, 2, col_means, FUN = "-")
    centred_surface <- centred_surface + overall_mean

    cell_index <- build_bivariate_index_grid(spec$x_values, spec$y_values)

    data.frame(
        x = spec$x_values[cell_index$x],
        y = spec$y_values[cell_index$y],
        z = centred_surface[cbind(cell_index$x, cell_index$y)],
        count = counts[cbind(cell_index$x, cell_index$y)],
        xmin = spec$x_breaks[cell_index$x],
        xmax = spec$x_breaks[cell_index$x + 1L],
        ymin = spec$y_breaks[cell_index$y],
        ymax = spec$y_breaks[cell_index$y + 1L]
    )
}


fill_missing_ale_cells <- function(values) {
    if (!anyNA(values)) {
        return(values)
    }

    filled <- values

    repeat {
        missing <- which(is.na(filled), arr.ind = TRUE)
        if (!nrow(missing)) {
            break
        }

        changed <- FALSE
        for (index in seq_len(nrow(missing))) {
            row_index <- missing[index, 1]
            col_index <- missing[index, 2]
            row_span <- seq(max(1L, row_index - 1L), min(nrow(filled), row_index + 1L))
            col_span <- seq(max(1L, col_index - 1L), min(ncol(filled), col_index + 1L))
            neighbours <- filled[row_span, col_span]
            neighbours <- neighbours[!is.na(neighbours)]

            if (length(neighbours)) {
                filled[row_index, col_index] <- mean(neighbours)
                changed <- TRUE
            }
        }

        if (!changed) {
            break
        }
    }

    filled[is.na(filled)] <- 0
    filled
}


accumulate_ale_surface <- function(diffs) {
    cumulative_cols <- diffs
    for (index in seq_len(ncol(diffs))) {
        cumulative_cols[, index] <- cumsum(diffs[, index])
    }

    cumulative_rows <- diffs
    for (index in seq_len(nrow(diffs))) {
        cumulative_rows[index, ] <- cumsum(diffs[index, ])
    }

    full_cumsum <- cumulative_cols
    for (index in seq_len(nrow(cumulative_cols))) {
        full_cumsum[index, ] <- cumsum(cumulative_cols[index, ])
    }

    full_cumsum - 0.5 * cumulative_cols - 0.5 * cumulative_rows + 0.25 * diffs
}


build_bivariate_grid <- function(reference_row, x_name, x_values, y_name,
                                 y_values) {
    index_grid <- build_bivariate_index_grid(x_values, y_values)

    grid <- reference_row[rep(1L, nrow(index_grid)), , drop = FALSE]
    grid[[x_name]] <- coerce_curve_values(
        reference = reference_row[[x_name]],
        values = x_values[index_grid$x]
    )
    grid[[y_name]] <- coerce_curve_values(
        reference = reference_row[[y_name]],
        values = y_values[index_grid$y]
    )

    grid
}


build_bivariate_index_grid <- function(x_values, y_values) {
    expand.grid(
        x = seq_along(x_values),
        y = seq_along(y_values),
        KEEP.OUT.ATTRS = FALSE
    )
}


build_bivariate_stack <- function(background_rows, x_name, x_values, y_name,
                                  y_values) {
    index_grid <- build_bivariate_index_grid(x_values, y_values)
    grid <- background_rows[
        rep(seq_len(nrow(background_rows)), each = nrow(index_grid)),
        ,
        drop = FALSE
    ]

    grid[[x_name]] <- coerce_curve_values(
        reference = background_rows[[x_name]],
        values = rep(x_values[index_grid$x], times = nrow(background_rows))
    )
    grid[[y_name]] <- coerce_curve_values(
        reference = background_rows[[y_name]],
        values = rep(y_values[index_grid$y], times = nrow(background_rows))
    )

    grid
}


coerce_curve_values <- function(reference, values) {
    if (is.factor(reference)) {
        return(factor(
            as.character(values),
            levels = levels(reference),
            ordered = is.ordered(reference)
        ))
    }

    as.vector(values)
}


response_breaks <- function(z_limits, bins) {
    breaks <- pretty(z_limits, n = bins)
    breaks <- breaks[breaks >= z_limits[1] & breaks <= z_limits[2]]

    if (length(breaks) < 2L) {
        seq(z_limits[1], z_limits[2], length.out = bins + 1L)
    } else {
        breaks
    }
}


plot_2D <- function(df, x_name, y_name, x_factor, y_factor, plot_type, z_name,
                    palette, z_limits, contour_breaks, rug_df = NULL) {

    if (plot_type == "contour" && (x_factor || y_factor)) {
        stop(
            "plot_type = \"contour\" is only supported for numeric predictor pairs"
        )
    }

    plt <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))

    if (plot_type == "contour") {
        plt <- plt +
            ggplot2::geom_contour_filled(
                ggplot2::aes(z = z),
                breaks = contour_breaks
            ) +
            ggplot2::geom_contour(
                ggplot2::aes(z = z),
                breaks = contour_breaks,
                color = "grey30",
                linewidth = 0.25
            ) +
            response_fill_scale(
                palette = palette,
                z_limits = z_limits,
                discrete = TRUE,
                n_levels = max(length(contour_breaks) - 1L, 1L)
            )
    } else if (!x_factor && !y_factor && has_rect_bounds(df)) {
        plt <- plt +
            ggplot2::geom_rect(
                ggplot2::aes(
                    xmin = get("xmin"),
                    xmax = get("xmax"),
                    ymin = get("ymin"),
                    ymax = get("ymax"),
                    fill = get("z")
                )
            ) +
            response_fill_scale(
                palette = palette,
                z_limits = z_limits,
                discrete = FALSE
            )
    } else if (!x_factor && !y_factor) {
        plt <- plt +
            ggplot2::geom_raster(ggplot2::aes(fill = z)) +
            response_fill_scale(
                palette = palette,
                z_limits = z_limits,
                discrete = FALSE
            )
    } else {
        plt <- plt +
            ggplot2::geom_tile(ggplot2::aes(fill = z), color = "white") +
            response_fill_scale(
                palette = palette,
                z_limits = z_limits,
                discrete = FALSE
            )
    }

    if (!is.null(rug_df) && nrow(rug_df)) {
        plt <- plt + ggplot2::geom_rug(
            data = rug_df,
            ggplot2::aes(x = x, y = y),
            inherit.aes = FALSE,
            sides = "bl",
            color = "grey30",
            alpha = 0.35
        )
    }

    plt <- plt +
        ggplot2::theme_bw() +
        ggplot2::labs(
            x = x_name,
            y = y_name,
            fill = z_name
        ) +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            plot.title = ggplot2::element_blank()
        )

    if (!x_factor && !y_factor) {
        plt <- plt + ggplot2::coord_cartesian(expand = FALSE)
    }

    plt
}


has_rect_bounds <- function(df) {
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(df))
}


response_fill_scale <- function(palette, z_limits, discrete, n_levels = NULL) {
    if (is_viridis_palette(palette)) {
        if (discrete) {
            return(ggplot2::scale_fill_viridis_d(option = palette))
        }

        return(ggplot2::scale_fill_viridis_c(
            option = palette,
            limits = z_limits
        ))
    }

    if (discrete) {
        values <- if (is.null(n_levels)) palette else {
            grDevices::colorRampPalette(palette)(n_levels)
        }

        return(ggplot2::scale_fill_manual(values = values))
    }

    ggplot2::scale_fill_gradientn(
        colours = palette,
        limits = z_limits
    )
}


default_ale_palette <- function() {
    c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
      "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
}


is_viridis_palette <- function(palette) {
    is.character(palette) &&
        length(palette) == 1L &&
        palette %in% c(
            "A", "B", "C", "D", "E", "F", "G", "H",
            "magma", "inferno", "plasma", "viridis",
            "cividis", "rocket", "mako", "turbo"
        )
}


ale_surface_limits <- function(values, padding = 0.02) {
    max_abs <- max(abs(values), na.rm = TRUE)
    if (!is.finite(max_abs)) {
        stop("Predictions must be finite to compute plot limits")
    }

    pad <- if (max_abs == 0) {
        padding
    } else {
        max_abs * padding
    }

    c(-max_abs - pad, max_abs + pad)
}


sample_bivariate_rug_data <- function(x_df, x_name, y_name, max_n = 5000L) {
    dat <- x_df[, c(x_name, y_name), drop = FALSE]
    names(dat) <- c("x", "y")
    dat <- dat[stats::complete.cases(dat), , drop = FALSE]

    if (!nrow(dat)) {
        return(data.frame(x = numeric(0), y = numeric(0)))
    }

    if (nrow(dat) > max_n) {
        index <- unique(round(seq(1, nrow(dat), length.out = max_n)))
        dat <- dat[index, , drop = FALSE]
    }

    dat
}


combine_bivariate_plots <- function(plots, nrow, ncol) {
    if (length(plots) == 1L) {
        return(plots[[1]])
    }

    legend <- cowplot::get_legend(
        plots[[1]] +
            ggplot2::theme(legend.position = "right")
    )
    panel <- cowplot::plot_grid(
        plotlist = lapply(plots, function(plot) {
            plot + ggplot2::theme(legend.position = "none")
        }),
        nrow = nrow,
        ncol = ncol
    )

    cowplot::plot_grid(
        panel,
        legend,
        nrow = 1,
        rel_widths = c(1, 0.12)
    )
}


plot_surface_3D <- function(spec, df, zlab) {
    if (spec$x_factor || spec$y_factor) {
        stop("plot_type = \"surface\" is only supported for numeric predictor pairs")
    }

    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("The plotly package must be installed for plot_type = \"surface\"")
    }

    z_matrix <- matrix(
        df$z,
        nrow = length(spec$x_values),
        ncol = length(spec$y_values)
    )
    z_matrix <- t(z_matrix)

    plotly::layout(
        plotly::plot_ly(
            x = spec$x_values,
            y = spec$y_values,
            z = z_matrix,
            type = "surface"
        ),
        scene = list(
            xaxis = list(title = spec$x_name),
            yaxis = list(title = spec$y_name),
            zaxis = list(title = zlab)
        )
    )
}
