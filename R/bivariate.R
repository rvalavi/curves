#' Bivariate response surface plot
#'
#' This function generates bivariate response plots for a given model by
#' varying two predictors at a time while keeping others constant.
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
#'   variable (default: 40).
#' @param plot_type Character, plot type. Use `"heatmap"` for a static surface,
#'   `"contour"` for filled contours, or `"surface"` for an interactive 3D
#'   surface. The 3D surface requires the suggested `plotly` package and a
#'   single numeric predictor pair.
#' @param zlab Character, label for the response legend or z-axis (default:
#'   `"Prediction"`).
#' @param bins Integer, number of contour bins for `"heatmap"` overlays and
#'   `"contour"` plots.
#' @param palette Character vector of colours used for the response scale.
#' @param nrows Integer, number of rows in the plot grid. If `NULL`, it is
#'   automatically determined.
#' @param ncols Integer, number of columns in the plot grid. If `NULL`, it is
#'   automatically determined.
#'
#' @return A `ggplot2` object for static plot types or a `plotly` widget for
#'   `plot_type = "surface"`.
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
                      plot_type = c("heatmap", "contour", "surface"),
                      zlab = "Prediction", bins = 8,
                      palette = grDevices::hcl.colors(
                          12,
                          "YlOrRd",
                          rev = TRUE
                      ),
                      nrows = NULL, ncols = NULL) {

    plot_type <- match.arg(plot_type)

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
        x_source <- x
    } else {
        x_source <- predict_data
    }

    if (!is.numeric(n) || length(n) != 1L || n < 2) {
        stop("n must be a single number greater than or equal to 2")
    }

    if (!is.numeric(bins) || length(bins) != 1L || bins < 2) {
        stop("bins must be a single number greater than or equal to 2")
    }

    x_df <- validate_predictors(x_source, sample_size = 5000L)
    pair_specs <- build_pair_specs(x_df, pairs = pairs, n = n)
    npairs <- length(pair_specs)

    if (plot_type == "surface" && npairs != 1L) {
        stop("plot_type = \"surface\" requires a single predictor pair")
    }

    reference_row <- build_reference_row(x_df)
    tables <- lapply(pair_specs, function(spec) {
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
            z = as.numeric(fun(model, grid, ...))
        )
    })

    if (plot_type == "surface") {
        return(plot_surface_3D(
            spec = pair_specs[[1]],
            df = tables[[1]],
            zlab = zlab
        ))
    }

    z_limits <- curve_limits(unlist(lapply(tables, function(table) table$z)),
                             padding = 0.02)
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
                contour_breaks = contour_breaks
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


build_pair_specs <- function(x_df, pairs, n) {
    pair_list <- validate_pairs(x_df, pairs = pairs)

    lapply(pair_list, function(pair) {
        x_name <- pair[1]
        y_name <- pair[2]

        list(
            x_name = x_name,
            y_name = y_name,
            x_factor = is.factor(x_df[[x_name]]),
            y_factor = is.factor(x_df[[y_name]]),
            x_values = curve_values(x_df[[x_name]], n = n),
            y_values = curve_values(x_df[[y_name]], n = n)
        )
    })
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


build_bivariate_grid <- function(reference_row, x_name, x_values, y_name,
                                 y_values) {
    index_grid <- expand.grid(
        x = seq_along(x_values),
        y = seq_along(y_values),
        KEEP.OUT.ATTRS = FALSE
    )

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
                    palette, z_limits, contour_breaks) {
    pair_label <- paste(x_name, y_name, sep = " vs ")

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
            )
    } else if (!x_factor && !y_factor) {
        plt <- plt +
            ggplot2::geom_raster(ggplot2::aes(fill = z)) +
            ggplot2::geom_contour(
                ggplot2::aes(z = z),
                breaks = contour_breaks,
                color = "white",
                alpha = 0.7,
                linewidth = 0.25
            ) +
            ggplot2::scale_fill_gradientn(
                colours = palette,
                limits = z_limits
            )
    } else {
        plt <- plt +
            ggplot2::geom_tile(ggplot2::aes(fill = z), color = "white") +
            ggplot2::scale_fill_gradientn(
                colours = palette,
                limits = z_limits
            )
    }

    plt <- plt +
        ggplot2::theme_bw() +
        ggplot2::labs(
            x = x_name,
            y = y_name,
            fill = z_name,
            title = pair_label
        ) +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(size = 10)
        )

    if (!x_factor && !y_factor) {
        plt <- plt + ggplot2::coord_cartesian(expand = FALSE)
    }

    plt
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
