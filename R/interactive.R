#' Interactive map-linked response curves
#'
#' Launch a Shiny explorer that links a prediction raster to univariate
#' response curves. Clicking a map cell overlays the clicked site's predictor
#' values on the response-curve panels, similar to the Maxent-style diagnostic
#' view shown by Elith et al. (2010).
#'
#' @param model A fitted model object that supports prediction.
#' @param map A single-layer `terra::SpatRaster` containing the predicted
#'   surface shown on the map.
#' @param predictors A `terra::SpatRaster` containing the predictor layers used
#'   to extract covariate values at the clicked map cell.
#' @param predict_data Optional data frame or raster used to build the response
#'   curves. If `NULL`, `predictors` is used.
#' @param fun A function used to generate predictions from the model. Defaults
#'   to `predict`.
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
#' @param ylab Character, label for the response scale (default:
#'   `"Prediction"`).
#' @param rug Logical, whether to include rug marks for numeric predictors
#'   (default: `TRUE`).
#' @param ylim Numeric vector of length 2, specifying the limits of the
#'   response axis. If `NULL`, limits are computed from the fitted curves.
#' @param color Character, colour of the fitted response curves.
#' @param response Optional column name or index to select when `fun` returns
#'   multiple predictions per row. If `NULL` and exactly two prediction columns
#'   are returned, the second column is used.
#' @param nrows Integer, number of rows in the response-curve grid. If `NULL`,
#'   it is computed automatically.
#' @param ncols Integer, number of columns in the response-curve grid. Defaults
#'   to `2` so the response-curve panel stays closer to the map height.
#' @param method Character, the curve type to plot. `"profile"` uses a single
#'   reference profile, `"pdp"` averages over sampled predictor rows,
#'   `"ice"` draws individual conditional expectation curves, `"ice+pdp"`
#'   overlays the averaged PDP on top of the ICE curves, and `"ale"` draws
#'   accumulated local effects curves for numeric predictors.
#' @param selected_color Character, colour used for the clicked-site marker on
#'   the map and response curves.
#' @param map_palette Character vector of colours used to draw the prediction
#'   map.
#' @param map_title Character, title shown above the map.
#' @param launch Logical, whether to launch the Shiny app immediately.
#'
#' @return A `shiny.appobj` object. If `launch = TRUE`, the app is also run and
#'   returned invisibly after it closes.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE) &&
#'     requireNamespace("shiny", quietly = TRUE)) {
#'   r <- terra::rast(
#'     ncols = 10,
#'     nrows = 10,
#'     nlyrs = 2,
#'     xmin = 0,
#'     xmax = 1,
#'     ymin = 0,
#'     ymax = 1
#'   )
#'   values <- cbind(
#'     rep(seq(0, 1, length.out = 10), each = 10),
#'     rep(seq(0, 1, length.out = 10), times = 10)
#'   )
#'   terra::values(r) <- values
#'   names(r) <- c("x1", "x2")
#'
#'   dat <- terra::as.data.frame(r)
#'   dat$y <- 1 + 2 * dat$x1 - dat$x2
#'   fit <- lm(y ~ x1 + x2, data = dat)
#'
#'   pred_map <- r[[1]]
#'   terra::values(pred_map) <- dat$y
#'   names(pred_map) <- "prediction"
#'
#'   app <- interactive_map_curves(
#'     fit,
#'     map = pred_map,
#'     predictors = r,
#'     launch = FALSE
#'   )
#'   invisible(app)
#' }
interactive_map_curves <- function(model, map, predictors,
                                   predict_data = NULL,
                                   fun = stats::predict, ...,
                                   n = 100,
                                   background_n = n,
                                   interval = c("none", "quantile"),
                                   interval_level = 0.8,
                                   ylab = "Prediction",
                                   rug = TRUE,
                                   ylim = NULL,
                                   color = "#ff5a36",
                                   response = NULL,
                                   nrows = NULL,
                                   ncols = 2,
                                   method = c(
                                       "profile",
                                       "pdp",
                                       "ice",
                                       "ice+pdp",
                                       "ale"
                                   ),
                                   selected_color = "deepskyblue3",
                                   map_palette = grDevices::hcl.colors(
                                       64,
                                       "Spectral",
                                       rev = TRUE
                                   ),
                                   map_title = "Prediction map",
                                   launch = interactive()) {

    method <- match.arg(method)
    interval <- match.arg(interval)
    map_height <- 780L

    if (interval != "none" && method != "pdp") {
        stop("interval is only supported when method = \"pdp\"")
    }

    if (interval == "quantile") {
        interval_level <- validate_interval_level(interval_level)
    }

    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop(
            "Package `shiny` must be installed to use interactive_map_curves().",
            call. = FALSE
        )
    }

    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(
            "Package `terra` must be installed to use interactive_map_curves().",
            call. = FALSE
        )
    }

    validate_interactive_map_inputs(map, predictors)

    curve_source <- if (is.null(predict_data)) predictors else predict_data
    curve_data <- prepare_interactive_curve_data(
        model = model,
        x_source = curve_source,
        fun = fun,
        n = n,
        background_n = background_n,
        interval = interval,
        interval_level = interval_level,
        ylab = ylab,
        rug = rug,
        ylim = ylim,
        color = color,
        response = response,
        nrows = nrows,
        ncols = ncols,
        method = method,
        ...
    )

    missing_layers <- setdiff(
        names(curve_data$predictor_specs),
        names(predictors)
    )
    if (length(missing_layers)) {
        stop(
            "predictors must contain layers for every plotted variable. Missing layers: ",
            paste(missing_layers, collapse = ", ")
        )
    }

    curve_height <- interactive_curve_plot_height(
        map_height = map_height
    )
    curve_style <- interactive_curve_style(
        nrows = curve_data$nrows,
        map_height = map_height
    )

    ui <- shiny::fluidPage(
        shiny::tags$head(
            shiny::tags$style(shiny::HTML(
                "
                .curves-map-panel {
                    padding-right: 8px;
                }
                .curves-side-panel {
                    padding-left: 8px;
                }
                .curves-info-box {
                    background: #f4f4f4;
                    border: 1px solid #d7d7d7;
                    border-radius: 6px;
                    margin-top: 12px;
                    padding: 10px 12px;
                }
                .curves-info-title {
                    font-weight: 700;
                    margin-bottom: 4px;
                }
                .curves-info-line {
                    margin: 0;
                }
                "
            ))
        ),
        shiny::fluidRow(
            shiny::column(
                width = 7,
                class = "curves-map-panel",
                shiny::plotOutput(
                    "curves_map",
                    click = "curves_map_click",
                    height = sprintf("%spx", map_height)
                ),
                shiny::uiOutput("curves_selection_info")
            ),
            shiny::column(
                width = 5,
                class = "curves-side-panel",
                shiny::plotOutput(
                    "curves_plot",
                    height = sprintf("%spx", curve_height)
                )
            )
        )
    )

    server <- function(input, output, session) {
        selected_site <- shiny::reactiveVal(NULL)

        shiny::observeEvent(input$curves_map_click, {
            selected_site(resolve_map_selection(
                map = map,
                predictors = predictors,
                x_coord = input$curves_map_click$x,
                y_coord = input$curves_map_click$y
            ))
        })

        output$curves_map <- shiny::renderPlot({
            plot_prediction_map(
                map = map,
                selection = selected_site(),
                palette = map_palette,
                title = map_title,
                marker_color = selected_color
            )
        })

        output$curves_selection_info <- shiny::renderUI({
            build_selection_info(selected_site(), ylab = ylab)
        })

        output$curves_plot <- shiny::renderPlot({
            selected <- selected_site()

            plots <- lapply(curve_data$predictor_specs, function(spec) {
                table <- curve_data$tables[[spec$name]]
                plt <- plot_1D(
                    df = table$curves,
                    dat = if (curve_data$rug && !spec$is_factor) {
                        sample_rug_values(curve_data$x_df, spec$name)
                    } else {
                        NULL
                    },
                    fact = spec$is_factor,
                    ordered_factor = spec$is_ordered,
                    rug = curve_data$rug && !spec$is_factor,
                    se = FALSE,
                    x_name = spec$name,
                    y_name = curve_data$ylab,
                    color = curve_data$color,
                    ylim = curve_data$limits,
                    curve_alpha = if (
                        curve_data$method %in% c("ice", "ice+pdp")
                    ) {
                        0.15
                    } else {
                        1
                    },
                    curve_linewidth = if (
                        curve_data$method %in% c("ice", "ice+pdp")
                    ) {
                        0.35
                    } else {
                        0.7
                    },
                    summary_df = table$summary,
                    summary_linewidth = 1
                )

                plt <- style_interactive_curve_plot(
                    plt,
                    title = spec$name,
                    style = curve_style
                )
                add_selected_site_marker(
                    plot = plt,
                    spec = spec,
                    table = table,
                    selected = selected,
                    selected_color = selected_color
                )
            })

            cowplot::plot_grid(
                plotlist = plots,
                nrow = curve_data$nrows,
                ncol = curve_data$ncols
            )
        }, res = 96)
    }

    app <- shiny::shinyApp(ui = ui, server = server)

    if (launch) {
        shiny::runApp(app)
        return(invisible(app))
    }

    app
}


validate_interactive_map_inputs <- function(map, predictors) {
    if (!.is_rast(map)) {
        stop("map must be a terra::SpatRaster")
    }

    if (!.is_rast(predictors)) {
        stop("predictors must be a terra::SpatRaster")
    }

    if (terra::nlyr(map) != 1L) {
        stop("map must contain exactly one layer")
    }

    if (terra::nrow(map) != terra::nrow(predictors) ||
        terra::ncol(map) != terra::ncol(predictors)) {
        stop("map and predictors must have the same number of rows and columns")
    }

    if (!isTRUE(all.equal(terra::ext(map), terra::ext(predictors)))) {
        stop("map and predictors must share the same extent")
    }

    if (!identical(
        terra::crs(map, proj = TRUE),
        terra::crs(predictors, proj = TRUE)
    )) {
        stop("map and predictors must use the same coordinate reference system")
    }

    predictor_names <- names(predictors)
    if (!length(predictor_names) || any(!nzchar(predictor_names))) {
        stop("predictors must contain named layers")
    }
}


prepare_interactive_curve_data <- function(model, x_source, fun, ...,
                                           n, background_n, interval,
                                           interval_level,
                                           ylab, rug, ylim, color,
                                           response, nrows, ncols, method) {
    n <- validate_curve_n(n)
    background_n <- validate_background_n(background_n)

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

    if (is.null(ncols)) {
        ncols <- min(2L, max(1L, nvars))
    }
    if (!is.numeric(ncols) || length(ncols) != 1L || is.na(ncols) || ncols < 1) {
        stop("ncols must be a single positive integer")
    }
    ncols <- as.integer(ncols)

    if (is.null(nrows)) {
        nrows <- ceiling(nvars / ncols)
    }
    if (!is.numeric(nrows) || length(nrows) != 1L || is.na(nrows) || nrows < 1) {
        stop("nrows must be a single positive integer")
    }
    nrows <- as.integer(nrows)

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

    list(
        x_df = x_df,
        predictor_specs = predictor_specs,
        tables = tables,
        limits = limits,
        nrows = nrows,
        ncols = ncols,
        ylab = ylab,
        rug = rug,
        color = color,
        method = method
    )
}


interactive_curve_plot_height <- function(map_height) {
    as.integer(map_height)
}


interactive_curve_style <- function(nrows, map_height) {
    panel_height <- map_height / max(1L, nrows)

    list(
        title_size = max(8, min(11, panel_height / 16)),
        axis_text_size = max(6.5, min(10, panel_height / 22)),
        margin = if (panel_height < 150) 2 else 4
    )
}


resolve_map_selection <- function(map, predictors, x_coord, y_coord) {
    if (is.null(x_coord) || is.null(y_coord)) {
        return(NULL)
    }

    cell <- terra::cellFromXY(
        map,
        matrix(c(x_coord, y_coord), ncol = 2)
    )[1]

    if (is.na(cell)) {
        return(NULL)
    }

    prediction_df <- extract_raster_values(
        map,
        matrix(c(x_coord, y_coord), ncol = 2)
    )
    predictor_df <- extract_raster_values(
        predictors,
        matrix(c(x_coord, y_coord), ncol = 2)
    )

    if (!nrow(prediction_df) || !nrow(predictor_df)) {
        return(NULL)
    }

    prediction <- as.numeric(prediction_df[[1]][1])
    if (!is.finite(prediction)) {
        return(NULL)
    }

    values <- predictor_df[1, , drop = FALSE]

    list(
        x = x_coord,
        y = y_coord,
        cell = as.integer(cell),
        prediction = prediction,
        values = as.list(values)
    )
}


extract_raster_values <- function(x, locations) {
    extracted <- terra::extract(x, locations)
    extracted <- as.data.frame(extracted, check.names = FALSE)

    if ("ID" %in% names(extracted)) {
        extracted <- extracted[, names(extracted) != "ID", drop = FALSE]
    }

    extracted
}


plot_prediction_map <- function(map, selection, palette, title, marker_color) {
    terra::plot(
        map,
        col = palette,
        colNA = "black",
        axes = FALSE,
        box = FALSE,
        mar = c(1.5, 1.5, 2.5, 5.5),
        main = title
    )

    if (!is.null(selection)) {
        graphics::points(
            selection$x,
            selection$y,
            pch = 21,
            bg = marker_color,
            col = "white",
            cex = 1.3,
            lwd = 1.1
        )
    }
}


build_selection_info <- function(selection, ylab) {
    if (is.null(selection)) {
        return(shiny::div(
            class = "curves-info-box",
            shiny::div("Selected site", class = "curves-info-title"),
            shiny::tags$p(
                "Click a non-missing raster cell to place site markers on the response curves.",
                class = "curves-info-line"
            )
        ))
    }

    shiny::div(
        class = "curves-info-box",
        shiny::div("Selected site", class = "curves-info-title"),
        shiny::tags$p(
            sprintf("x = %.4f, y = %.4f", selection$x, selection$y),
            class = "curves-info-line"
        ),
        shiny::tags$p(
            sprintf("%s = %.4f", ylab, selection$prediction),
            class = "curves-info-line"
        )
    )
}


style_interactive_curve_plot <- function(plot, title, style) {
    plot +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                hjust = 0.5,
                face = "bold",
                size = style$title_size
            ),
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(size = style$axis_text_size),
            panel.grid.minor = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(
                style$margin,
                style$margin + 2,
                style$margin,
                style$margin + 2
            )
        )
}


add_selected_site_marker <- function(plot, spec, table, selected,
                                     selected_color) {
    if (is.null(selected) || !spec$name %in% names(selected$values)) {
        return(plot)
    }

    selected_value <- selected$values[[spec$name]]
    if (length(selected_value) != 1L || is.na(selected_value)) {
        return(plot)
    }

    if (!spec$is_factor) {
        return(plot + ggplot2::geom_vline(
            xintercept = as.numeric(selected_value),
            color = selected_color,
            linewidth = 0.8,
            alpha = 0.95
        ))
    }

    marker_df <- selected_factor_marker_df(table, selected_value)
    if (is.null(marker_df)) {
        return(plot)
    }

    plot + ggplot2::geom_point(
        data = marker_df,
        ggplot2::aes(x = x, y = y),
        color = selected_color,
        size = 2.8,
        inherit.aes = FALSE
    )
}


selected_factor_marker_df <- function(table, selected_value) {
    curve_df <- if (!is.null(table$summary)) {
        table$summary
    } else if ("curve" %in% names(table$curves)) {
        average_curve_table(table$curves)
    } else {
        table$curves
    }

    if (!is.factor(curve_df$x)) {
        return(NULL)
    }

    selected_level <- as.character(selected_value)[1]
    keep <- as.character(curve_df$x) == selected_level

    if (!any(keep)) {
        return(NULL)
    }

    curve_df[which(keep)[1], c("x", "y"), drop = FALSE]
}
