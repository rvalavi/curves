#' Interactive map-linked response curve explorer
#'
#' Launch a Shiny app that links a prediction raster to model-agnostic
#' univariate response curves. Clicking a map cell extracts that cell's
#' predictor values and marks them on the curve panels, making spatial
#' predictions and curve-based model behaviour inspectable together.
#'
#' @details
#' The map displays the supplied prediction raster; the model is not refitted
#' inside the app. The curve panel is computed once with the same methods as
#' [univariate()]. For a fitted prediction function \eqn{\hat{f}} and plotted
#' predictor \eqn{x_j}, `"profile"` uses a reference-row curve
#' \eqn{\hat{f}(z, x_{-j}^{ref})}, `"ice"` plots sampled row-level curves
#' \eqn{\hat{f}(z, x_{-j}^{(i)})}, `"pdp"` plots their average
#' \deqn{\hat{f}_{j,PDP}(z) = \frac{1}{m}\sum_{i=1}^{m}
#'   \hat{f}(z, x_{-j}^{(i)}),}
#' and `"ale"` plots centred accumulated local effects using the same
#' univariate ALE definitions as [univariate()].
#' When a user clicks the map, the clicked cell's covariate value is overlaid on
#' each panel so the local environmental context can be compared with the
#' fitted response curve and the sampled predictor distribution.
#'
#' These curves are diagnostic summaries of a fitted prediction model. They do
#' not by themselves establish causal effects, and PDP/ICE/profile curves can
#' evaluate uncommon predictor combinations when predictors are dependent.
#'
#' @param model A fitted model object that supports prediction.
#' @param map A single-layer `terra::SpatRaster` containing the predicted
#'   surface shown on the map.
#' @param predictors A `terra::SpatRaster` containing the predictor layers used
#'   to extract covariate values at the clicked map cell.
#' @param predict_data Optional data frame or raster used to build the response
#'   curves. If `NULL`, `predictors` is used.
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
#'   accumulated local effects curves.
#' @param selected_color Character, colour used for the clicked-site marker on
#'   the map and response curves.
#' @param show_selected_ice Logical, whether to overlay the clicked site's
#'   ceteris paribus / ICE curve on the response panels using `selected_color`.
#'   Defaults to `TRUE`. Ignored when `method = "ale"`.
#' @param crosshair Logical, whether to draw dashed horizontal and vertical
#'   guide lines through the selected map cell. Defaults to `TRUE`.
#' @param map_palette Character vector of colours used to draw the prediction
#'   map.
#' @param map_title Character, title shown above the map.
#' @param launch Logical, whether to launch the Shiny app immediately.
#'
#' @return A `shiny.appobj` object. If `launch = TRUE`, the app is also run and
#'   returned invisibly after it closes.
#'
#' @references
#' Molnar, C. (2025). *Interpretable Machine Learning: A Guide for Making Black
#' Box Models Explainable* (3rd ed.). <https://christophm.github.io/interpretable-ml-book/>
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
#'   app <- mapcurve(
#'     fit,
#'     map = pred_map,
#'     predictors = r,
#'     launch = FALSE
#'   )
#'   invisible(app)
#' }
mapcurve <- function(model, map, predictors,
                     predict_data = NULL,
                     fun = NULL, ...,
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
                         "pdp",
                         "ice",
                         "ice+pdp",
                         "ale",
                         "profile"
                     ),
                     selected_color = "deepskyblue3",
                     show_selected_ice = TRUE,
                     crosshair = TRUE,
                     map_palette = grDevices::hcl.colors(
                         64,
                         "Inferno"
                     ),
                     map_title = "Prediction map",
                     launch = interactive()) {

    method <- match.arg(method)
    interval <- match.arg(interval)
    fun <- resolve_predict_fun(fun, env = parent.frame())
    map_height <- 780L

    if (interval != "none" && method != "pdp") {
        stop("interval is only supported when method = \"pdp\"")
    }

    if (interval == "quantile") {
        interval_level <- validate_interval_level(interval_level)
    }

    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop(
            "Package `shiny` must be installed to use mapcurve().",
            call. = FALSE
        )
    }

    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(
            "Package `terra` must be installed to use mapcurve().",
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
    app_theme <- interactive_app_theme()
    curve_style <- interactive_curve_style(
        nrows = curve_data$nrows,
        map_height = map_height,
        theme = app_theme
    )

    ui <- shiny::fluidPage(
        shiny::tags$head(
            shiny::tags$style(shiny::HTML(
                "
                body {
                    background: #252932;
                    color: #f1f3f5;
                    font-family: 'Avenir Next', 'Helvetica Neue', 'Segoe UI', sans-serif;
                }
                .container-fluid {
                    max-width: 1900px;
                    padding: 18px 24px 24px;
                }
                .curves-map-panel {
                    padding-right: 8px;
                    display: flex;
                    flex-direction: column;
                }
                .curves-side-panel {
                    padding-left: 8px;
                    display: flex;
                    flex-direction: column;
                }
                .curves-card {
                    background: #2d323a;
                    border: 1px solid #424851;
                    border-radius: 12px;
                    box-shadow: 0 10px 24px rgba(0, 0, 0, 0.18);
                }
                .curves-map-card {
                    padding: 12px 14px 10px;
                }
                .curves-curves-card {
                    padding: 8px 10px;
                }
                .curves-info-box,
                .curves-legend-panel {
                    background: #2d323a;
                    border: 1px solid #424851;
                    border-radius: 12px;
                    display: flex;
                    align-items: center;
                    gap: 16px;
                    flex-wrap: wrap;
                    margin-top: 12px;
                    padding: 7px 14px;
                    min-height: 54px;
                    box-sizing: border-box;
                }
                .curves-legend-panel {
                    align-content: center;
                    justify-content: flex-start;
                }
                .curves-legend-box {
                    display: flex;
                    align-items: center;
                    gap: 18px;
                    flex-wrap: wrap;
                    min-width: 0;
                }
                .curves-legend-item {
                    display: inline-flex;
                    align-items: center;
                    gap: 8px;
                    color: #d8dde3;
                    font-size: 0.92rem;
                    white-space: nowrap;
                }
                .curves-legend-swatch {
                    display: inline-block;
                    width: 28px;
                    height: 0;
                    border-top-width: 3px;
                    border-top-style: solid;
                    border-radius: 999px;
                    opacity: 0.95;
                }
                .curves-info-title {
                    font-weight: 700;
                    text-transform: uppercase;
                    letter-spacing: 0.08em;
                    color: #ffc857;
                    font-size: 0.8rem;
                    white-space: nowrap;
                }
                .curves-info-metric {
                    margin: 0;
                    color: #f1f3f5;
                    font-size: 1rem;
                    white-space: nowrap;
                }
                .curves-info-muted {
                    color: #c9d0d7;
                }
                .shiny-plot-output {
                    display: block;
                }
                "
            ))
        ),
        shiny::fluidRow(
            shiny::column(
                width = 5,
                class = "curves-map-panel",
                shiny::div(
                    class = "curves-card curves-map-card",
                    shiny::plotOutput(
                        "curves_map",
                        click = "curves_map_click",
                        height = sprintf("%spx", map_height)
                    )
                ),
                shiny::uiOutput("curves_selection_info")
            ),
            shiny::column(
                width = 7,
                class = "curves-side-panel",
                shiny::div(
                    class = "curves-card curves-curves-card",
                    shiny::plotOutput(
                        "curves_plot",
                        height = sprintf("%spx", curve_height)
                    )
                ),
                shiny::uiOutput("curves_plot_legend")
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
                marker_color = selected_color,
                crosshair = crosshair,
                theme = app_theme
            )
        })

        output$curves_selection_info <- shiny::renderUI({
            build_selection_info(selected_site(), ylab = ylab)
        })

        output$curves_plot_legend <- shiny::renderUI({
            build_curve_legend(
                method = curve_data$method,
                show_selected_ice = show_selected_ice,
                curve_color = curve_data$color,
                selected_color = selected_color
            )
        })

        output$curves_plot <- shiny::renderPlot({
            selected <- selected_site()
            selected_curves <- build_selected_site_curves(
                model = model,
                selected = selected,
                x_df = curve_data$x_df,
                predictor_specs = curve_data$predictor_specs,
                method = curve_data$method,
                fun = fun,
                response = response,
                show_selected_ice = show_selected_ice,
                predict_args = list(...)
            )
            plot_limits <- extend_curve_limits(
                limits = curve_data$limits,
                selected_curves = selected_curves
            )

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
                    ylim = plot_limits,
                    curve_color = if (identical(curve_data$method, "ice+pdp")) {
                        "gray70"
                    } else {
                        curve_data$color
                    },
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
                plt <- add_selected_ice_curve(
                    plot = plt,
                    spec = spec,
                    selected_curve = selected_curves[[spec$name]],
                    selected_color = selected_color
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


build_selected_site_curves <- function(model, selected, x_df, predictor_specs,
                                       method, fun, response,
                                       show_selected_ice, predict_args) {
    out <- stats::setNames(
        vector("list", length(predictor_specs)),
        names(predictor_specs)
    )

    if (is.null(selected) || !isTRUE(show_selected_ice) || method == "ale") {
        return(out)
    }

    reference_row <- coerce_selected_site_row(
        values = selected$values,
        template = x_df
    )

    if (is.null(reference_row)) {
        return(out)
    }

    for (spec_name in names(predictor_specs)) {
        spec <- predictor_specs[[spec_name]]
        out[[spec_name]] <- do.call(
            build_profile_curve_table,
            c(
                list(
                    model = model,
                    reference_row = reference_row,
                    column = spec$name,
                    values = spec$values,
                    fun = fun,
                    response = response
                ),
                predict_args
            )
        )
    }

    out
}


coerce_selected_site_row <- function(values, template) {
    cols <- lapply(names(template), function(name) {
        value <- values[[name]]
        column <- template[[name]]

        if (is.factor(column)) {
            return(factor(
                as.character(value)[1],
                levels = levels(column),
                ordered = is.ordered(column)
            ))
        }

        as.numeric(value)[1]
    })
    names(cols) <- names(template)

    row <- data.frame(cols, check.names = FALSE)
    if (!nrow(row) || !stats::complete.cases(row)) {
        return(NULL)
    }

    row
}


extend_curve_limits <- function(limits, selected_curves) {
    selected_values <- unlist(
        lapply(selected_curves, function(df) {
            if (is.null(df) || !"y" %in% names(df)) {
                return(numeric(0))
            }

            df$y
        }),
        use.names = FALSE
    )

    selected_values <- selected_values[is.finite(selected_values)]
    if (!length(selected_values)) {
        return(limits)
    }

    c(
        min(limits[1], min(selected_values)),
        max(limits[2], max(selected_values))
    )
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
    as.integer(map_height + 8L)
}


interactive_app_theme <- function() {
    list(
        map_bg = "#252932",
        map_na = "#252932",
        map_text = "#f1f3f5",
        plot_bg = "#ffffff",
        plot_panel_bg = "#ffffff",
        plot_border = "#d9d9d9",
        plot_grid = "#e8e8e8",
        plot_text = "#20252b",
        plot_muted = "#58616a"
    )
}


interactive_curve_style <- function(nrows, map_height, theme) {
    panel_height <- map_height / max(1L, nrows)

    list(
        title_size = max(8, min(11, panel_height / 16)),
        axis_text_size = max(6.5, min(10, panel_height / 22)),
        margin = if (panel_height < 150) 2 else 4,
        family = "sans",
        title_color = theme$plot_text,
        axis_color = theme$plot_muted,
        plot_bg = theme$plot_bg,
        panel_bg = theme$plot_panel_bg,
        panel_border = theme$plot_border,
        grid_color = theme$plot_grid
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


plot_prediction_map <- function(map, selection, palette, title, marker_color,
                                crosshair, theme) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar), add = TRUE)

    graphics::par(
        bg = theme$map_bg,
        fg = theme$map_text,
        family = "sans",
        col.main = theme$map_text,
        cex.main = 1.35,
        font.main = 2
    )

    terra::plot(
        map,
        col = palette,
        colNA = theme$map_na,
        axes = FALSE,
        box = FALSE,
        mar = c(1.4, 1.4, 2.2, 5.1),
        main = title
    )

    if (!is.null(selection)) {
        if (isTRUE(crosshair)) {
            guide_color <- grDevices::adjustcolor(marker_color, alpha.f = 0.72)
            graphics::abline(
                v = selection$x,
                h = selection$y,
                col = guide_color,
                lty = 3,
                lwd = 1.5
            )
        }

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
            shiny::span("Selected site", class = "curves-info-title"),
            shiny::span(
                "Click a non-missing raster cell to place site markers on the response curves.",
                class = "curves-info-metric curves-info-muted"
            )
        ))
    }

    shiny::div(
        class = "curves-info-box",
        shiny::span("Selected site", class = "curves-info-title"),
        shiny::span(
            sprintf("x = %.4f", selection$x),
            class = "curves-info-metric"
        ),
        shiny::span(
            sprintf("y = %.4f", selection$y),
            class = "curves-info-metric"
        ),
        shiny::span(
            sprintf("%s = %.4f", ylab, selection$prediction),
            class = "curves-info-metric"
        )
    )
}


build_curve_legend <- function(method, show_selected_ice, curve_color,
                               selected_color) {
    items <- list()

    if (method == "profile") {
        items[[length(items) + 1L]] <- legend_item(
            "Model curve",
            curve_color,
            linetype = "solid"
        )
    } else if (method == "pdp") {
        items[[length(items) + 1L]] <- legend_item(
            "PDP",
            curve_color,
            linetype = "solid"
        )
    } else if (method == "ale") {
        items[[length(items) + 1L]] <- legend_item(
            "ALE",
            curve_color,
            linetype = "solid"
        )
    } else if (method == "ice") {
        items[[length(items) + 1L]] <- legend_item(
            "ICE curves",
            curve_color,
            linetype = "solid",
            alpha = 0.6
        )
    } else if (method == "ice+pdp") {
        items[[length(items) + 1L]] <- legend_item(
            "PDP",
            curve_color,
            linetype = "solid"
        )
        items[[length(items) + 1L]] <- legend_item(
            "Background ICE",
            "gray70",
            linetype = "solid",
            alpha = 0.55
        )
    }

    if (isTRUE(show_selected_ice) && method != "ale") {
        items[[length(items) + 1L]] <- legend_item(
            "Selected pixel ICE",
            selected_color,
            linetype = "solid"
        )
    }

    items[[length(items) + 1L]] <- legend_item(
        "Selected value",
        grDevices::adjustcolor(selected_color, alpha.f = 0.72),
        linetype = "dashed"
    )

    shiny::div(
        class = "curves-legend-panel",
        shiny::span("Curve legend", class = "curves-info-title"),
        shiny::div(
            class = "curves-legend-box",
            items
        )
    )
}


legend_item <- function(label, color, linetype = c("solid", "dashed"),
                        alpha = 0.95) {
    linetype <- match.arg(linetype)
    color <- css_color(color)
    style <- sprintf(
        "border-top-color:%s;border-top-style:%s;border-top-width:3px;opacity:%s;",
        color,
        if (identical(linetype, "dashed")) "dashed" else "solid",
        format(alpha, trim = TRUE)
    )
    if (identical(linetype, "dashed")) {
        style <- paste0(style, "border-image:none;")
    }

    shiny::div(
        class = "curves-legend-item",
        shiny::span(class = "curves-legend-swatch", style = style),
        shiny::span(label)
    )
}


css_color <- function(color) {
    rgb <- grDevices::col2rgb(color, alpha = TRUE)
    grDevices::rgb(
        red = rgb[1, 1],
        green = rgb[2, 1],
        blue = rgb[3, 1],
        alpha = rgb[4, 1],
        maxColorValue = 255
    )
}


style_interactive_curve_plot <- function(plot, title, style) {
    plot +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme(
            text = ggplot2::element_text(
                family = style$family,
                color = style$title_color
            ),
            plot.title = ggplot2::element_text(
                hjust = 0.5,
                face = "bold",
                size = style$title_size,
                color = style$title_color
            ),
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(
                size = style$axis_text_size,
                color = style$axis_color
            ),
            axis.ticks = ggplot2::element_line(
                color = style$axis_color,
                linewidth = 0.25
            ),
            panel.background = ggplot2::element_rect(
                fill = style$panel_bg,
                color = NA
            ),
            panel.border = ggplot2::element_rect(
                fill = NA,
                color = style$panel_border,
                linewidth = 0.55
            ),
            plot.background = ggplot2::element_rect(
                fill = style$plot_bg,
                color = NA
            ),
            panel.grid.major = ggplot2::element_line(
                color = style$grid_color,
                linewidth = 0.35
            ),
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
            color = grDevices::adjustcolor(selected_color, alpha.f = 0.72),
            linewidth = 0.65,
            linetype = "dashed"
        ))
    }

    selected_position <- selected_factor_position(table, selected_value)
    if (is.null(selected_position)) {
        return(plot)
    }

    plot + ggplot2::geom_vline(
        xintercept = selected_position,
        color = grDevices::adjustcolor(selected_color, alpha.f = 0.72),
        linewidth = 0.65,
        linetype = "dashed",
        inherit.aes = FALSE
    )
}


add_selected_ice_curve <- function(plot, spec, selected_curve, selected_color) {
    if (is.null(selected_curve) || !nrow(selected_curve)) {
        return(plot)
    }

    if (!spec$is_factor) {
        return(plot + ggplot2::geom_line(
            data = selected_curve,
            ggplot2::aes(x = x, y = y),
            color = selected_color,
            linewidth = 0.58,
            alpha = 0.94,
            inherit.aes = FALSE
        ))
    }

    if (spec$is_ordered) {
        plot <- plot + ggplot2::geom_line(
            data = selected_curve,
            ggplot2::aes(x = x, y = y, group = 1),
            color = selected_color,
            linewidth = 0.58,
            alpha = 0.94,
            inherit.aes = FALSE
        )
    }

    plot + ggplot2::geom_point(
        data = selected_curve,
        ggplot2::aes(x = x, y = y),
        color = selected_color,
        size = 2.4,
        inherit.aes = FALSE
    )
}


selected_factor_position <- function(table, selected_value) {
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
    position <- match(selected_level, levels(curve_df$x))
    if (is.na(position)) {
        return(NULL)
    }

    position
}
