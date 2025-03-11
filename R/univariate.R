#' Univariate response curve plot
#'
#' This function generates response curves for a given model by varying one predictor at a time while keeping others constant.
#'
#' @param model A fitted model object that supports prediction.
#' @param x A data frame or raster containing predictor variables. If `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions should be made. If `NULL`, `x` must be provided.
#' @param fun A function used to generate predictions from the model. Defaults to `predict`.
#' @param ... Additional arguments passed to `fun`.
#' @param n Integer, number of points to sample for each predictor variable (default: 100).
#' @param ylab Character, label for the y-axis (default: "Prediction").
#' @param nrows Integer, number of rows in the plot grid. If `NULL`, it is automatically determined.
#' @param ncols Integer, number of columns in the plot grid. If `NULL`, it is automatically determined.
#' @param rug Logical, whether to include a rug plot along the x-axis (default: `TRUE`).
#' @param ylim Numeric vector of length 2, specifying the limits of the y-axis. If `NULL`, limits are automatically set.
#' @param color Character, colour of the response curve (default: "orangered2").
#'
#' @return A `ggplot2` object containing the response curves arranged in a grid.
#'
#' @export
#'
#' @examples
#' # Example usage with a fitted model
#' library(randomForest)
#' data(iris)
#' model <- randomForest(Sepal.Length ~ ., data = iris[, 1:4])
#' response_plot <- univariate(model, x = iris[, 2:4])
#' print(response_plot)
univariate <- function(model, x = NULL, predict_data = NULL,
                     fun = predict, ..., n = 100, ylab = "Prediction",
                     rug = TRUE, ylim = NULL,
                     color = "deepskyblue2",
                     nrows = NULL, ncols = NULL) {

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
    } else {
        x <- predict_data
    }

    # check if all x are in predict_data
    # get x from model if applicable
    # don't replace x with predict_data

    nms <- names(x)
    rngs <- get_range(x)
    nvars <- if(.is_rast(x)) terra::nlyr(x) else ncol(x)
    nsamp <- nrow(x)

    # define row and columns of the plots
    ncols <- if(is.null(ncols)) ceiling(sqrt(nvars))
    nrows <- if(is.null(nrows)) ceiling(nvars / ncols)

    # create the modelling matrices
    means <- outer(rep(1, n), rngs[2, ]) # get the means
    ranges <- apply(rngs, 2, function(arg) seq(arg[1], arg[3], length.out = n))

    # sort out the factors
    facts <- which(.is_factor(x))
    if (length(facts) > 0) {
        for (k in facts) {
            unq <- unique(x[[k]])
            ranges[, k] <- rep(unq, length.out = n)
        }
    }

    # a: means
    # b: ranges
    f <- function(a, b, i) {
        a[,i] <- b[,i]
        x <- as.data.frame(a)
        data.frame(
            x = x[[i]], # get the ith column
            y = as.numeric(fun(model, x, ...))
        )
    }

    # get the variables table
    tables <- lapply(1:nvars, function(i) f(means, ranges, i))

    # get mins and maxes
    limits <- c(
        min(sapply(tables, FUN = function(x) min(x[[2]]))) * 1.1,
        max(sapply(tables, FUN = function(x) max(x[[2]]))) * 1.1
    )

    plots <- list()
    for (j in seq_along(tables)) {
        name <- nms[j]
        plots[[j]] <- plot_1D(
            df = tables[[j]],
            dat = if (rug) data.frame(var = unique(x[[name]])) else 0,
            fact = ifelse(j %in% facts, TRUE, FALSE),
            rug = rug,
            x_name = name,
            y_name = ylab,
            color = color,
            ylim = if(is.null(ylim)) limits else ylim
        )
    }

    return(
        cowplot::plot_grid(plotlist = plots, nrow = nrows, ncol = ncols)
    )
}


# plotting function
plot_1D <- function(df, dat, fact, rug, x_name, y_name, ylim, color) {
    geom_conf <- if (ncol(df) > 2) {
        geom_ribbon(aes(ymin = y - std, ymax = y + std), fill = "grey70", alpha = 0.6)
    } else NULL

    rug_geom <- if (rug && !fact) {
        geom_rug(data = dat, aes(x = var),
                 sides = "b", color = "black", alpha = 0.5,
                 inherit.aes = FALSE)
    } else NULL

    plot_geom <- if (fact) {
        geom_segment(aes(x = x - 0.5, xend = x + 0.5, y = y, yend = y),
                     color = color, size = 1.2)
    } else {
        geom_line(color = color, size = 0.7)
    }
    plt <- ggplot(df, aes(x = x, y = y)) +
            plot_geom +
            geom_conf +
            rug_geom +
            scale_y_continuous(limits = ylim) +
            theme_bw() + # base_size = 12
            labs(x = x_name, y = y_name)

    return(plt)
}

