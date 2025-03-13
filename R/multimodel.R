#' Creating a multimodel response curve plot
#'
#' This function generates response curves for several models by varying one predictor at a time while keeping others constant.
#'
#' @param models A list object of all fitted model that supports prediction.
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
#' @param se Standard deviation of the the curve to be added there.
#'
#' @return A `ggplot2` object containing the response curves arranged in a grid.
#'
#' @export
#'
#' @examples
multimodel <- function(models, x = NULL, predict_data = NULL,
                       fun = predict, ..., n = 100, ylab = "Prediction",
                       se = TRUE,
                       rug = TRUE, ylim = NULL,
                       color = "deepskyblue2",
                       se_color = "grey80",
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
    nsamp <- nrow(x)
    rngs <- get_range(x)
    nvars <- if(.is_rast(x)) terra::nlyr(x) else ncol(x)
    nmod <- length(models)

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
        # predict the ith variable with all models
        mat <- sapply(1:nmod, function(m) as.numeric(fun(models[[m]], x, ...)))
        data.frame(
            x = x[[i]], # get the ith column
            y = rowMeans(mat),
            std = apply(mat, 1, sd)
        )
    }

    # get the variables table
    tables <- lapply(1:nvars, function(i) f(means, ranges, i))

    # get mins and maxes
    min_pred <- min(sapply(tables, FUN = function(x) min(x[[2]] - x[[3]])))
    max_pred <- max(sapply(tables, FUN = function(x) max(x[[2]] + x[[3]])))
    limits <- c(
        min_pred + min_pred * 0.1,
        max_pred + max_pred * 0.1
    )

    plots <- list()
    for (j in seq_along(tables)) {
        name <- nms[j]
        plots[[j]] <- plot_1D(
            df = tables[[j]],
            dat = if (rug) data.frame(var = unique(x[[name]])) else 0,
            fact = ifelse(j %in% facts, TRUE, FALSE),
            rug = rug,
            se = se,
            x_name = name,
            y_name = ylab,
            color = color,
            ribcol = se_color,
            ylim = if(is.null(ylim)) limits else ylim
        )
    }

    return(
        cowplot::plot_grid(plotlist = plots, nrow = nrows, ncol = ncols)
    )
}

