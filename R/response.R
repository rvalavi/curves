#' Response curve plot
#'
#' @param model
#' @param x
#' @param predict_data
#' @param fun
#' @param ...
#' @param n
#' @param ylab
#' @param nrows
#' @param ncols
#' @param rug
#' @param ylim
#' @param color
#'
#' @returns
#' @export
#'
#' @examples
response <- function(model, x = NULL, predict_data = NULL,
                     fun = predict, ..., n = 100, ylab = "Prediction",
                     rug = TRUE, ylim = NULL,
                     color = "orangered2",
                     nrows = NULL, ncols = NULL) {

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
    } else {
        x <- predict_data
    }

    # check if all x are in predict_data

    nms <- names(x)
    rngs <- get_range(x)
    nvars <- if(.is_rast(x)) terra::nlyr(x) else ncol(x)

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
            x = x[[i]],
            y = as.numeric(fun(model, x, ...))
        )
    }

    tables <- list()
    for (i in seq_len(nvars)) {
        tables[[i]] <- f(means, ranges, i)
    }

    # get mins and maxes
    limits <- c(
        min(sapply(tables, FUN = function(x) min(x[[2]]))) * 1.1,
        max(sapply(tables, FUN = function(x) max(x[[2]]))) * 1.1
    )

    plots <- list()
    for (j in seq_along(tables)) {
        name <- nms[j]
        plots[[j]] <- plotting(
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
plotting <- function(df, dat, fact, rug, x_name, y_name, ylim, color) {
    rug <- if (rug) {
        geom_rug(data = dat, aes(x = var),
                 sides = "b", color = "black", alpha = 0.5,
                 inherit.aes = FALSE)
    } else {
        NULL
    }
    plot_geom <- if (fact) {
        geom_segment(aes(x = x - 0.5, xend = x + 0.5, y = y, yend = y), color = color, size = 1.2)
    } else {
        geom_line(color = color, size = 1)
    }
    plt <- ggplot(df, aes(x = x, y = y)) +
            plot_geom +
            switch(!fact, rug, NULL) +
            scale_y_continuous(limits = ylim) +
            theme_bw() + # base_size = 12
            labs(x = x_name, y = y_name)

    return(plt)
}




