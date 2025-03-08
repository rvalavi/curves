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
#'
#' @returns
#' @export
#'
#' @examples
response <- function(model, x = NULL, predict_data = NULL,
                     fun = predict, ..., n = 100, ylab = "Prediction",
                     nrows = NULL, ncols = NULL) {

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
    } else {
        x <- predict_data
    }

    nms <- names(x)
    rng <- get_range(x)
    nvars <- if(is_rast(x)) terra::nlyr(x) else ncol(x)

    means <- outer(rep(1, n), rng[2, ]) # get the means
    rngs <- apply(x, 2, function(x) seq(x[1], x[3], length.out=n))

    facts <- 0
    if (is.factor(x)) {

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
    # plots
    plotting <- function(df, name) {
        return(
            ggplot(df, aes(x = x, y = y)) +
                geom_line(color = "black", size = 1) +
                geom_rug(sides = "b", color = "black", alpha = 0.5) +
                theme_bw(base_size = 14) +
                labs(x = name, y = ylab)
        )
    }

    plots <- list()
    for (i in seq_len(nvars)) {
        plots[[i]] <- plotting(
            df = f(means, rngs, i),
            name = nms[i]
        )
    }

    return(
        cowplot::plot_grid(plotlist = plots, nrow = 2, ncol = 2)
    )
}


# debugonce(response)
response(model = mod, x = train_data[, 2:3])


