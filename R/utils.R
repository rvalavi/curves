# get summary of both numeric and factor variables
calc_summary <- function(x, ...) {
    if (is.factor(x)) {
        # For factor vectors: calculate mode(s)
        freq_table <- table(x)
        max_freq <- max(freq_table)
        modes <- names(freq_table[freq_table == max_freq])
        result <- c(
            min = min(as.numeric(levels(x))[x], ...),
            mean = as.numeric(modes),
            max = max(as.numeric(levels(x))[x], ...)
        )
    } else if (is.numeric(x)) {
        # For numeric vectors: calculate min, max, and the specified function
        result <- c(
            min = min(x, ...),
            mean = mean(x, ...),
            max = max(x, ...)
        )
    } else {
        stop("Input must be either numeric or factor.")
    }
    return(result)
}

# estimate summary stats of a raster faster
gestimate <- function(x, fun) {
    sapply(
        terra::spatSample(x, method = "regular", size = 5e5),
        FUN = fun,
        na.rm = TRUE
    )
}

# get the range of a raster or matrix
get_range <- function(x) {
    require(terra)
    if(is(x, "SpatRaster")) {
        rng <- gestimate(x, fun = calc_summary)
    } else {
        rng <- sapply(x, FUN = calc_summary)
    }

    return(rng)
}


