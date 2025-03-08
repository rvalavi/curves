# estimate mean of raster faster
gestimate <- function(x) {
    apply(
        terra::spatSample(x, method = "regular", size = 5e5),
        MARGIN = 2,
        FUN = mean,
        na.rm = TRUE
    )
}

# get the range of a raster or matrix
get_range <- function(x) {
    require(terra)
    if(is(x, "SpatRaster")) {
        if (all(x@ptr@.xData$hasRange)) {
            rng <- t(
                cbind(
                    x@ptr@.xData$range_min,
                    gestimate(x),
                    x@ptr@.xData$range_max
                )
            )
        } else {
            rng <- t(
                cbind(
                    terra::global(x, fun = "min", na.rm = TRUE),
                    terra::global(x, fun = "mean", na.rm = TRUE),
                    terra::global(x, fun = "max", na.rm = TRUE)
                )
            )
        }
    } else {
        f <- function(x) {
            return(
                c(
                    min(x),
                    mean(x),
                    max(x)
                )
            )
        }
        rng <- apply(x, 2, f)
    }

    return(rng)
}


