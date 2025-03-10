
.is_rast <- function(x) {
    is(x, "SpatRaster")
}


.is_factor <- function(x) {
    if (.is_rast(x)) {
        terra::is.factor(x)
    } else {
        sapply(x, is.factor)
    }
}
