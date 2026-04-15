.is_rast <- function(x) {
    inherits(x, "SpatRaster")
}


.is_factor <- function(x) {
    if (.is_rast(x)) {
        terra::is.factor(x)
    } else {
        vapply(x, is.factor, logical(1))
    }
}


utils::globalVariables(c("std", "var", "x", "y"))
