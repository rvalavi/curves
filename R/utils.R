# create a data frame of predictors while keeping raster support optional
as_predictor_frame <- function(x, sample_size = NULL) {
    if (.is_rast(x)) {
        n_cells <- terra::ncell(x)
        if (is.null(sample_size) || n_cells <= sample_size) {
            return(terra::as.data.frame(x, na.rm = TRUE))
        }

        return(as.data.frame(
            terra::spatSample(
                x,
                method = "regular",
                size = sample_size,
                na.rm = TRUE
            )
        ))
    }

    as.data.frame(x, check.names = FALSE)
}


validate_predictors <- function(x, sample_size = NULL) {
    x_df <- as_predictor_frame(x, sample_size = sample_size)

    if (ncol(x_df) == 0L) {
        stop("x must contain at least one predictor")
    }

    supported <- vapply(
        x_df,
        function(column) is.numeric(column) || is.factor(column),
        logical(1)
    )

    if (!all(supported)) {
        unsupported <- names(x_df)[!supported]
        stop(
            "Only numeric and factor predictors are supported. Unsupported columns: ",
            paste(unsupported, collapse = ", ")
        )
    }

    x_df
}


factor_mode <- function(x) {
    x <- stats::na.omit(x)
    if (!length(x)) {
        stop("Predictors must contain at least one non-missing value")
    }

    freq_table <- table(x)
    mode_level <- names(freq_table)[which.max(freq_table)]

    factor(
        mode_level,
        levels = levels(x),
        ordered = is.ordered(x)
    )
}


reference_value <- function(x) {
    x <- stats::na.omit(x)
    if (!length(x)) {
        stop("Predictors must contain at least one non-missing value")
    }

    if (is.factor(x)) {
        return(factor_mode(x))
    }

    if (!is.numeric(x)) {
        stop("Only numeric and factor predictors are supported")
    }

    mean(x)
}


curve_values <- function(x, n) {
    x <- stats::na.omit(x)
    if (!length(x)) {
        stop("Predictors must contain at least one non-missing value")
    }

    if (is.factor(x)) {
        observed_levels <- levels(x)[levels(x) %in% unique(as.character(x))]
        return(factor(
            observed_levels,
            levels = levels(x),
            ordered = is.ordered(x)
        ))
    }

    if (!is.numeric(x)) {
        stop("Only numeric and factor predictors are supported")
    }

    rng <- range(x)
    seq(rng[1], rng[2], length.out = n)
}


build_reference_row <- function(x_df) {
    data.frame(
        lapply(x_df, reference_value),
        check.names = FALSE
    )
}


build_curve_grid <- function(reference_row, column, values) {
    grid <- reference_row[rep(1L, length(values)), , drop = FALSE]

    if (is.factor(reference_row[[column]])) {
        grid[[column]] <- factor(
            as.character(values),
            levels = levels(reference_row[[column]]),
            ordered = is.ordered(reference_row[[column]])
        )
    } else {
        grid[[column]] <- values
    }

    grid
}


curve_limits <- function(values, padding = 0.1) {
    rng <- range(values, na.rm = TRUE)
    if (!all(is.finite(rng))) {
        stop("Predictions must be finite to compute plot limits")
    }

    span <- diff(rng)
    pad <- if (span == 0) {
        max(abs(rng[1]), 1) * padding
    } else {
        span * padding
    }

    c(rng[1] - pad, rng[2] + pad)
}


sample_rug_values <- function(x_df, column, max_n = 5000L) {
    values <- stats::na.omit(x_df[[column]])
    if (!length(values)) {
        return(data.frame(var = numeric(0)))
    }

    if (length(values) > max_n) {
        index <- unique(round(seq(1, length(values), length.out = max_n)))
        values <- values[index]
    }

    data.frame(var = values)
}
