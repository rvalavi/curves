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


validate_positive_count <- function(n, name) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) {
        stop(name, " must be a single positive integer")
    }

    as.integer(n)
}


validate_curve_n <- function(n) {
    validate_positive_count(n, name = "n")
}


validate_background_n <- function(n) {
    validate_positive_count(n, name = "background_n")
}


curve_sample_size <- function(x_source, n, background_n, method) {
    target_n <- if (method %in% c("pdp", "ice", "ice+pdp")) {
        max(n, background_n)
    } else {
        n
    }

    if (.is_rast(x_source)) {
        return(max(5000L, target_n * 50L))
    }

    5000L
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


build_curve_stack <- function(background_rows, column, values) {
    grid <- background_rows[
        rep(seq_len(nrow(background_rows)), each = length(values)),
        ,
        drop = FALSE
    ]

    if (is.factor(background_rows[[column]])) {
        grid[[column]] <- factor(
            rep(as.character(values), times = nrow(background_rows)),
            levels = levels(background_rows[[column]]),
            ordered = is.ordered(background_rows[[column]])
        )
    } else {
        grid[[column]] <- rep(values, times = nrow(background_rows))
    }

    grid
}


sample_background_rows <- function(x_df, background_n) {
    x_df <- complete_predictor_rows(
        x_df,
        context = "PDP or ICE methods"
    )

    if (nrow(x_df) <= background_n) {
        return(x_df)
    }

    index <- sort(sample.int(nrow(x_df), size = background_n))
    x_df[index, , drop = FALSE]
}


complete_predictor_rows <- function(x_df, context) {
    keep <- stats::complete.cases(x_df)
    x_df <- x_df[keep, , drop = FALSE]

    if (!nrow(x_df)) {
        stop(
            "predict_data must contain at least one complete row for ",
            context
        )
    }

    x_df
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


normalize_multimodel_funs <- function(fun, n_models) {
    if (is.function(fun)) {
        return(rep(list(fun), n_models))
    }

    if (!is.list(fun)) {
        stop("`fun` must be a function or a list of functions.")
    }

    if (length(fun) != n_models) {
        stop("When `fun` is a list, it must have the same length as `models`.")
    }

    is_fun <- vapply(fun, is.function, logical(1))
    if (!all(is_fun)) {
        stop(
            "Each element of `fun` must be a function. Invalid indices: ",
            paste(which(!is_fun), collapse = ", ")
        )
    }

    fun
}


extract_prediction_vector <- function(prediction, n, response = NULL) {
    if (is.data.frame(prediction) || is.matrix(prediction)) {
        prediction_df <- as.data.frame(prediction, check.names = FALSE)

        if (nrow(prediction_df) != n) {
            stop(
                "Prediction output must have ", n,
                " rows, but got ", nrow(prediction_df), "."
            )
        }

        if (ncol(prediction_df) == 1L) {
            prediction <- prediction_df[[1L]]
        } else {
            if (is.null(response)) {
                if (ncol(prediction_df) == 2L) {
                    response <- 2L
                } else {
                    stop(
                        "Prediction output has ", ncol(prediction_df),
                        " columns. Supply `response` as a column name or index, ",
                        "or pass a `fun` that returns a single prediction vector."
                    )
                }
            }

            if (is.character(response)) {
                if (length(response) != 1L || !response %in% names(prediction_df)) {
                    stop(
                        "`response` must match a prediction column name. Available columns: ",
                        paste(names(prediction_df), collapse = ", ")
                    )
                }
                prediction <- prediction_df[[response]]
            } else if (is.numeric(response) && length(response) == 1L && !is.na(response)) {
                response <- as.integer(response)
                if (response < 1L || response > ncol(prediction_df)) {
                    stop(
                        "`response` must be between 1 and ", ncol(prediction_df), "."
                    )
                }
                prediction <- prediction_df[[response]]
            } else {
                stop("`response` must be NULL, a single column name, or a single column index.")
            }
        }
    }

    if (length(prediction) != n) {
        stop(
            "Prediction output must have length ", n,
            ", but got length ", length(prediction), "."
        )
    }

    if (is.logical(prediction)) {
        return(as.numeric(prediction))
    }

    if (is.factor(prediction)) {
        return(as.numeric(prediction))
    }

    if (!is.numeric(prediction)) {
        stop(
            "Prediction output must be numeric, logical, factor, matrix, or data frame."
        )
    }

    as.numeric(prediction)
}
