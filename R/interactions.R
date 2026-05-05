#' Rank pairwise interactions using second-order ALE
#'
#' Quantify and rank pairwise interaction strength using the same centred
#' second-order accumulated local effects (ALE) surfaces used by
#' [bivariate()] with `method = "ale"`. This is also the ranking used
#' internally by `bivariate(method = "ale", top_n = ...)` when it selects the
#' strongest surfaces to plot.
#'
#' @details
#' Let \eqn{\hat{f}_{ab,ALE}(c_k, d_l)} denote the centred second-order ALE
#' surface for predictor pair \eqn{(a, b)} evaluated at cell centre
#' \eqn{(c_k, d_l)}, and let \eqn{n_{k,l}} be the number of observed rows in
#' that ALE cell. The returned interaction strength is the count-weighted
#' root-mean-square ALE magnitude,
#' \deqn{I_{ab} =
#'   \sqrt{\frac{\sum_{k,l} n_{k,l}\hat{f}_{ab,ALE}(c_k, d_l)^2}
#'                {\sum_{k,l} n_{k,l}}}.}
#' This uses the observed ALE cell counts as weights, so densely supported
#' parts of the predictor space contribute more than sparse cells. Additive
#' predictor pairs score zero, and larger values indicate stronger non-additive
#' joint effects. Cells with zero observations are excluded from the score.
#'
#' @param model A fitted model object that supports prediction.
#' @param x A data frame or raster containing predictor variables. If
#'   `predict_data` is provided, this argument is ignored.
#' @param predict_data A data frame containing values at which predictions
#'   should be made. If `NULL`, `x` must be provided.
#' @param pairs Optional specification of predictor pairs to rank. Supply
#'   `NULL` to rank all unique pairs, a character or numeric vector of length 2
#'   for a single pair, or a list/data frame/matrix of pairs. Numeric pairs are
#'   interpreted as predictor column indices. Non-numeric pairs are ignored
#'   with a warning because second-order ALE currently supports numeric
#'   predictor pairs only.
#' @param fun A function used to generate predictions from the model. If
#'   `NULL`, the generic `predict()` is used.
#' @param ... Additional arguments passed to `fun`.
#' @param n Integer, maximum number of ALE intervals per numeric predictor
#'   (default: `10`).
#' @param response Optional column name or index to select when `fun` returns
#'   multiple predictions per row. If `NULL` and exactly two prediction columns
#'   are returned, the second column is used.
#' @param details Logical; if `FALSE` (default), return the ranking data frame.
#'   If `TRUE`, return a list containing the ranking plus the ALE tables and
#'   pair specifications used internally by [bivariate()] when `top_n` filters
#'   ALE surfaces. The returned tables keep interpolated values for unsupported
#'   cells so callers such as [bivariate()] can decide whether to mask or show
#'   them.
#'
#' @return If `details = FALSE`, a data frame ordered from highest to lowest
#'   interaction strength, with columns `rank`, `pair`, `predictor_1`,
#'   `predictor_2`, `strength`, `support`, `supported_cells`, and
#'   `total_cells`. If `details = TRUE`, a list with components `ranking`,
#'   `pair_specs`, and `tables`.
#'
#' @references
#' Molnar, C. (2025). *Interpretable Machine Learning: A Guide for Making Black
#' Box Models Explainable* (3rd ed.). <https://christophm.github.io/interpretable-ml-book/>
#'
#' Apley, D. W., & Zhu, J. (2020). Visualizing the effects of predictor
#' variables in black box supervised learning models. *Journal of the Royal
#' Statistical Society: Series B*, 82(4), 1059-1086.
#'
#' @export
#'
#' @examples
#' data(iris)
#' model <- lm(
#'   Sepal.Length ~ Sepal.Width * Petal.Length + Petal.Width,
#'   data = iris
#' )
#'
#' interactions(
#'   model,
#'   x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
#' )
interactions <- function(model, x = NULL, predict_data = NULL,
                         pairs = NULL, fun = NULL, ...,
                         n = 10, response = NULL, details = FALSE) {

    fun <- resolve_predict_fun(fun, env = parent.frame())
    n <- validate_curve_n(n)

    if (!is.logical(details) || length(details) != 1L || is.na(details)) {
        stop("details must be TRUE or FALSE")
    }

    if (is.null(predict_data)) {
        if (is.null(x)) {
            stop("x or predict_data must be provided")
        }
        x_source <- x
    } else {
        x_source <- predict_data
    }

    sample_size <- curve_sample_size(
        x_source,
        n = n,
        background_n = n,
        method = "ale"
    )

    x_df <- validate_predictors(x_source, sample_size = sample_size)
    pair_specs <- build_pair_specs(x_df, pairs = pairs, n = n, method = "ale")
    pair_specs <- filter_ale_pair_specs(pair_specs)

    if (!length(pair_specs)) {
        stop("ALE interaction ranking requires at least one numeric predictor pair")
    }

    ale_rows <- complete_predictor_rows(x_df, context = "ALE methods")
    results <- build_ale_pair_results(
        model = model,
        ale_rows = ale_rows,
        pair_specs = pair_specs,
        fun = fun,
        response = response,
        extrapolate = details,
        ...
    )

    if (details) {
        return(results)
    }

    results$ranking[, setdiff(names(results$ranking), "pair_id"), drop = FALSE]
}


validate_top_n <- function(top_n) {
    if (is.null(top_n)) {
        return(NULL)
    }

    validate_positive_count(top_n, name = "top_n")
}


build_ale_pair_results <- function(model, ale_rows, pair_specs, fun, response,
                                   extrapolate = FALSE, ...) {
    pair_ids <- paste0("pair_", seq_along(pair_specs))
    tables <- Map(
        function(spec, pair_id) {
            table <- build_ale_surface_table(
                model = model,
                ale_rows = ale_rows,
                spec = spec,
                extrapolate = extrapolate,
                fun = fun,
                response = response,
                ...
            )

            attr(table, "pair_id") <- pair_id
            table
        },
        pair_specs,
        pair_ids
    )
    names(tables) <- pair_ids

    ranking <- do.call(
        rbind,
        Map(
            function(spec, pair_id, table) {
                summary <- summarize_ale_surface(table)
                data.frame(
                    pair_id = pair_id,
                    pair = pair_label(spec),
                    predictor_1 = spec$x_name,
                    predictor_2 = spec$y_name,
                    strength = summary$strength,
                    support = summary$support,
                    supported_cells = summary$supported_cells,
                    total_cells = summary$total_cells,
                    stringsAsFactors = FALSE
                )
            },
            pair_specs,
            pair_ids,
            tables
        )
    )

    ranking <- ranking[
        order(-ranking$strength, ranking$predictor_1, ranking$predictor_2),
        ,
        drop = FALSE
    ]
    rownames(ranking) <- NULL
    ranking$rank <- seq_len(nrow(ranking))
    ranking <- ranking[
        ,
        c(
            "rank",
            "pair_id",
            "pair",
            "predictor_1",
            "predictor_2",
            "strength",
            "support",
            "supported_cells",
            "total_cells"
        ),
        drop = FALSE
    ]

    list(
        ranking = ranking,
        pair_specs = pair_specs,
        tables = tables
    )
}


summarize_ale_surface <- function(table) {
    keep <- table$count > 0L & !is.na(table$z)
    total_cells <- nrow(table)
    supported_cells <- sum(table$count > 0L)

    list(
        strength = if (any(keep)) {
            sqrt(stats::weighted.mean(table$z[keep]^2, w = table$count[keep]))
        } else {
            NA_real_
        },
        support = if (total_cells) supported_cells / total_cells else NA_real_,
        supported_cells = supported_cells,
        total_cells = total_cells
    )
}
