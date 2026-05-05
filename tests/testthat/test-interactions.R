test_that("interactions returns near-zero scores for additive models", {
    predictors <- expand.grid(
        x1 = seq(0, 1, length.out = 8),
        x2 = seq(1, 2, length.out = 7)
    )
    predictors$x3 <- seq_len(nrow(predictors)) / 10

    ranks <- interactions(
        model = NULL,
        x = predictors,
        n = 5,
        fun = function(model, newdata) {
            3 * newdata$x1 - 2 * newdata$x2 + newdata$x3
        }
    )

    expect_true(max(ranks$strength) < 1e-10)
    expect_true(all(diff(ranks$strength) <= 0))
})


test_that("interactions orders pairs by ALE interaction strength", {
    set.seed(123)
    predictors <- data.frame(
        x1 = stats::runif(300),
        x2 = stats::runif(300),
        x3 = stats::runif(300)
    )

    ranks <- interactions(
        model = NULL,
        x = predictors,
        n = 8,
        fun = function(model, newdata) {
            2 * newdata$x1 * newdata$x2 + newdata$x3
        }
    )

    expect_identical(ranks$pair[1], "x1 vs x2")
    expect_true(all(diff(ranks$strength) <= 0))
    expect_gt(ranks$strength[1], 0.05)
    expect_true(max(ranks$strength[-1L]) < 1e-10)
})


test_that("interactions filters to numeric predictor pairs", {
    model <- lm(
        Sepal.Length ~ Species + Sepal.Width * Petal.Length,
        data = iris
    )

    expect_warning(
        ranks <- interactions(
            model,
            x = iris[, c("Species", "Sepal.Width", "Petal.Length")],
            n = 8
        ),
        "numeric predictor pairs only"
    )

    expect_identical(ranks$pair, "Sepal.Width vs Petal.Length")
})


test_that("interactions details expose tables used by bivariate ale", {
    set.seed(123)
    predictors <- data.frame(
        x1 = stats::runif(150),
        x2 = stats::runif(150),
        x3 = stats::runif(150)
    )

    details <- interactions(
        model = NULL,
        x = predictors,
        n = 6,
        details = TRUE,
        fun = function(model, newdata) {
            2 * newdata$x1 * newdata$x2 + newdata$x3
        }
    )

    expect_type(details, "list")
    expect_true(all(c("ranking", "pair_specs", "tables") %in% names(details)))
    expect_identical(details$ranking$pair[1], "x1 vs x2")
})


test_that("bivariate ale top_n keeps the highest-ranked pair", {
    set.seed(123)
    predictors <- data.frame(
        x1 = stats::runif(300),
        x2 = stats::runif(300),
        x3 = stats::runif(300)
    )

    plot <- bivariate(
        model = NULL,
        x = predictors,
        method = "ale",
        top_n = 1,
        n = 8,
        fun = function(model, newdata) {
            2 * newdata$x1 * newdata$x2 + newdata$x3
        }
    )

    expect_s3_class(plot, "ggplot")
    expect_identical(plot$labels$x, "x1")
    expect_identical(plot$labels$y, "x2")
})


test_that("bivariate top_n is limited to ALE mode", {
    model <- lm(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
        data = iris
    )

    expect_error(
        bivariate(
            model,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            top_n = 1
        ),
        "top_n is only supported when method = \"ale\""
    )
})
