test_that("multimodel averages response curves across models", {
    models <- list(
        lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris),
        lm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris)
    )

    plot <- multimodel(
        models,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
    )

    expect_s3_class(plot, "ggplot")
})


test_that("multimodel supports pdp, ale, and model overlays", {
    models <- list(
        lm(
            Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
            data = iris
        ),
        lm(
            Sepal.Length ~ Sepal.Width + Petal.Length,
            data = iris
        )
    )

    pdp_plot <- multimodel(
        models,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        method = "pdp",
        n = 20,
        background_n = 15,
        interval = "quantile",
        show_models = TRUE
    )

    expect_s3_class(pdp_plot, "ggplot")

    expect_no_warning(
        ale_plot <- multimodel(
            list(
                lm(Sepal.Length ~ Species + Sepal.Width, data = iris),
                lm(Sepal.Length ~ Species + Petal.Length, data = iris)
            ),
            x = iris[, c("Species", "Sepal.Width", "Petal.Length")],
            method = "ale",
            n = 15
        )
    )

    expect_s3_class(ale_plot, "ggplot")
})


test_that("multimodel ale supports unordered factor predictors", {
    x_df <- data.frame(
        grp = factor(c("a", "a", "b", "b", "c", "c")),
        check.names = FALSE
    )

    plot <- multimodel(
        models = list(structure(list(id = 1), class = "dummy"),
                      structure(list(id = 2), class = "dummy")),
        x = x_df,
        method = "ale",
        interval = "none",
        fun = list(
            function(model, newdata) {
                c(a = 0, b = 1, c = 2)[as.character(newdata$grp)]
            },
            function(model, newdata) {
                c(a = 2, b = 1, c = 0)[as.character(newdata$grp)]
            }
        )
    )

    expect_s3_class(plot, "ggplot")
})


test_that("multimodel handles factor predictors", {
    models <- list(
        lm(Sepal.Length ~ Species + Petal.Width, data = iris),
        lm(Sepal.Length ~ Species + Petal.Length, data = iris)
    )

    plot <- multimodel(
        models,
        x = iris[, c("Species", "Petal.Width", "Petal.Length")]
    )

    expect_s3_class(plot, "ggplot")
})


test_that("multimodel summaries support custom aggregation and weights", {
    mat <- cbind(c(1, 3), c(3, 5))

    summary_df <- curves:::summarize_multimodel_predictions(
        x = c(0, 1),
        mat = mat,
        agg = function(x, w) sum(x * w) / sum(w),
        weights = c(1, 3),
        interval = "sd"
    )

    expect_equal(summary_df$y, c(2.5, 4.5))
    expect_equal(
        summary_df$ymax - summary_df$y,
        rep(sqrt(0.75), 2)
    )
})


test_that("multimodel supports binary probability matrices", {
    models <- list(
        lm(
            Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
            data = iris
        ),
        lm(
            Sepal.Width ~ Petal.Length + Petal.Width + Sepal.Length,
            data = iris
        )
    )

    plot <- multimodel(
        models,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        n = 25,
        response = "present",
        fun = function(model, newdata) {
            prob <- stats::plogis(
                0.4 * newdata$Sepal.Width -
                    0.3 * newdata$Petal.Length +
                    0.2 * newdata$Petal.Width
            )
            data.frame(absent = 1 - prob, present = prob)
        }
    )

    expect_s3_class(plot, "ggplot")
})


test_that("multimodel validates weights and custom aggregators", {
    models <- list(
        lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris),
        lm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris)
    )

    expect_error(
        multimodel(
            models,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            weights = 1
        ),
        "same length as models"
    )

    expect_error(
        curves:::summarize_multimodel_predictions(
            x = c(0, 1),
            mat = cbind(c(1, 3), c(3, 5)),
            agg = function(x) stats::median(x),
            weights = c(1, 3),
            interval = "none"
        ),
        "accept a `weights` or `w` argument"
    )
})


test_that("multimodel accepts one prediction function per model", {
    models <- list(
        lm(
            Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
            data = iris
        ),
        lm(
            Sepal.Width ~ Petal.Length + Petal.Width,
            data = iris
        )
    )

    plot <- multimodel(
        models,
        x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
        n = 25,
        fun = list(
            function(model, newdata) {
                stats::plogis(stats::predict(model, newdata))
            },
            function(model, newdata) {
                prob <- stats::plogis(stats::predict(model, newdata))
                data.frame(absent = 1 - prob, present = prob)
            }
        ),
        response = "present"
    )

    expect_s3_class(plot, "ggplot")
})


test_that("multimodel validates list-valued fun", {
    models <- list(
        lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris),
        lm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris)
    )

    expect_error(
        multimodel(
            models,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            fun = list(stats::predict)
        ),
        "same length as `models`"
    )

    expect_error(
        multimodel(
            models,
            x = iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")],
            fun = list(stats::predict, "not-a-function")
        ),
        "Each element of `fun` must be a function"
    )
})
