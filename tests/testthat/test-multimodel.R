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
