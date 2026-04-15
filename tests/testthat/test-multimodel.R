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
