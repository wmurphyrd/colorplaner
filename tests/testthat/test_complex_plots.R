data("iris")

test_that("Faceted plots", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species)
  ))
})

test_that("Grouped plots", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_path() +
      scale_colour_colourplane(Y = .5)
  ))
})

test_that("Multiple data layer plots", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, #y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      stat_bin(aes(fill = Petal.Length),
               binwidth = 1) +
      geom_point(aes(y = Sepal.Width),
                 data = iris[iris$Species != "setosa", ]) +
      scale_colour_colourplane(Y = .5)
  ))
})
