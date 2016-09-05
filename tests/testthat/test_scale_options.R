test_that("Color scale options", {
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     color = Sepal.Width / max(Sepal.Width),
                     color2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter") +
      scale_color_colorplane(labels = scales::percent,
                             labels_y = function(x)paste0(x, "!")) +
      ggtitle("guide labels: x percent, y with !")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     color = Sepal.Width / max(Sepal.Width),
                     color2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter") +
      scale_color_colorplane(breaks = c(.5, .75),
                             breaks_y = function(x)seq(floor(x[1]), x[2]),
                             axis_title = "% Sepal Width",
                             axis_title_y = "Sepal Length") +
      ggtitle("guide breaks: x (.5, .75), y 5:7, axis titles: spaces and %")
  ))
})

test_that("Fill scale options", {
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     fill = Sepal.Width / max(Sepal.Width),
                     fill2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter", shape = 23, size = 5) +
      scale_fill_colorplane(labels = scales::percent,
                             labels_y = function(x)paste0(x, "!")) +
      ggtitle("guide labels: x percent, y with !")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     fill = Sepal.Width / max(Sepal.Width),
                     fill2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter", shape = 23, size = 5) +
      scale_fill_colorplane(breaks = c(.5, .75),
                             breaks_y = function(x)seq(floor(x[1]), x[2]),
                             axis_title = "% Sepal Width",
                             axis_title_y = "Sepal Length") +
      ggtitle("guide breaks: x (.5, .75), y 5:7, axis titles: spaces and %")
  ))
})
