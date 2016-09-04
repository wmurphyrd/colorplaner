text_theme <- calc_element(
  "legend.title",
  theme_bw() %+%
    theme(legend.title =
            element_text(color = "red", hjust = 0, family = "mono"))
  )

test_that("Title options", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(title = "Test Title"))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(title = expression(Test[5]*Sigma)))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(
        title = "Test\nTwo Lines",
        title.position = "bottom",
        title.hjust = 1,
        title.vjust = 1.5))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(
        title = "Test\nTwo Lines",
        title.position = "bottom",
        title.theme = text_theme))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(
        title = "Test\nTwo Lines",
        title.position = "bottom",
        title.theme = element_text(color = "red")))
  ))
})

test_that("Axis title options", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis_title = "Test Title",
                                       axis_title_y = "Y test Title"))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis_title = expression(Test[5]*Sigma),
                                       axis_title_y = expression(Test^Y*sigma)))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis_title.position = c("top", "bottom"),
                                       axis_title_y.position = c("left", "right")))
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis_title.position = "bottom",
                                       axis_title_y.position = "right"))
  ))

})
