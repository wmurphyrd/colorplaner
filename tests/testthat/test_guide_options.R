text_theme <- calc_element(
  "legend.title",
  theme_bw() %+%
    theme(legend.title =
            element_text(color = "red", family = "mono"))
  )

test_that("Title options", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(title = "Test Title")) +
      ggtitle("Bright colors, facet, guide title: Test Title")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(title = expression(Test[5]*Sigma))) +
      ggtitle("Bright, facets, guide title: expression")
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
        title.vjust = 1.5)) +
      ggtitle("Bright, facets, guide title: bottom, right just")
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
        title.theme = text_theme,
        title.hjust = 0)) +
      ggtitle("Bright, facets, title: bottom, red, left just, mono font")
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
        title.theme = element_text(color = "red"))) +
      ggtitle("Guide title: bottom, red")
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
        title.theme = theme_void())) +
      ggtitle("Bright, facets, title: invisible")
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
                                       axis_title_y = "Y test Title")) +
      ggtitle("Guide axis titles")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis_title = expression(Test[5]*Sigma),
                                       axis_title_y = expression(Test^Y*sigma),
                                       axis_title.theme = element_text(
                                         color = "blue", face = "bold",
                                         size = 18),
                                       axis_title_y.theme = text_theme)) +
      ggtitle("Guide axis titles: expressions, red&rotate y, blue x")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color =
               guide_colourplane(axis_title.position = c("top", "bottom"),
                                 axis_title_y.position = c("left", "right"))) +
      ggtitle("Guide axis titles on both sides")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis_title.position = "bottom",
                                       axis_title_y.position = "right")) +
      ggtitle("Guide axis titles: bottom, right")
  ))

})
