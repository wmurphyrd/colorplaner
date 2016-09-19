# This file is part of colorplaner
#
# colorplaner is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# colorplaner is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with colorplaner.  If not, see <http://www.gnu.org/licenses/>.

text_theme <- theme_bw() %+%
    theme(text =
            element_text(color = "red", family = "mono"))

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
      guides(color = guide_colourplane(axis.title = "Test Title",
                                       axis.title.y = "Y test Title")) +
      ggtitle("Guide axis titles")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis.title = expression(Test[5]*Sigma),
                                       axis.title.y = expression(Test^Y*sigma),
                                       axis.title.theme = element_text(
                                         color = "blue", face = "bold",
                                         size = 18),
                                       axis.title.y.theme = text_theme)) +
      ggtitle("Guide axis titles: expressions, red&mono y, blue x")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color =
               guide_colourplane(axis.title.position = c("top", "bottom"),
                                 axis.title.y.position = c("left", "right"))) +
      ggtitle("Guide axis titles on both sides")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color =
               guide_colourplane(axis.title.position = c("top", "bottom"),
                                 axis.title.y.position = c("left", "right"),
                                 axis.title = "This is an extra extra long x axis title",
                                 axis.title.y = "This is an extra extra long y axis title")) +
      ggtitle("Long guide axis titles on both sides")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      guides(color = guide_colourplane(axis.title.position = "bottom",
                                       axis.title.y.position = "right")) +
      ggtitle("Guide axis titles: bottom, right")
  ))

})

test_that("Label options", {
  expect_silent(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(ticks = F, label = F)) +
      ggtitle("No ticks, no labels")
  ))
  expect_silent(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(label.hjust = 0, label.vjust = 0,
                                 label.y.hjust = 1, label.y.vjust = 1)) +
      ggtitle("Guide label justifications: right, down")
  ))
  expect_silent(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(
          label.position = "top", label.y.position = "right",
          label.theme = element_text(color = "green", size = 7),
          label.y.theme = text_theme
        )) +
      ggtitle("Guide labels: x green&small&top, y red&mono&right")
  ))
  expect_silent(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(
          label.position = c("bottom", "top"),
          label.y.position = c("right", "left")
        )) +
      ggtitle("Guide labels: both sides")
  ))
  expect_error(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(
          label.position = c("both"),
          label.y.position = c("right", "left")
        ))
  ), "should be one of .bottom., .top.")
  expect_error(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(
          label.y.position = c("hello")
        ))
  ), "should be one of .left., .right.")
})

test_that("Plane options", {
  expect_silent(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(
          planewidth = 3,
          planeheight = 4)) +
      ggtitle("Plane taller than wide")
  ))
  expect_silent(print(
    ggplot(mtcars, aes(x = drat, y = mpg,
                       color = hp, color2 = disp/max(disp))) +
      geom_point() +
      scale_color_colorplane(
        guide = guide_colorplane(
          planewidth = grid::unit(15, "mm"),
          planeheight = grid::unit(10, "mm"))) +
      ggtitle("Plane wider than tall")
  ))
})
