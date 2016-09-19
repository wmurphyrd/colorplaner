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
      scale_color_colorplane(labels = function(x)rep(expression(sigma), length(x)),
                             labels_y = function(x)rep(expression(beta), length(x))) +
      ggtitle("guide labels expressions: x sigmas, y betas")
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
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     color = Sepal.Width / max(Sepal.Width),
                     color2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter") +
      scale_color_colorplane(breaks = NULL) +
      ggtitle("No guide")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     color = Sepal.Width / max(Sepal.Width),
                     color2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter") +
      scale_color_colorplane(breaks_y = NULL) +
      ggtitle("No guide")
  ))
  expect_error(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     color = Sepal.Width / max(Sepal.Width),
                     color2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter") +
      scale_color_colorplane(breaks = seq(0.5, 0.8, by = .1),
                             labels = c("5, 6, 7"))
  ), "breaks. and .labels. must have the same length")
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
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     fill = Sepal.Width / max(Sepal.Width),
                     fill2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter", shape = 23, size = 5) +
      scale_fill_colorplane(breaks = NULL) +
      ggtitle("no guide")
  ))
  expect_silent(print(
    ggplot(iris, aes(x = Petal.Width, y = Petal.Length,
                     fill = Sepal.Width / max(Sepal.Width),
                     fill2 = Sepal.Length,
                     shape = Species)) +
      geom_point(position = "jitter", shape = 23, size = 5) +
      scale_fill_colorplane(breaks_y = NULL) +
      ggtitle("no guide")
  ))

})
