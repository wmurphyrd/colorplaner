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

data("iris")

test_that("Faceted plots", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_point() +
      scale_colour_colourplane(Y = .5) +
      facet_grid(~Species) +
      ggtitle("Facet, bright scale colors")
  ))
})

test_that("Grouped plots", {
  expect_silent(print(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species,
                     colour = Petal.Length, color2 = Petal.Width)) +
      geom_path() +
      scale_colour_colourplane() +
      ggtitle("Grouped lines")
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
      scale_colour_colourplane() +
      ggtitle("Points over histogram, petal width > 1, length > 3")
  ))
})
