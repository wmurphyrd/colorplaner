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

test_that("Alt projection red_blue can be used", {
  expect_silent({
    print(ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
            geom_point() +
            scale_color_colorplane(color_projection = "red_blue") +
            ggtitle("Red-blue color projection"))

    print(ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
            geom_point() +
            scale_color_colorplane(color_projection = red_blue_projection) +
            ggtitle("Red-blue color projection"))
  })
})

test_that("Interpolation projection can be used", {
  expect_silent({
    print(ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
            geom_point() +
            scale_color_colorplane(color_projection = "interpolate",
                                   zero_color = "darkorange2",
                                   horizontal_color = "mediumspringgreen",
                                   vertical_color = "#CD00CD") +
            ggtitle("Orange-green-magenta projection"))
  })
})

test_that("Invalid Y arguments handled by YUV_projection", {
  expect_warning(print(
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
      geom_point() +
      scale_color_colorplane(Y = c(.5, 1, 1, 1, 1)) +
      ggtitle("normal colorplane points")
  ), "YUV_projection: Y argument length > 1, using first element", fixed = TRUE)

  expect_error(print(
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
      geom_point() +
      scale_color_colorplane(Y = "hello") +
      ggtitle("error")
  ), "Invalid Y specification. Needs numeric in [0,1]", fixed = TRUE)

  expect_error(print(
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
      geom_point() +
      scale_color_colorplane(Y = 2) +
      ggtitle("error")
  ), "Invalid Y specification. Needs numeric in [0,1]", fixed = TRUE)
})


