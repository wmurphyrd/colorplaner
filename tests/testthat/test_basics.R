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

test_that("Basic plotting occurs without error", {
  expect_silent({
    print(ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
    geom_point() +
    scale_color_colorplane() +
    ggtitle("Colored points"))

    print(ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane() +
      ggtitle("Filled diamond points"))

    print(ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp,
                       color = gear, color2 = carb)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane() +
      scale_color_colorplane() +
      ggtitle("Filled and colored diamonds"))
  })
})

test_that("Transformed colorplane scale plots without error", {
  expect_silent({
    print(ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
      geom_point() +
      scale_color_colorplane(trans = "log") +
      ggtitle("Log trans colored points"))
    print(ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane(trans = "log") +
      ggtitle("Log trans filled diamonds"))
    })
})

test_that("OOB and NA data handled without error", {
  expect_silent({
    print(ggplot(mtcars, aes(x = wt, y = mpg, color = qsec, colour2 = hp)) +
      geom_point(size = 4) +
      scale_color_colorplane(limits = c(NA, 18.9), limits_y = c(60, 300),
                             na.color = "yellow") +
      ggtitle("Yellow out-of-bounds points"))
    print(ggplot(mtcars, aes(x = wt, y = mpg, fill = qsec, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane(limits = c(NA, 18.9), limits_y = c(60, 300),
                            na.color = "yellow") +
      ggtitle("Yellow out-of-bounds diamonds"))
    })

})

test_that("Scale constructors are in sync", {
  expect_identical(formals(scale_color_colorplane),
                   formals(scale_fill_colorplane))
})

test_that("Arguments can be passed to projection functions", {
  expect_silent({
    print(ggplot(mtcars, aes(x = wt, y = mpg, color = qsec, colour2 = hp)) +
            geom_point(size = 4) +
            scale_color_colorplane(limits = c(NA, 18.9), limits_y = c(60, 300),
                                   na.color = "red", Y = .9) +
            ggtitle("Bright colored points"))
  })
})

test_that("Attachment warning given when appropriate", {
  detach("package:colorplaner")
  expect_warning(
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
      geom_point() +
      colorplaner::scale_color_colorplane(guide =
                                            colorplaner::guide_colorplane()),
    "colorplaner must be attached")

})

#reattach package after detachment for testing
library(colorplaner)
