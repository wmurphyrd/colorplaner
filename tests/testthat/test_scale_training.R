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

aqd <- airquality[complete.cases(airquality), ]
aqm <- lm(Ozone ~ Temp, aqd)
aqd <- cbind(aqd, predict(aqm, newdata = aqd,
                          interval = "prediction"))

test_that("Plots with static color layers succeed", {
  expect_silent(print(
    ggplot(aqd, aes(x = Temp, y = Ozone, color = Solar.R, color2 = Wind)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .35, color = NA) +
      geom_point() +
      geom_rug() +
      scale_color_colorplane() +
      ggtitle("Grey ribbon (no outline) behind points")
  ))
})

test_that("Plots with no valid mapping give informative message", {
  expect_message({
    print(ggplot(aqd, aes(x = Temp, y = Ozone, color= Solar.R)) +
      geom_point() +
      scale_color_colorplane() +
      ggtitle("Mismapped colors, no guide"))
  }, "scale_color_colorplane requires both color and color2")
  expect_message({
    print(ggplot(aqd, aes(x = Temp, y = Ozone, color= Solar.R)) +
      geom_point() +
      scale_fill_colorplane() +
      ggtitle("Mismapped colors, colorbar guide"))
  }, "scale_fill_colorplane requires both fill and fill2")
})

test_that("Layer-level mappings produce guide axis titles without error", {
  expect_error(
    print(ggplot(aqd, aes(x = Temp, y = Ozone)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .35) +
      geom_point(aes(color = Month, color2 = Day)) +
      geom_rug() +
      scale_color_colorplane() +
      ggtitle("Points over grey ribbon")),
    NA)
})
