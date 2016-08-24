aqd <- airquality[complete.cases(airquality), ]
aqm <- lm(Ozone ~ Temp, aqd)
aqd <- cbind(aqd, predict(aqm, newdata = aqd,
                          interval = "prediction"))

test_that("Plots with static color layers succeed", {
  expect_silent(
    print(ggplot(aqd, aes(x = Temp, y = Ozone, color = Solar.R, color2 = Wind)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .35, color = NA) +
    geom_point() +
    geom_rug() +
    scale_color_colorplane())
  )
})

test_that("Plots with no valid mapping give informative message", {
  expect_message({
    print(ggplot(aqd, aes(x = Temp, y = Ozone, color= Solar.R)) +
      geom_point() +
      scale_color_colorplane())
  }, "scale_color_colorplane requires both color and color2")
  expect_message({
    print(ggplot(aqd, aes(x = Temp, y = Ozone, color= Solar.R)) +
      geom_point() +
      scale_fill_colorplane())
  }, "scale_fill_colorplane requires both fill and fill2")
})
