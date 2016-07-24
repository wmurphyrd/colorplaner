test_that("Basic plotting occurs without error", {
  expect_silent({
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
    geom_point() +
    scale_color_colorplane()

    ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane()

    ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp,
                       color = gear, color2 = carb)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane() +
      scale_color_colorplane()
  })
})

test_that("Transformed colorplane scale plots without error", {
  expect_silent(ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
                  geom_point() +
                  scale_color_colorplane(trans = "log"))
})

test_that("OOB and NA data handled without error", {
  expect_silent(ggplot(mtcars, aes(x = wt, y = mpg, color = qsec, colour2 = hp)) +
                  geom_point(size = 4) +
                  scale_color_colorplane(limits = c(NA, 18.9),
                                         na.color = "yellow"))
})
