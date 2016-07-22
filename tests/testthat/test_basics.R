test_that("Basic plotting occurs without error", {
  expect_silent({
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
    geom_point() +
    scale_color_colorplane(guide = guide_colorplane())

    ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane(guide = guide_colorplane())

    ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp,
                       color = gear, color2 = carb)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane(guide = guide_colorplane()) +
      scale_color_colorplane(guide = guide_colorplane())
  })
})
