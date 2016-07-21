test_that("Basic plotting occurs without error", {
  expect_silent(
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
    geom_point() +
    scale_color_colorplane(guide = guide_colorplane())
  )
})
