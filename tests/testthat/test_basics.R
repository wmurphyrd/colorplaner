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
  expect_silent({
    ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
      geom_point() +
      scale_color_colorplane(trans = "log")
    ggplot(mtcars, aes(x = wt, y = mpg, fill = disp, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane(trans = "log")
    })
})

test_that("OOB and NA data handled without error", {
  expect_silent({
    ggplot(mtcars, aes(x = wt, y = mpg, color = qsec, colour2 = hp)) +
      geom_point(size = 4) +
      scale_color_colorplane(limits = c(NA, 18.9), limits_y = c(60, 300),
                             na.color = "yellow")
    ggplot(mtcars, aes(x = wt, y = mpg, fill = qsec, fill2 = hp)) +
      geom_point(shape = 23, size = 5) +
      scale_fill_colorplane(limits = c(NA, 18.9), limits_y = c(60, 300),
                            na.color = "yellow")
    })

})

test_that("Scale constructors are in sync", {
  expect_identical(formals(scale_color_colorplane),
                   formals(scale_fill_colorplane))
})

test_that("Arguments can be passed to projection functions", {
  expect_silent({
    ggplot(mtcars, aes(x = wt, y = mpg, color = qsec, colour2 = hp)) +
    geom_point(size = 4) +
    scale_color_colorplane(limits = c(NA, 18.9), limits_y = c(60, 300),
                           na.color = "red", Y = .9)
  })
})

