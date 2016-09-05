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

