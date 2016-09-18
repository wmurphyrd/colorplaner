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


