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
