library(ggplot2)

ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  stat_colorplane(aes(bg = hp, fg = disp)) +
  scale_color_identity()
