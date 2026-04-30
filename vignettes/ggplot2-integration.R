## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  dpi = 300,
  dev = "ragg_png"
)
library(gridmicrotex)
library(ggplot2)

## ----geom-basic, out.width="70%"----------------------------------------------
df <- data.frame(
  x = 1:3,
  y = 1:3,
  eq = c("$x^2$", "\\frac{a}{b}", "$\\sum_{i=1}^n x_i$"),
  col = c("red", "blue", "green")
)

ggplot(df, aes(x, y, 
               label = eq, 
               colour = col, 
               size = c(14, 18, 14))) +
  geom_latex() +
  scale_colour_identity() +
  scale_size_identity() +
  theme_minimal()

## ----regression-annotation, out.width="70%"-----------------------------------
fit <- lm(mpg ~ wt, data = mtcars)
b0 <- round(coef(fit)[1], 1)
b1 <- round(coef(fit)[2], 1)
r2 <- round(summary(fit)$r.squared, 3)

eq_label <- sprintf("$\\hat{y} = %s %s x, \\quad R^2 = %s$",
                     b0, b1, r2)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("latex", x = 4, y = 30, label = eq_label, size = 12) +
  theme_minimal()

## ----element-axis, out.width="70%"--------------------------------------------
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  labs(
    x = "$\\beta_1 \\cdot x + \\beta_0$",
    y = "$\\mathrm{mpg}$"
  ) +
  theme(
    axis.title.x = element_latex(fontsize = 14),
    axis.title.y = element_latex(fontsize = 14)
  )

