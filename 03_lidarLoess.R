#####
# 3. Lidar LOESS
# 15/07/2019
#####

# LOESS
# locally estimated scatterplot smoothing

# Libraries ####
library(xtable)
library(mgcv)
library(splines)
library(MASS)
library(tidyverse)
library(gridExtra)

# Set plot themes
theme_set(theme_bw())


# Plot lidar ####

data_legend <- tibble(x = rep(mean(lidar$range), 6),
                      y = rep(mean(lidar$logratio),6),
                      degree = as.character(c(0,0,1,1,2,2)))

ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point() +
  geom_smooth(method = "loess", se = F, span = .7,
              method.args = list(degree = 2), color = "red") +
  geom_smooth(method = "loess", se = F, span = .3,
              method.args = list(degree = 1), color = "blue") +
  geom_smooth(method = "loess", se = F, span = .3,
              method.args = list(degree = 0), color = "green") +
  geom_line(data = data_legend,
            aes(x = x, y = y, color = degree)) +
  scale_color_manual(values = c("green", "blue", "red")) +
  labs(title = "Lidar dataset",
       subtitle = "Local Polynomial Regression")



hist(lidar$range)
barplot(table(lidar$range))








