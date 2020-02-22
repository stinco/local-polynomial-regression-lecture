#####
# 2.plot facet
# 15/07/2019
#####


# Libraries ####
library(xtable)
library(mgcv)
library(splines)
library(MASS)
library(tidyverse)
library(gridExtra)

# Set plot themes
theme_set(theme_bw())


# Generate data ####
set.seed(42)
n <- 100
x <- sort(runif(n,0,1))
truefun <- function(x){sin(x*2*pi) * (x<0.5) + 0}
y <- truefun(x) + rnorm(n,0,0.1)


grid = seq(from = 0, to = 1, by = .001)
# grid = seq(from = 0, to = 1, by = .1)


# Linear loess ####
my_loess <- function(x, y, grid, span){
  stima <- rep(NA, length(grid))
  
  a0 <- rep(NA, length(grid))
  a1 <- rep(NA, length(grid))
  
  # Computing
  for (i in 1:length(grid)){
    fit <- lm(y~x, weights = dnorm(x, grid[i], span))
    a0[i] <- coef(fit)[1]
    a1[i] <- coef(fit)[2]
    stima[i] = predict(fit, data.frame(x = grid[i]))
  }
  return(list(stima = stima,
              a0 = a0,
              a1 = a1))
}




# Fit models

span1 <- .1
span2 <- .04

interesting_points <- c(.1, .25, .4, .8)


fit_myloess_1 <- my_loess(x, y, grid, span = span1)
fit_myloess_2 <- my_loess(x, y, grid, span = span2)



# Fit 1
stima <- fit_myloess_1$stima
a0 <- fit_myloess_1$a0
a1 <- fit_myloess_1$a1


# Create dataframes
data_fit1_grid <- tibble(grid,
                    a0, a1,
                    stima)

data_fit1_grid2 <- data_fit1_grid %>% 
  select(x = grid, y = stima)


data <- tibble(x, y)

data_fit1_grid_small <- data_fit1_grid %>% 
  filter(grid %in% interesting_points)

data_fit1_cross_small <- crossing(data_fit1_grid_small, data)



# Fit 2
stima <- fit_myloess_2$stima
a0 <- fit_myloess_2$a0
a1 <- fit_myloess_2$a1

# Create dataframes
data_fit2_grid <- tibble(grid,
                         a0, a1,
                         stima)

data_fit2_grid2 <- data_fit2_grid %>% 
  select(x = grid, y = stima)


# data <- tibble(x, y)

data_fit2_grid_small <- data_fit2_grid %>% 
  filter(grid %in% interesting_points)

data_fit2_cross_small <- crossing(data_fit2_grid_small, data)



# Union of datasets
data_grid_small <- rbind(data_fit1_grid_small %>% 
                           mutate(span = span1),
                         data_fit2_grid_small %>% 
                           mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))

data_cross_small <- rbind(data_fit1_cross_small %>%
                            mutate(span = span1),
                          data_fit2_cross_small %>% 
                            mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))

data_grid2 <- rbind(data_fit1_grid2 %>% 
                      mutate(span = span1),
                    data_fit2_grid2 %>% 
                      mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))


# Plotting

data_fit1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span1) / dnorm(0, mean = 0, sd = span1))) +
  geom_line(data = data_fit1_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_abline(aes(intercept = a0, slope = a1),
              color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "")


p_loess_linear <- data_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span) / dnorm(0, mean = 0, sd = span))) +
  geom_line(data = data_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_abline(aes(intercept = a0, slope = a1),
              color = "red") +
  facet_grid(span_str~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "", title = "Linear loess")





# Quadratic loess ####

my_loess_quad <- function(x, y, grid, span){
  stima <- rep(NA, length(grid))
  
  a0 <- rep(NA, length(grid))
  a1 <- rep(NA, length(grid))
  a2 <- rep(NA, length(grid))
  
  # Computing
  for (i in 1:length(grid)){
    fit <- lm(y ~ x + I(x^2), weights = dnorm(x, grid[i], span))
    a0[i] <- coef(fit)[1]
    a1[i] <- coef(fit)[2]
    a2[i] <- coef(fit)[3]
    stima[i] = predict(fit, data.frame(x = grid[i]))
  }
  return(list(stima = stima,
              a0 = a0,
              a1 = a1,
              a2 = a2))
}


# Fit models

fit_myloess_quad_1 <- my_loess_quad(x, y, grid, span = span1)
fit_myloess_quad_2 <- my_loess_quad(x, y, grid, span = span2)



# Fit 1
stima <- fit_myloess_quad_1$stima
a0 <- fit_myloess_quad_1$a0
a1 <- fit_myloess_quad_1$a1
a2 <- fit_myloess_quad_1$a2


# Create dataframes
data_fit_quad1_grid <- tibble(grid,
                         a0, a1, a2,
                         stima)

data_fit_quad1_grid2 <- data_fit_quad1_grid %>% 
  select(x = grid, y = stima)


data <- tibble(x, y)

data_fit_quad1_grid_small <- data_fit_quad1_grid %>% 
  filter(grid %in% interesting_points)

data_fit_quad1_cross_small <- crossing(data_fit_quad1_grid_small, data)



# Fit 2
stima <- fit_myloess_quad_2$stima
a0 <- fit_myloess_quad_2$a0
a1 <- fit_myloess_quad_2$a1
a2 <- fit_myloess_quad_2$a2

# Create dataframes
data_fit_quad2_grid <- tibble(grid,
                         a0, a1, a2,
                         stima)

data_fit_quad2_grid2 <- data_fit_quad2_grid %>% 
  select(x = grid, y = stima)


# data <- tibble(x, y)

data_fit_quad2_grid_small <- data_fit_quad2_grid %>% 
  filter(grid %in% interesting_points)

data_fit_quad2_cross_small <- crossing(data_fit_quad2_grid_small, data)



# Union of datasets
data_quad_grid_small <- rbind(data_fit_quad1_grid_small %>% 
                           mutate(span = span1),
                         data_fit_quad2_grid_small %>% 
                           mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))

data_quad_cross_small <- rbind(data_fit_quad1_cross_small %>%
                            mutate(span = span1),
                          data_fit_quad2_cross_small %>% 
                            mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))

data_quad_grid2 <- rbind(data_fit_quad1_grid2 %>% 
                      mutate(span = span1),
                    data_fit_quad2_grid2 %>% 
                      mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))



data_fit_quad1_grid_small

grid

data_parabola_1 <- crossing(data_fit_quad1_grid_small, tibble(x = seq(from = -.1, to = 1.1, by = 0.001))) %>% 
  mutate(y = a0 + a1*x + a2*x^2)

data_parabola_2 <- crossing(data_fit_quad2_grid_small, tibble(x = seq(from = -.1, to = 1.1, by = 0.001))) %>% 
  mutate(y = a0 + a1*x + a2*x^2)

data_parabola <- rbind(data_parabola_1 %>% 
                         mutate(span = span1),
                       data_parabola_2 %>% 
                         mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))



# Plotting
data_fit_quad1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit_quad1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span) / dnorm(0, mean = 0, sd = span))) +
  geom_line(data = data_fit_quad1_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_line(data = data_parabola_1,
            aes(x = x, y = y),
            color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2))



p_loess_quadratic <- data_quad_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_quad_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span) / dnorm(0, mean = 0, sd = span))) +
  geom_line(data = data_quad_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_line(data = data_parabola,
            aes(x = x, y = y),
            color = "red") +
  facet_grid(span_str~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "", title = "Quadratic loess")



grid.arrange(p_loess_linear,
             p_loess_quadratic,
             ncol = 1)




# Cubic loess ####

my_loess_cub <- function(x, y, grid, span){
  stima <- rep(NA, length(grid))
  
  a0 <- rep(NA, length(grid))
  a1 <- rep(NA, length(grid))
  a2 <- rep(NA, length(grid))
  a3 <- rep(NA, length(grid))
  
  # Computing
  for (i in 1:length(grid)){
    fit <- lm(y ~ x + I(x^2) + I(x^3), weights = dnorm(x, grid[i], span))
    a0[i] <- coef(fit)[1]
    a1[i] <- coef(fit)[2]
    a2[i] <- coef(fit)[3]
    a3[i] <- coef(fit)[4]
    stima[i] = predict(fit, data.frame(x = grid[i]))
  }
  return(list(stima = stima,
              a0 = a0,
              a1 = a1,
              a2 = a2,
              a3 = a3))
}


# Fit models

fit_myloess_cub_1 <- my_loess_cub(x, y, grid, span = span1)
fit_myloess_cub_2 <- my_loess_cub(x, y, grid, span = span2)



# Fit 1
stima <- fit_myloess_cub_1$stima
a0 <- fit_myloess_cub_1$a0
a1 <- fit_myloess_cub_1$a1
a2 <- fit_myloess_cub_1$a2
a3 <- fit_myloess_cub_1$a3


# Create dataframes
data_fit_cub1_grid <- tibble(grid,
                              a0, a1, a2, a3,
                              stima)

data_fit_cub1_grid2 <- data_fit_cub1_grid %>% 
  select(x = grid, y = stima)


data <- tibble(x, y)

data_fit_cub1_grid_small <- data_fit_cub1_grid %>% 
  filter(grid %in% interesting_points)

data_fit_cub1_cross_small <- crossing(data_fit_cub1_grid_small, data)



# Fit 2
stima <- fit_myloess_cub_2$stima
a0 <- fit_myloess_cub_2$a0
a1 <- fit_myloess_cub_2$a1
a2 <- fit_myloess_cub_2$a2
a3 <- fit_myloess_cub_2$a3


# Create dataframes
data_fit_cub2_grid <- tibble(grid,
                              a0, a1, a2, a3,
                              stima)

data_fit_cub2_grid2 <- data_fit_cub2_grid %>% 
  select(x = grid, y = stima)


# data <- tibble(x, y)

data_fit_cub2_grid_small <- data_fit_cub2_grid %>% 
  filter(grid %in% interesting_points)

data_fit_cub2_cross_small <- crossing(data_fit_cub2_grid_small, data)


# Union of datasets
data_cub_grid_small <- rbind(data_fit_cub1_grid_small %>% 
                                mutate(span = span1),
                              data_fit_cub2_grid_small %>% 
                                mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))

data_cub_cross_small <- rbind(data_fit_cub1_cross_small %>%
                                 mutate(span = span1),
                               data_fit_cub2_cross_small %>% 
                                 mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))

data_cub_grid2 <- rbind(data_fit_cub1_grid2 %>% 
                           mutate(span = span1),
                         data_fit_cub2_grid2 %>% 
                           mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))



data_fit_cub1_grid_small

grid

data_cubic_1 <- crossing(data_fit_cub1_grid_small, tibble(x = seq(from = -.1, to = 1.1, by = 0.001))) %>% 
  mutate(y = a0 + a1*x + a2*x^2 + a3*x^3)

data_cubic_2 <- crossing(data_fit_cub2_grid_small, tibble(x = seq(from = -.1, to = 1.1, by = 0.001))) %>% 
  mutate(y = a0 + a1*x + a2*x^2 + a3*x^3)

data_cubic <- rbind(data_cubic_1 %>% 
                         mutate(span = span1),
                       data_cubic_2 %>% 
                         mutate(span = span2)) %>% 
  mutate(span_str = str_c("span = ", span))



# Plotting
data_fit_cub1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit_cub1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span) / dnorm(0, mean = 0, sd = span))) +
  geom_line(data = data_fit_cub1_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_line(data = data_cubic_1,
            aes(x = x, y = y),
            color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2))



p_loess_cubic <- data_cub_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_cub_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span) / dnorm(0, mean = 0, sd = span))) +
  geom_line(data = data_cub_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_line(data = data_cubic,
            aes(x = x, y = y),
            color = "red") +
  facet_grid(span_str~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "", title = "Cubic loess")



grid.arrange(p_loess_linear,
             p_loess_quadratic,
             p_loess_cubic,
             ncol = 1)






# Explaining the process of LOESS ####

data_fit1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  # stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span1) / dnorm(0, mean = 0, sd = span1))) +
  # geom_line(data = data_fit1_grid2,
  #           aes(x = x, y = y),
  #           col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_abline(aes(intercept = a0, slope = a1),
              color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "")

data_fit1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  # stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span1) / dnorm(0, mean = 0, sd = span1))) +
  geom_line(data = data_fit1_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  # geom_abline(aes(intercept = a0, slope = a1),
  # color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "")

data_fit1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span1) / dnorm(0, mean = 0, sd = span1))) +
  geom_line(data = data_fit1_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  # geom_abline(aes(intercept = a0, slope = a1),
  #             color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "")

data_fit1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  stat_function(fun = truefun, size = 1) +
  geom_point(data = data_fit1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span1) / dnorm(0, mean = 0, sd = span1))) +
  geom_line(data = data_fit1_grid2,
            aes(x = x, y = y),
            col = "brown", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_abline(aes(intercept = a0, slope = a1),
              color = "red") +
  facet_grid(~grid) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "gray90", high = "black") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.2)) +
  labs(x = "", y = "")


