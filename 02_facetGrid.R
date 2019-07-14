#####
# 2.plot facet
# 15/07/2019
#####


n <- 100
x <- sort(runif(n,0,1))
truefun <- function(x){sin(x*2*pi) * (x<0.5) + 0}
y <- truefun(x) + rnorm(n,0,0.1)


grid = seq(from = 0, to = 1, by = .001)
# grid = seq(from = 0, to = 1, by = .1)


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
  filter(grid %in% c(.1, .3, .5, .8))

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
  filter(grid %in% c(.1, .3, .5, .8))

data_fit2_cross_small <- crossing(data_fit2_grid_small, data)



# Union of datasets
data_grid_small <- rbind(data_fit1_grid_small %>% 
                           mutate(span = span1),
                         data_fit2_grid_small %>% 
                           mutate(span = span2))

data_cross_small <- rbind(data_fit1_cross_small %>%
                            mutate(span = span1),
                          data_fit2_cross_small %>% 
                            mutate(span = span2))

data_grid2 <- rbind(data_fit1_grid2 %>% 
                      mutate(span = span1),
                    data_fit2_grid2 %>% 
                      mutate(span = span2))


# Plotting
data_fit1_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  geom_point(data = data_fit1_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span))) +
  geom_line(data = data_fit1_grid2,
            aes(x = x, y = y),
            col = "brown") +
  geom_point(color = "red", size = 2) +
  geom_abline(aes(intercept = a0, slope = a1)) +
  facet_grid(~grid) +
  theme(legend.position = "none")


data_grid_small %>% 
  ggplot(aes(x = grid, y = stima)) +
  geom_point(data = data_cross_small,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span))) +
  geom_line(data = data_grid2,
            aes(x = x, y = y),
            col = "brown") +
  geom_point(color = "red", size = 2) +
  geom_abline(aes(intercept = a0, slope = a1)) +
  facet_grid(span~grid) +
  theme(legend.position = "none")




