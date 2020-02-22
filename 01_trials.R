#####
# 1. Trials from lucidi5
# 14/07/2019
#####
# Libraries ####
library(xtable)
library(mgcv)
library(splines)
library(MASS)
library(tidyverse)

# Set plot themes
theme_set(theme_bw())

# Read data ####

cmb <- read.table("data/wmap.dat",header=TRUE)
lidar <- read.table("data/lidar.dat",header=TRUE)
lidar <- lidar[sort.list(lidar$range),]
bpd <- read.table("data/bpd.dat",header=TRUE)

# View(cmb)
# View(lidar)
# View(bpd)


# Plot lidar ####

plot(lidar$range, lidar$logratio,
     pch=20, xlab = "range (standardized)", ylab = "logratio")

ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point()


# default span = .75
ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point() +
  geom_smooth(method = "loess", se = F, span = .75)

fit0 <- loess(data = lidar,
              formula = logratio ~ range,
              span = .75, degree = 0)

fit1 <- loess(data = lidar,
              formula = logratio ~ range,
              span = .75, degree = 1)

fit2 <- loess(data = lidar,
              formula = logratio ~ range,
              span = .75, degree = 2)

range <- seq(from = range(lidar$range)[1],
    to = range(lidar$range)[2],
    by = 1)

pred_tab <- tibble(range)

pred_tab <- pred_tab %>% 
  mutate(pred0 = predict(fit0, select(pred_tab,range)),
         pred1 = predict(fit1, select(pred_tab,range)),
         pred2 = predict(fit2, select(pred_tab,range)))

ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point() +
  geom_smooth(method = "loess", se = F, span = .75) +
  geom_line(data = pred_tab, aes(x = range, y = pred0), col = "black") +
  geom_line(data = pred_tab, aes(x = range, y = pred1), col = "red") +
  geom_line(data = pred_tab, aes(x = range, y = pred2), col = "green")


summary(fit1)


# library(KernSmooth)
# ?locpoly
# 
# 
# fit0 <- locpoly(x = lidar$range, y = lidar$logratio,
#                 bandwidth = .75, degree = 0)
# 
# fit1 <- locpoly(x = lidar$range, y = lidar$logratio,
#                 bandwidth = .75, degree = 1)
# 
# fit2 <- locpoly(x = lidar$range, y = lidar$logratio,
#                 bandwidth = .75, degree = 2)
# 
# fit3 <- locpoly(x = lidar$range, y = lidar$logratio,
#                 bandwidth = .75, degree = 3)
# 
# plot(fit0, type = "l")
# plot(fit1, type = "l")
# plot(fit2, type = "l")
# plot(fit3, type = "l")



x0 <- 500

ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point() +
  geom_smooth(method = "loess", se = F, span = .75)


n=100
x=sort(runif(n,0,1))
truefun=function(x) sin(x*2*pi)*(x<0.5)+0
y=truefun(x)+rnorm(n,0,0.1)
save(x,y,file="sim2.Rdata")
layout(matrix(c(1,1,2),ncol=1))
par(mar=c(2,2,0.2,0.2),cex=1.4)
stima=rep(NA,length(x))
for (i in 1:length(x)){
  plot(x,y,xaxt="n")
  fit=lm(y~x,weights=dnorm(x,x[i],0.1))
  abline(coef(fit))
  stima[i]=fit$fitted[i]
  points(x,y,col=gray(1-dnorm(x,x[i],0.1)/(1/(sqrt(2*pi)*0.1))),pch=20)
  points(x[i],y[i],col="green",pch=20)
  axis(1,at=x[i],lab=expression(x[i]))
  segments(x[i],-1,x[i],y[i])
  if (i>1)   lines(x[1:(i)],stima[1:(i)],pch=20,col="brown",lwd=2)
  points(x[i],stima[i],pch=20,col="red",cex=1.3)
  curve(dnorm(x,x[i],0.1),from=min(x),to=max(x))
}



span = 0.1

n <- 100
x <- sort(runif(n,0,1))
truefun <- function(x){sin(x*2*pi) * (x<0.5) + 0}
y <- truefun(x) + rnorm(n,0,0.1)
# save(x,y,file="sim2.Rdata")
layout(matrix(c(1,1,2), ncol = 1))
par(mar = c(2,2,0.2,0.2), cex = 1.4)

grid = seq(from = 0, to = 1, by = .001)
# grid = seq(from = 0, to = 1, by = .1)

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

# Ploting
i = length(grid) %/% 2

plot(x, y, xaxt = "n",
     xlim = grid[c(1,length(grid))])
abline(a0[i], a1[i])

points(x, y, col = gray(1 - dnorm(x,grid[i],span) / (1/(sqrt(2*pi)*span))), pch = 20)

axis(1, at = grid[i], lab = expression(x[i]))
segments(grid[i], -1, grid[i], stima[i])
if (i>1){
  lines(grid[1:i], stima[1:i], pch = 20, col = "brown", lwd = 2)
}
points(grid[i], stima[i], pch=20, col = "red", cex = 1.3)
curve(dnorm(x, grid[i], span), from = grid[1], to = grid[length(grid)])

  
  
data_grid <- tibble(grid,
                    a0, a1,
                    stima)

data_grid2 <- data_grid %>% 
  select(x = grid, y = stima)

data <- tibble(x, y)

data_cross <- crossing(data_grid, data)


data_cross %>% 
  filter(grid == .4) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()
  
# data_grid %>% 
#   ggplot(aes(x = grid, y = stima)) +
#   geom_point(data = data_cross,
#              aes(x = x, y = y,
#                  color = dnorm(x, mean = grid, sd = span))) +
#   geom_line(data = data_grid2,
#             aes(x = x, y = y),
#             col = "brown") +
#   geom_point(color = "red") +
#   geom_abline(aes(intercept = a0, slope = a1)) +
#   facet_wrap(~grid) +
#   theme(legend.position = "none")



data_grid_small <- data_grid %>% 
  filter(grid %in% c(.1, .3, .5, .8))

data_cross_small <- crossing(data_grid_small, data)


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
  facet_grid(~grid) +
  theme(legend.position = "none")





p <- data_grid %>% 
  ggplot(aes(x = grid, y = stima, frame = grid)) +
  geom_point(data = data_cross,
             aes(x = x, y = y,
                 color = dnorm(x, mean = grid, sd = span))) +
  geom_point(data = data_grid2,
            aes(x = x, y = y)) +
  # geom_line(data = data_grid2,
  #           aes(x = grid, y = stima),
  #           col = "brown") +
  geom_point(color = "red") +
  # geom_abline(aes(intercept = a0, slope = a1)) +
  # facet_wrap(~grid) +
  theme(legend.position = "none")

ggplotly(p)



# gganimate ####

library(gganimate)


# transition_reveal(Day)

# p1 <- data_grid %>% 
p1 <- data_grid %>% 
  ggplot(aes(x = grid, y = stima)) +
  geom_point(data = data,
             aes(x = x, y = y),
             size = 2) +
  geom_line(color = "brown") +
  geom_point(color = "red") +
  geom_abline(aes(intercept = a0, slope = a1)) +
  labs(x = "", y = "")
  

p1 +
  transition_reveal(grid)




p2 <- data_grid %>% 
  ggplot(aes(x = grid, y = stima)) +
  geom_point(data = data,
             aes(x = x, y = y)) +
  # geom_line(data = data_grid2,
  #           aes(x = x, y = y),
  #           col = "brown") +
  geom_point(color = "red") +
  geom_abline(aes(intercept = a0, slope = a1))


p2 +
  transition_time(grid)


# plotly ####

library(plotly)

df <- data.frame(
  x = c(1,2,3,4), 
  y = c(1,2,3,4), 
  f = c(1,2,3,4)
)

p <- ggplot(df, aes(x, y)) +
  geom_point(aes(frame = f))

ggplotly(p)



library(plotly)
library(gapminder)


p <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()

ggplotly(p)



p3 <- data_grid %>% 
  ggplot(aes(x = grid, y = stima,
             intercept = a0, slope = a1,
             frame = grid)) +
  # geom_point(data = data,
  #            aes(x = x, y = y)) +
  # geom_line(data = data_grid2,
  #           aes(x = x, y = y),
  #           col = "brown") +
  geom_point(color = "red") +
  geom_abline()

ggplotly(p3)



library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

d <- txhousing %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area")) %>%
  accumulate_by(~date)

p <- d %>%
  plot_ly(
    x = ~date, 
    y = ~median,
    split = ~city,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Date",
      zeroline = F
    ),
    yaxis = list(
      title = "Median",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = T
  ) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )

p




library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>%
  accumulate_by(~ID)

p <- ggplot(df,aes(ID, AAPL.Close, frame = frame)) +
  geom_line()

p <- ggplotly(p) %>%
  layout(
    title = "AAPL: Last 30 days",
    yaxis = list(
      title = "Close",
      zeroline = F,
      tickprefix = "$"
    ),
    xaxis = list(
      title = "Day",
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )
