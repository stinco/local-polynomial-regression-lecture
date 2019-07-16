#####
# 5. What neighborhood means
# 16/07/2019
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


# neighborhood of radius h ####

runmean=function(x,xx,yy,h){
  mean(yy[abs(xx-x)<h])
}
runmean2=function(x,xx,yy,h){  
  mapply(runmean,x,MoreArgs=list(xx=xx,yy=yy,h=h))
}

data_legend <- tibble(x = rep(mean(lidar$range), 4),
                      y = rep(mean(lidar$logratio),4),
                      h = factor(str_c("h = ", c(10,10,50,50)),
                                 levels = str_c("h = ", c(10,50))))

ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point() +
  stat_function(fun = function(x){runmean2(x,lidar$range,lidar$logratio,h=10)},
                size = 1, col = "red") +
  stat_function(fun = function(x){runmean2(x,lidar$range,lidar$logratio,h=50)},
                size = 1, col = "darkgreen") +
  geom_line(data = data_legend,
            aes(x = x, y = y, color = h), size = 1) +
  scale_color_manual(values = c("red", "darkgreen")) +
  labs(title = "Lidar dataset",
       subtitle = "Neighborhood of radius h")

  


# knn ####

knn=function(x,xx,yy,k){
  mean(yy[abs(xx-x)<sort(abs(xx-x))[k]])
}
knn2=function(x,xx,yy,k){  
  mapply(knn,x,MoreArgs=list(xx=xx,yy=yy,k=k))
}

data_legend <- tibble(x = rep(mean(lidar$range), 4),
                      y = rep(mean(lidar$logratio),4),
                      k = factor(str_c("k = ", c(5,5,30,30)),
                                 levels = str_c("k = ", c(5,30))))

ggplot(data = lidar,
       aes(x = range, y = logratio)) +
  geom_point() +
  stat_function(fun = function(x){knn2(x,lidar$range,lidar$logratio,k=5)},
                size = 1, col = "red") +
  stat_function(fun = function(x){knn2(x,lidar$range,lidar$logratio,k=30)},
                size = 1, col = "darkgreen") +
  geom_line(data = data_legend,
            aes(x = x, y = y, color = k), size = 1) +
  scale_color_manual(values = c("red", "darkgreen")) +
  labs(title = "Lidar dataset",
       subtitle = "k nearest neighbors")





# What neighborhood mean ####

set.seed(42)

n = 100
h = .1

x = sort(runif(n, 0, 1))
x0 = 0.5
ff = function(x){sqrt(3*x + .5)}
m = ff(x)
y = m + rnorm(n,0,0.025)

xlim = c(0,1)
# ylim = c(min(y) - .05 * (max(y) - min(y)),
#          max(y) + .05 * (max(y) - min(y)))
ylim = c(.6, 2)

plot(x, y, xlim = xlim, ylim = ylim, yaxt = "n", xaxt = "n",
     xlab = "", ylab = "", yaxs = "i")

axis(1, at = c(x0-h, x0+h), labels = c("", ""))
axis(1, at = x0, labels = expression(x[0]))

rect(x0 - h, ylim[1], x0 + h, ylim[2],
     lwd=2, col = hsv(184/360, .57, .98, alpha = 0.2), border=NA)


curve(ff(x), add = TRUE, lwd = 1,
      xlim = c(-.1, 1.1))

ind <- abs(x-x0) < h
points(x[ind], y[ind], col = "blue", pch = 20)

segments(x0, -1, x0, mean(y[ind]), lwd = .5, lty = 2)
points(x0, mean(y[ind]), col = "red", pch = 20, cex = 3)

lines(c(x0-h, x0+h), ylim[c(1,1)],
      lwd = 2, col = "blue")


# Design bias

set.seed(42)

n = 50
h = .1

# x = sort(runif(n, 0, 1))
mean = .6
sd = .04
x = sort(rnorm(n, mean, sd))
x0 = 0.5
ff = function(x){sqrt(3*x + .5)}
m = ff(x)
y = m + rnorm(n,0,0.025)

xlim = c(0,1)
# ylim = c(min(y) - .05 * (max(y) - min(y)),
#          max(y) + .05 * (max(y) - min(y)))
ylim = c(.6, 2)

plot(x, y, xlim = xlim, ylim = ylim, yaxt = "n", xaxt = "n",
     xlab = "", ylab = "", yaxs = "i")

axis(1, at = c(x0-h, x0+h), labels = c("", ""))
axis(1, at = x0, labels = expression(x[0]))

rect(x0 - h, ylim[1], x0 + h, ylim[2],
     lwd=2, col = hsv(184/360, .57, .98, alpha = 0.2), border=NA)

curve(ff(x), add = TRUE, lwd = 1,
      xlim = c(-.1, 1.1))

ind <- abs(x-x0) < h
points(x[ind], y[ind], col = "blue", pch = 20)

segments(x0, -1, x0, mean(y[ind]), lwd = .5, lty = 2)
points(x0, mean(y[ind]), col = "red", pch = 20, cex = 3)


curve(ylim[1] + 0.2 * (ylim[2] - ylim[1]) * dnorm(x, mean, sd) / dnorm(0,0,sd),
      add = TRUE, lwd = 2, col = "lightblue")

lines(c(x0-h, x0+h), ylim[c(1,1)],
      lwd = 2, col = "blue")




# Pauli ####
par(mar=c(2,1,1,0))
layout(matrix(c(1:8),byrow=FALSE,nrow=2))
n=50
k1=10
k2=40
x=sort(runif(n,0,1))
x0=0.5
ind1=sort.list(abs(x-x0),decreasing = FALSE)[1:k1]
ind2=sort.list(abs(x-x0),decreasing = FALSE)[1:k2]
x1=x[sort.list(abs(x-x0),decreasing = FALSE)[1:k1]]
x2=x[sort.list(abs(x-x0),decreasing = FALSE)[1:k2]]

ff=function(x) 0.5+0*x
m=ff(x)
y=m+rnorm(n,0,0.025)
ungraf=function(){
  plot(x,y,ylim=c(-0.1,1),yaxt="n",xaxt="n",xlab="",ylab="",yaxs="i")
  axis(1,at=range(x1),labels=c("",""))
  axis(1,at=x0,labels=expression(x[0]))
  rect(min(x1),-0.1,max(x1),1,lwd=2,col=hsv(184/360,.57,.98,alpha=0.2),border=NA)
  lines(range(x1),-0.1*c(1,1),lwd=2,col="blue")
  curve(ff(x),add=TRUE,lwd=1,n=100)
  points(x1,y[ind1],col="blue",pch=20)
  points(x0,mean(y[ind1]),col="red",pch=4,cex=2)
  
  plot(x,y,ylim=c(-0.1,1),yaxt="n",xaxt="n",xlab="",ylab="",yaxs="i")
  axis(1,at=range(x2),labels=c("",""))
  axis(1,at=x0,labels=expression(x[0]))
  lines(range(x2),-0.1*c(1,1),lwd=2,col="blue")
  rect(min(x2),-0.1,max(x2),1,lwd=2,col=hsv(184/360,.57,.98,alpha=0.2),border=NA)
  curve(ff(x),add=TRUE,lwd=1,n=100)
  points(x2,y[ind2],col="blue",pch=20)
  points(x0,mean(y[ind2]),col="red",pch=4,cex=2)
}
ungraf()

ff=function(x) x
m=ff(x)
y=m+rnorm(n,0,0.025)
ungraf()

ff=function(x) 1-cos(pi*x^2/2)
m=ff(x)
y=m+rnorm(n,0,0.025)
ungraf()

ff=function(x) ((x-x0)*3)^2/2
m=ff(x)
y=m+rnorm(n,0,0.025)
ungraf()
















