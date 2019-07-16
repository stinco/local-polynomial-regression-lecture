#####
# 4. Deign bias and boundary bias
# 15/07/2019
#####


# Libraries ####
library(xtable)
library(mgcv)
library(splines)
library(MASS)
library(tidyverse)
library(gridExtra)
library(sm)

# Set plot themes
theme_set(theme_bw())


# Nadaraya-Watson ####

par(mar = c(2, .5, 1, .5))
laymat = matrix(c(1:8), byrow = FALSE, nrow = 2)
laymat = laymat[c(1, 1, 2), ]
layout(laymat)

ungraf2 = function(x, y, ff, bw=1,
                   miny = min(y)-0.05*(max(y)-min(y)),
                   maxy = max(y)+0.05*(max(y)-min(y))){
  plot(x, y, ylim = c(miny, maxy), yaxs = "i", yaxt = "n",
       xlab = "", ylab = "", col = gray(0.7))
  rug(x, line = 0.5)
  
  curve(ff(x), add = TRUE, lwd = 2, n = 100, col = "black") # True regression
  a = ksmooth(x, y, bandwidth = bw)
  lines(a$x, a$y, lwd = 2, col = "darkgreen") # Nadaraya–Watson
  # rect(min(x1),-0.1,max(x1),1,lwd=2,col=hsv(184/360,.57,.98,alpha=0.2),border=NA)
  # lines(range(x1),-0.1*c(1,1),lwd=2,col="blue")
  # sig=0.25*bw/qnorm(0.75)
  # curve(miny+0.1*(maxy-miny)*dnorm(x,0.5,sig)/dnorm(0.5,0.5,sig),add=TRUE,lwd=2,col="lightblue")
  # hist(x,freq=FALSE,main="",yaxt="n",col=gray(0.7),border="white")
}

set.seed(42)
n = 200

# Computing linear

x1_unif = seq(0, 1, length = n) #sort(runif(n,0,1))
ff1 = function(x){0.5 + x}
m = ff1(x1_unif)
y1_unif = m + rnorm(n, 0, 0.025)

x = qnorm(seq(0, 1, length = (n + 2))[2:(n + 1)]) #sort(rnorm(n,0,1))
min = min(x)
max = max(x)
x1_norm = (x - min(x)) / (max(x) - min(x))
# ff1 = function(x){0.5 + x}
m = ff1(x1_norm)
y1_norm = m + rnorm(n, 0, 0.025)



# Plotting linear

miny1 = min(y1_unif, y1_norm) - 0.05*(max(y1_unif, y1_norm) - min(y1_unif, y1_norm))
maxy1 = max(y1_unif, y1_norm) + 0.05*(max(y1_unif, y1_norm) - min(y1_unif, y1_norm))

ungraf2(x = x1_unif, y = y1_unif, ff = ff1, bw = 0.5,
        miny = miny1, maxy = maxy1)
curve(dunif(x),
      yaxt = "n")

ungraf2(x = x1_norm, y = y1_norm, ff = ff1, bw = 0.5,
        miny = miny1, maxy = maxy1)
curve(dnorm(x, mean = .5, sd = 1/(max - min)),
      yaxt = "n")



# Computing quadratic

x2_unif = seq(0, 1, length = n) #sort(runif(n,0,1))
ff2 = function(x){(x - 0.5)^2}
m = ff2(x2_unif)
y2_unif = m + rnorm(n,0,0.025)

x2_norm = qnorm(seq(0, 1, length = (n + 2))[2:(n + 1)]) #sort(rnorm(n,0,1))
min = min(x)
max = max(x)
x2_norm = (x - min(x)) / (max(x) - min(x))
# ff2 = function(x){(x - 0.5)^2}
m = ff2(x2_norm)
y2_norm = m + rnorm(n, 0, 0.025)


# Plotting quadratic

miny2 = min(y2_unif, y2_norm) - 0.05*(max(y2_unif, y2_norm) - min(y2_unif, y2_norm))
maxy2 = max(y2_unif, y2_norm) + 0.05*(max(y2_unif, y2_norm) - min(y2_unif, y2_norm))

ungraf2(x = x2_unif, y = y2_unif, ff = ff2, bw = 0.4,
        miny = miny2, maxy = maxy2)
curve(dunif(x),
      yaxt = "n")

ungraf2(x = x2_norm, y = y2_norm, ff = ff2, bw = 0.4,
        miny = miny2, maxy = maxy2)
curve(dnorm(x, mean = .5, sd = 1/(max - min)),
      yaxt = "n")



# LOESS ####

ungraf3 = function(x, y, ff, bw=1,
                   miny = min(y)-0.05*(max(y)-min(y)),
                   maxy = max(y)+0.05*(max(y)-min(y))){
  plot(x, y, ylim = c(miny, maxy), yaxs = "i", yaxt = "n",
       xlab = "", ylab = "", col = gray(0.7))
  rug(x, line = 0.5)
  
  curve(ff(x), add = TRUE, lwd = 2, n = 100, col = "black") # True regression
  
  # The quartile are at +-0.25*bw
  a = ksmooth(x, y, bandwidth = bw)
  lines(a$x, a$y, lwd = 2, col = "darkgreen") # Nadaraya–Watson
  
  sig = 0.25 * bw / qnorm(0.75) # Standard deviation of the normal kernel
  a = sm.regression(x, y, h = sig, display = 'none')
  lines(a$eval.points, a$estimate, lwd = 2, col = "blue") # LOESS
  
  # rect(min(x1),-0.1,max(x1),1,lwd=2,col=hsv(184/360,.57,.98,alpha=0.2),border=NA)
  # lines(range(x1),-0.1*c(1,1),lwd=2,col="blue")
  # sig=0.25*bw/qnorm(0.75)
  # curve(miny+0.1*(maxy-miny)*dnorm(x,0.5,sig)/dnorm(0.5,0.5,sig),add=TRUE,lwd=2,col="lightblue")
  # hist(x,freq=FALSE,main="",yaxt="n",col=gray(0.7),border="white")
}


# Plotting linear

miny1 = min(y1_unif, y1_norm) - 0.05*(max(y1_unif, y1_norm) - min(y1_unif, y1_norm))
maxy1 = max(y1_unif, y1_norm) + 0.05*(max(y1_unif, y1_norm) - min(y1_unif, y1_norm))

ungraf3(x = x1_unif, y = y1_unif, ff = ff1, bw = 0.5,
        miny = miny1, maxy = maxy1)
curve(dunif(x),
      yaxt = "n")

ungraf3(x = x1_norm, y = y1_norm, ff = ff1, bw = 0.5,
        miny = miny1, maxy = maxy1)
curve(dnorm(x, mean = .5, sd = 1/(max - min)),
      yaxt = "n")


# Plotting quadratic

miny2 = min(y2_unif, y2_norm) - 0.05*(max(y2_unif, y2_norm) - min(y2_unif, y2_norm))
maxy2 = max(y2_unif, y2_norm) + 0.05*(max(y2_unif, y2_norm) - min(y2_unif, y2_norm))

ungraf3(x = x2_unif, y = y2_unif, ff = ff2, bw = 0.4,
        miny = miny2, maxy = maxy2)
curve(dunif(x),
      yaxt = "n")

ungraf3(x = x2_norm, y = y2_norm, ff = ff2, bw = 0.4,
        miny = miny2, maxy = maxy2)
curve(dnorm(x, mean = .5, sd = 1/(max - min)),
      yaxt = "n")





