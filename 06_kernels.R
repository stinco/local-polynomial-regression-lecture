#####
# 6. kernels
# 17/07/2019
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


# Kernels ####


# par(mfrow = c(2, 4),
#     mar = c(2,2,1,1))


# par(mar = c(2,2,1,1)))
ylim = c(0, 1.2)

plot_kernel = function(ff){
  plot(0, 0, type = "n",
       xlim = c(-2,2), ylim = ylim)
  segments(-3, 0, 3, 0, lwd = .5, lty = 2)
  curve(ff(x), add = T, lwd = 2, col = "red")
}


# Uniform
par(mar = c(2,2,1,1))
plot(0, 0, type = "n",
     xlim = c(-2,2), ylim = ylim)
segments(-3, 0, 3, 0, lwd = .5, lty = 2)
segments(-1, 1/2, 1, 1/2, lwd = 2, col = "red")
segments(-3, 0, -1, 0, lwd = 2, col = "red")
segments(1, 0, 3, 0, lwd = 2, col = "red")


# Triangle
ff = function(x){(1-abs(x)) * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)


# Triweight
ff = function(x){35/32*(1-x^2)^3 * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)


# Quartic
ff = function(x){15/16*(1-x^2)^2 * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)


# Cosine
ff = function(x){pi/4*cos(pi/2*x) * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)


# Epanechnikov
ff = function(x){3/4*(1-x^2) * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)


# Gaussian
par(mar = c(2,2,1,1))
plot_kernel(dnorm)





# Create images for kernels ####

<<uniform, fig.height = 2.5, fig.width = 2.5>>=
  par(mar = c(2,2,1,1))
plot(0, 0, type = "n",
     xlim = c(-2,2), ylim = ylim)
segments(-3, 0, 3, 0, lwd = .5, lty = 2)
segments(-1, 1/2, 1, 1/2, lwd = 2, col = "red")
segments(-3, 0, -1, 0, lwd = 2, col = "red")
segments(1, 0, 3, 0, lwd = 2, col = "red")
@

<<triangle, fig.height = 2.5, fig.width = 2.5>>=
  ff = function(x){(1-abs(x)) * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)
@
  
  <<triweight, fig.height = 2.5, fig.width = 2.5>>=
  ff = function(x){35/32*(1-x^2)^3 * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)
@
  
  <<quartic, fig.height = 2.5, fig.width = 2.5>>=
  ff = function(x){15/16*(1-x^2)^2 * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)
@
  
  <<cosine, fig.height = 2.5, fig.width = 2.5>>=
  ff = function(x){pi/4*cos(pi/2*x) * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)
@
  
  <<epanechnikov, fig.height = 2.5, fig.width = 2.5>>=
  ff = function(x){3/4*(1-x^2) * (x>=-1 & x<=1)}
par(mar = c(2,2,1,1))
plot_kernel(ff)
@
  
  <<gaussian, fig.height = 2.5, fig.width = 2.5>>=
  par(mar = c(2,2,1,1))
plot_kernel(dnorm)
@













