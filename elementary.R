# Clear environment
rm(list=ls())
library(fNonlinear)
library(crqa)

# set parameters for RPA plots (same for all days)
delay = 1
embed = 1
rescale = 1
radius = 1
normalize = 0
mindiagline = 2
minvertline = 2
tw = 0
whiteline = FALSE
recpt = FALSE
side = "both"
checkl = list(do = FALSE, thrshd = 3, datatype = "categorical", pad = TRUE)
par = list(unit = 2, labelx = "Time", labely = "Time", cols = "red", pcex = 1)



# Sin function
x <- seq(-500, 500, by = 1/1)
y =sin(x)
plot(x,y)
ts_y <- as.ts(y)
rpa_y = crqa(ts_y, ts_y, delay, embed, rescale, radius, normalize, mindiagline,minvertline, tw, whiteline, recpt, side, checkl)

# create tec plot as image in working dir
png("sin.png",width=3.25,height=3.25,units="in",res=1000,pointsize=3)
RP = rpa_y$RP
plotRP(RP, par)
dev.off()



# Normal distribution - white noise
y =rnorm(1000)
plot(y)
ts_y <- as.ts(y)
rpa_y = crqa(ts_y, ts_y, delay, embed, rescale, radius, normalize, mindiagline,minvertline, tw, whiteline, recpt, side, checkl)

# create tec plot as image in working dir
png("normal.png",width=3.25,height=3.25,units="in",res=1000,pointsize=3)
RP = rpa_y$RP
plotRP(RP, par)
dev.off()




# Auto-regressive time sequence
alpha = -0.99
# purely random process
Z <- rnorm(1000, mean = 0, sd = 0.5)
# seed
X <- rnorm(1)
# the process
for (i in 2:length(Z)) {
  X[i] <- alpha*X[i-1]+Z[i]
}
ts.plot(X)

ts_y <- as.ts(X)
rpa_y = crqa(ts_y, ts_y, delay, embed, rescale, radius, normalize, mindiagline,minvertline, tw, whiteline, recpt, side, checkl)

# create tec plot as image in working dir
png("ar1.png",width=3.25,height=3.25,units="in",res=1000,pointsize=3)
RP = rpa_y$RP
plotRP(RP, par)
dev.off()


# Brownian motion
t <- 0:1000  # time
sig2 <- 0.01
## first, simulate a set of random deviates
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
x <- c(0, cumsum(x))
plot(t, x, type = "l", ylim = c(-2, 2))

ts_y <- as.ts(x)
rpa_y = crqa(ts_y, ts_y, delay, embed, rescale, radius, normalize, mindiagline,minvertline, tw, whiteline, recpt, side, checkl)

# create tec plot as image in working dir
png("brownian.png",width=3.25,height=3.25,units="in",res=1000,pointsize=3)
RP = rpa_y$RP
plotRP(RP, par)
dev.off()



# Logistic map
logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}

library(compiler) ## requires R >= 2.13.0
logistic.map <- cmpfun(logistic.map) # same function as above
lm<-logistic.map(3.56995,0.01,1000,400)
ts_y <- as.ts(lm)
rpa_y = crqa(ts_y, ts_y, delay, embed, rescale, radius, normalize, mindiagline,minvertline, tw, whiteline, recpt, side, checkl)

# create tec plot as image in working dir
png("logistic.png",width=3.25,height=3.25,units="in",res=1000,pointsize=3)
RP = rpa_y$RP
plotRP(RP, par)
dev.off()

