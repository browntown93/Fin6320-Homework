setwd("C:\\Users\\Tyler\\USU2018\\data")
library(rugarch)

##Open data File

raw.data <- read.csv("C:\\Users\\Tyler\\USU2018\\data\\SP500.csv", header = T)

##Calculate Return
stockreturn <- diff(log(raw.data$SP500))

spec <- ugarchspec()
fit <- ugarchfit(stockreturn, spec = spec)
w <- coef(fit)[[4]]
a <- coef(fit)[[5]]
b <- coef(fit)[[6]]
c(w, a, b)

##Get Initial conditions as last return and volitility from data.


n <- length(stockreturn)
r.init <- stockreturn[n]
v.cond <- as.numeric(fitted(fit))
v.init <- v.cond[n]
c(r.init, v.init)

resid <- stockreturn / v.cond
tail(resid)


## Begin Simulation of Data

reps <- 10000 
steps <- 10
paths <- matrix(0, reps, steps + 1) 
paths[,1] <- r.init

for(i in 1:reps)
{
  sigma <- v.init
  z <- sample(resid, size = steps, replace = T)
  for(j in 2:(steps+1))
  {
    sigma <- sqrt(w + a * (paths[i,j-1] * paths[i,j-1]) + b * (sigma * sigma))
    paths[i,j] <- z[j-1] * sigma
  }
}

#Obtain quantiles of VaR.

ret.pred <- apply(paths[,2:(steps+1)], 1, sum)

alpha <- c(0.01, 0.05, 0.1)
quantile(ret.pred, probs=alpha)







