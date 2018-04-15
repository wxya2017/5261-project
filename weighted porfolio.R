setwd("~/Desktop/finalproject/data")
library(aTSA)
library(fGarch)
library(rugarch)
AMZN = read.csv("AMZN.csv")[,5]
BIDU = read.csv("BIDU.csv")[,5]
FB = read.csv("FB.csv")[,5]
GM = read.csv("GM.csv")[,5]
GOOG = read.csv("GOOG.csv")[,5]
NKE = read.csv("NKE.csv")[,5]
QIWI = read.csv("QIWI.csv")[,5]
SOHU = read.csv("SOHU.csv")[,5]
UPS = read.csv("UPS.csv")[,5]
YNDX = read.csv("YNDX.csv")[,5]
rf <- 0.0267/253

price = cbind(AMZN,BIDU,FB,GM,GOOG,NKE,QIWI,SOHU,UPS,YNDX)
# plot(AMZN, type = "l")
n = nrow(price)
return = apply(price,2,function(x) log(x[2:n]/x[1:(n-1)]))

# plot(return[,1],type = "l")
# acf(return[,1])
# pacf(return[,1])
# mod = arima(return[,1],order = c(1,0,0))
# library(aTSA)
# arch.test(mod$residual)

mu = colMeans(return)
sigma = cov(return)

########################
# short selling allowed
########################

###### portfolio
library(quadprog)
muP = seq(min(mu),max(mu),length=200)  # target portfolio return
sdP = muP # sd of portfolio return
weight = matrix(0,nrow=200,ncol=10) # storage for portfolio weights
for (i in 1:length(muP)){ # find the optimal portfolios
  result = solve.QP(Dmat=2*sigma,dvec=rep(0,10),
                    Amat=cbind(rep(1,10),mu),bvec=c(1,muP[i]),meq=2)
  sdP[i] = sqrt(result$value)
  weight[i,] = result$solution
}

library(ggplot2)
ind1 = (sdP == min(sdP))
ind2 = (muP > muP[ind1])
rf; # riskfree rate   
sharpe =(muP-rf)/sdP # Sharpe's ratios
ind3 = (sharpe == max(sharpe)) # find maximum Sharpe's ratio


ggplot(data = data.frame(sdP,muP)) +
  geom_path(aes(x = sdP,y = muP),linetype = 3,color = "red",size = 0.8)+
  geom_path(aes(x = sdP,y = muP),
            data = data.frame(sdP = sdP[ind2],muP = muP[ind2]),
            size = 1.2,color = "red")+
  geom_abline(slope = sharpe[ind3],intercept = rf,color = "blue",size = 1.2)+
  geom_point(aes(x = sdP[ind1],y = muP[ind1]),shape = "x",size = 5)+
  geom_text(x = sdP[ind1]+0.001, y = muP[ind1], label = "MVP")+
  geom_point(aes(x = 0, y = rf),shape = "x",size = 5)+
  geom_text(x = 0.001, y = rf, label = "RF")+
  geom_point(aes(x = sdP[ind3], y = muP[ind3]),shape = "x",size = 5,color = "black")+
  geom_text(x = sdP[ind3]-0.001, y = muP[ind3], label = "RM")

short_weight = weight[which.max(sharpe),];short_weight
short_por_return = as.matrix(return) %*% as.matrix(short_weight)

####### time series model
plot(short_por_return,type = "l")
library(forecast)
fit = auto.arima(short_por_return,max.p=3,max.q=3,ic="aic",
                 start.p = 2,
                 seasonal=FALSE,stepwise=FALSE,trace=TRUE, 
                 approximation=FALSE,allowdrift=FALSE,allowmean = TRUE)
fit
acf(short_por_return)
pacf(short_por_return)
mod <- arima(short_por_return,order = c(0,0,0))
arch.test(mod)


mean(short_por_return)
garchm = ugarchspec(mean.model=list(armaOrder=c(0,0),archpow=1), 
                    variance.model=list(garchOrder=c(1,1)))
GPRO.garchm = ugarchfit(garchm,data = short_por_return,solver = "solnp")
show(GPRO.garchm)
forcast = ugarchforecast(GPRO.garchm,n.ahead = 7,roll)
plot(forcast)

########################
# without short selling
########################
muP = seq(min(mu)+0.00001,max(mu)-0.00001,length=200) # target portfolio return
for (i in 1:length(muP)){ # find the optimal portfolios
  result = solve.QP(Dmat=2*sigma,dvec=rep(0,10),
                    Amat=cbind(rep(1,10),mu,diag(1,10)),
                    bvec=c(1,muP[i],rep(0.00000000000001,10)),meq=2)
  sdP[i] = sqrt(result$value)
  weight[i,] = result$solution
}

ind1 = (sdP == min(sdP))
ind2 = (muP > muP[ind1])
sharpe =(muP-rf)/sdP # Sharpe's ratios
ind3 = (sharpe == max(sharpe)) # find maximum Sharpe's ratio

ggplot(data = data.frame(sdP,muP)) +
  geom_path(aes(x = sdP,y = muP),linetype = 3,color = "red",size = 0.8)+
  geom_path(aes(x = sdP,y = muP),
            data = data.frame(sdP = sdP[ind2],muP = muP[ind2]),
            size = 1.2,color = "red")+
  geom_abline(slope = sharpe[ind3],intercept = rf,color = "blue",size = 1.2)+
  geom_point(aes(x = sdP[ind1],y = muP[ind1]),shape = "x",size = 5)+
  geom_text(x = sdP[ind1]+0.001, y = muP[ind1], label = "MVP")+
  geom_point(aes(x = 0, y = rf),shape = "x",size = 5)+
  geom_text(x = 0.001, y = rf, label = "RF")+
  geom_point(aes(x = sdP[ind3], y = muP[ind3]),shape = "x",size = 5,color = "black")+
  geom_text(x = sdP[ind3]-0.001, y = muP[ind3], label = "RM")

no_short_weight = weight[which.max(sharpe),];no_short_weight
no_short_por_return = as.matrix(return) %*% as.matrix(no_short_weight)

plot(no_short_por_return,type = "l")
fit = auto.arima(no_short_por_return,max.p=3,max.q=3,ic="aic",
                 seasonal=FALSE,stepwise=FALSE,trace=TRUE, 
                 approximation=FALSE,allowdrift=FALSE,allowmean = TRUE)
fit
acf(short_por_return)
pacf(short_por_return)
mod <- arima(short_por_return,order = c(0,0,0))
arch.test(mod)

garchm = ugarchspec(mean.model=list(armaOrder=c(0,0),archpow=1), 
                    variance.model=list(garchOrder=c(1,0)))
GPRO.garchm = ugarchfit(garchm,data = no_short_por_return,solver = "solnp")
show(GPRO.garchm)
plot(GPRO.garchm)

ugarchforecast(GPRO.garchm,n.ahead = 7)

