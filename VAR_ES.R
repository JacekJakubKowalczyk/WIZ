rm(list=ls())
setwd("~/Desktop/Praca licencjacka")
library("readxl")
library("zoo")
library("dplyr")
library("rugarch")
library("stargazer")
#Wczytaj dfy
df_index=read.csv("w20index2018.csv", stringsAsFactors = FALSE)
df_VIX_Value=read.csv("VIX_Value.csv",stringsAsFactors = FALSE)
df_VIX_Interest=read.csv("VIX_Interest.csv",stringsAsFactors = FALSE)
#Zamien do zoo pare szeregów
zoo_VIX_Value=zoo(df_VIX_Value$v, as.Date(df_VIX_Value$d))
zoo_VIX_Interest=zoo(df_VIX_Interest$v, as.Date(df_VIX_Interest$d))
zoo_returns=zoo(df_index$Change/100,as.Date(df_index$Date))
zoo_returns=zoo_returns[2:length(zoo_returns)]
zoo_returns_log=zoo(diff(log(df_index$Current)),as.Date(df_index$Date)[2:length(df_index$Date)])
R=coredata(zoo_returns)


#pare momentów
M1   = moment(R, order = 1, central = FALSE, na.rm = TRUE)
M2   = moment(R, order = 2, central = TRUE, na.rm = TRUE)
M3   = moment(R, order = 3, central = TRUE, na.rm = TRUE)
M4   = moment(R, order = 4, central = TRUE, na.rm = TRUE)
sig <- sqrt(M2)       
S   <- M3/(sig^3)     
K   <- M4/(sig^4)     
S0   = M3/(sig^3)
K0   = M4/(sig^4)

# Annualizacja
Nyear <- 250
muA   <- M1*Nyear
sigA  <- sig*sqrt(Nyear)

#VAR Historyczny dla próbki
N=250
p=0.01
VaR_HS=numeric()
ES_HS=numeric()
for (i in N:length(R)){
  R_i  <- R[(i-N):i]
  R0       <- sort(R_i)              
  N0       <- floor(N*p)                                  
  VaR_HS[i-N+1]  = R0[N0]
  ES_HS[i-N+1]  <- mean(R0[1:N0]) 
}
VaR_HS=zoo(VaR_HS, as.Date(df_VIX_Value$d[251:781]))
sigma_VIX_Value=zoo_VIX_Value/(sqrt(365))/100
plot(merge(zoo_returns,2*sigma_VIX_Value,-2*sigma_VIX_Value),facets=NULL,plot.type="single",main="Zwroty vs 2*zmienność z VIX")
##########################
#VaR normalnie
spec.gjrGARCH = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model="sstd")
gjrGARCH <- ugarchfit(R, spec=spec.gjrGARCH)
sigma_garch=zoo(coredata(sigma(gjrGARCH)),as.Date(df_index$Date))
q    <- qdist("sstd", p=p, shape=4)
plot(sigma_garch)
VaR_GARCH=sigma_garch*q
plot(VaR_GARCH, main ="VaR Garch")
summary(VaR_GARCH)


####################
#Var z dodatkiem VIX
reg=as.matrix(coredata(zoo_VIX_Value/(sqrt(365))))
spec.gjrGARCH.VIX=ugarchspec(variance.model=list(model="gjrGARCH", submodel=NULL,garchOrder=c(1,1),external.regressors=reg, variance.targeting = FALSE), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model="sstd")
gjrGARCH.VIX <- ugarchfit(R, spec=spec.gjrGARCH.VIX)
sigma_garch_VIX=zoo(coredata(sigma(gjrGARCH.VIX)),as.Date(df_index$Date))
VaR_GARCH_VIX=sigma_garch_VIX*q
plot(VaR_GARCH_VIX, main="GJR GARCH VIX")
plot(merge(VaR_GARCH_VIX,VaR_GARCH), main="VaR GJR Garch VIX vs VaR Garch")
summary(merge(VaR_GARCH_VIX,VaR_GARCH))


#########
#lagged volatility VaR
zoo_lag_VIX_Value=lag(coredata(zoo_VIX_Value/(100*sqrt(365))),1)
model=lm(coredata(zoo_VIX_Value/(100*sqrt(365)))~zoo_lag_VIX_Value)
plot(model$fitted.values, type="l",main="Sigma-fit from LR")
VaR_lagged_implied=model$fitted.values*q
VaR_lagged_implied=zoo(VaR_lagged_implied, as.Date(df_VIX_Value$d))
plot(VaR_lagged_implied,main="VaR lagged implied volatility vs VaR GJRGarch VIX")
lines(VaR_GARCH_VIX, col="red")

#Testy trzy łącznie

require(forecast)
f_tests=function(p,backN,returns,var){
  rr <- returns[index(var)]
  eta <- tail(rr<=var, backN)
  # b. Kupiec test
  n <- length(eta)
  n1 <- sum(eta)
  n0 <- n - n1
  pi <- n1/n
  pi
  #LR_uc   = ( p^n1*(1-p)^n0 ) / ( pi^n1*(1-pi)^n0 )
  LR_uc   = (p/pi)^n1 *((1-p)/(1-pi))^n0
  stat_uc = -2*log(LR_uc)
  prob_uc = 1 - pchisq(q=stat_uc,df=1, lower.tail=TRUE)
  #Christopherson
  eta1 <- coredata(eta[-length(eta)])
  eta0 <- coredata(eta[-1])
  n00 = sum(!eta1 & !eta0) # no exceedance after no exceedance
  n01 = sum(!eta1 &  eta0)  # exceedance after no exceedance
  n10 = sum( eta1 & !eta0)  # no exceedance after exceedance
  n11 = sum( eta1 &  eta0)   # exceedance after exceedance
  
  n0  = n00 + n10
  n1  = n01 + n11
  
  pi01 = n01 / (n00+n01) # prawdopodobienstwo przekroczenia po braku przekroczenia
  pi11 = n11 / (n10+n11) # prawdopodobienstwo przekroczenia po przekroczeniu
  pi   = (n01+n11) / (n00+n01+n10+n11)
  
  LR_ind   = pi^n1*(1-pi)^n0 / (pi01^n01 * pi11^n11 * (1-pi01)^n00  * (1-pi11)^n10)
  # LR_ind   = (pi/pi01)^n01 * (pi/pi11)^n11 * ((1-pi)/(1-pi01))^n00 * ((1-pi)/(1-pi11))^n10
  # LR_ind     = ( p^n1*(1-p)^n0 ) / ( pi^n1*(1-pi)^n0 )
  stat_ind = -2*log(LR_ind)
  prob_ind = 1 - pchisq(stat_ind,1)
  # Christofersen conditional coverage test
  eta1 <- coredata(eta[-length(eta)])
  eta0 <- coredata(eta[-1])
  # LR_cc   = p^n0*(1-p)^n1 / (pi01^n01 * pi11^n11 * (1-pi01)^n00  * (1-pi11)^n10)
  LR_cc   = (p/pi01)^n01 * (p/pi11)^n11 * ((1-p)/(1-pi01))^n00 * ((1-p)/(1-pi11))^n10
  stat_cc = -2*log(LR_cc)
  prob_cc = 1 - pchisq(stat_cc,1)
  return(c(prob_uc, prob_ind, prob_cc))
}
#p = 0.01, cały przedział
pval_VaRVIX=f_tests(0.01,500,zoo_returns,VaR_GARCH_VIX)
pval_VaRGarch=f_tests(0.01,500,zoo_returns,VaR_GARCH)
pval_VaRHS=f_tests(0.01,250,zoo_returns,VaR_HS)
pval_VaRLag=f_tests(0.01,500,zoo_returns,VaR_lagged_implied)

pvals=rbind(pval_VaRVIX,pval_VaRGarch,pval_VaRHS,pval_VaRLag)
colnames(pvals)=c("Kupiec", "Christofersen independence","Christofersen conditional coverage")
stargazer(pvals)

# p =0.025 wysoka zmienność
pval_VaRVIX2=f_tests(0.025,250,zoo_returns[482:733],VaR_GARCH_VIX[483:734])
pval_VaRGarch2=f_tests(0.025,250,zoo_returns[482:733],VaR_GARCH[483:734])
pval_VaRHS2=f_tests(0.025,250,zoo_returns[482:733],VaR_HS[233:484])
pval_VaRLag2=f_tests(0.025,250,zoo_returns[482:733],VaR_lagged_implied[483:734])

pvals2=rbind(pval_VaRVIX2,pval_VaRGarch2,pval_VaRHS2,pval_VaRLag2)
colnames(pvals2)=c("Kupiec", "Christofersen independence","Christofersen conditional coverage")
stargazer(pvals2)
# p =0.025 niska zmienność
pval_VaRVIX3=f_tests(0.025,250,zoo_returns[1:480],VaR_GARCH_VIX[2:481])
pval_VaRGarch3=f_tests(0.025,250,zoo_returns[1:480],VaR_GARCH[2:481])
pval_VaRHS3=f_tests(0.025,250,zoo_returns[1:480],VaR_HS[2:481])
pval_VaRLag3=f_tests(0.025,250,zoo_returns[1:480],VaR_lagged_implied[2:481])

pvals3=rbind(pval_VaRVIX3,pval_VaRGarch3,pval_VaRHS3,pval_VaRLag3)
colnames(pvals3)=c("Kupiec", "Christofersen independence","Christofersen conditional coverage")
stargazer(pvals3)
# p =0.025 średnia zmienność
pval_VaRVIX4=f_tests(0.025,250,zoo_returns[500:780],VaR_GARCH_VIX[501:781])
pval_VaRGarch4=f_tests(0.025,250,zoo_returns[500:780],VaR_GARCH[501:781])
pval_VaRHS4=f_tests(0.025,250,zoo_returns[500:780],VaR_HS[251:531])
pval_VaRLag4=f_tests(0.025,250,zoo_returns[500:780],VaR_lagged_implied[501:781])
pvals4=rbind(pval_VaRVIX4,pval_VaRGarch4,pval_VaRHS4,pval_VaRLag4)
colnames(pvals4)=c("Kupiec", "Christofersen independence","Christofersen conditional coverage")
stargazer(pvals4)
#Wykres dla czterech VaRów

plot(zoo_returns, xlab="",ylab="Log-returns")
lines(VaR_GARCH, col="red")
lines(VaR_GARCH_VIX, col="green")
lines(VaR_lagged_implied, col="blue")
legend("bottomleft",c("Log Returns","VaR gjrGARCH", "VaR gjrGarch w. WIZ","VAR Lagged WIZ"),lty=c(1,1,1,1), col =c("black","red","green","blue"))
#VARPLOT
VaRplot(0.01,zoo_returns, VaR_GARCH[2:781], title="XD")
VaRplot(0.025, zoo_returns, VaR_GARCH_VIX[2:781])
VaRplot(0.025, zoo_returns, VaR_lagged_implied[2:781])

# #Zmienność zrealizowana vs VIX
# f_realised=function(x){
#   sigma_realised=numeric()
#   for (i in 1:length(x)){
#     sigma_realised[i]=var(x[i:(i+30)])
#   }
#   return(sigma_realised)
# }
# sigma_realised=f_realised(R)*10000
# vix_realised=df_VIX_Interest$v/12
# plot(sigma_realised,type="l",main="Sigma Implied vs Sigma realised")
# lines(vix_realised, col="red")
# cor.test(head(o,750),head(vix_realised,750))
# 
# plot(sqrt(sigma_realised),type="l")
# lines(vix_realised,col="red")

#Fitdistr - rugarch 
#Backtesting - 3 okresy 
#Szereg czasowy - porównanie - literatura

