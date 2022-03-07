#Notatki
rm(list=ls())
setwd("~/Desktop/Praca licencjacka")
library("readxl")
library("zoo")
library("dplyr")
library(readr)
library(ggplot2)
require(moments)
library(stargazer)
df_index=read.csv("w20index2018.csv", stringsAsFactors = FALSE)
df_VIX_Value=read.csv("VIX_Value.csv",stringsAsFactors = FALSE)
df_VIX_Interest=read.csv("VIX_Interest.csv",stringsAsFactors = FALSE)
df_VIX_us = read.csv("VIX_US.csv", stringsAsFactors = FALSE)
zoo_VIX_Value=zoo(df_VIX_Value$v, as.Date(df_VIX_Value$d))
zoo_VIX_Interest=zoo(df_VIX_Interest$v, as.Date(df_VIX_Interest$d))
zoo_VIX_US=zoo(as.numeric(df_VIX_us$Open),as.Date(df_VIX_us$Date))
zoo_VIX_US=zoo_VIX_US[2:804]
zoo_returns=zoo(df_index$Change/100,as.Date(df_index$Date))
zoo_returns=zoo_returns[2:length(zoo_returns)]
zoo_returns_log=zoo(diff(log(df_index$Current)),as.Date(df_index$Date)[2:length(df_index$Date)])
R=coredata(zoo_returns)

#VIW Rudzki
wiv20 <- read_delim("wiv20.csv", ";", escape_double = FALSE,col_types = cols(WIV20 = col_number()), trim_ws = TRUE)
plot(wiv20$DATE[3579:4374],wiv20$WIV20[3579:4374]/100, type = "l",xlab="Date",ylab="VIW20", main="WIV 20")


#Lognormal dist
x <- seq(0, 10, length=1000)
hx <- dlnorm(x)
plot(y=hx,x=seq(0,10,length=1000), type="l",main="", xlab="x",ylab="y")

#ploty dla trzech VIXÓw
zoo_VIX_Interest=zoo_VIX_Interest[-534]
zoo_VIX_Value=zoo_VIX_Value[-534]
zoo_VIX_Value=zoo_VIX_Value[-532]
plot(zoo_VIX_US, type="l",main="", xlab="Value",ylab="Date", col="red")
lines(zoo_VIX_Value, col="green")
lines(zoo_VIX_Interest, col="blue")
legend("topleft",c("VIX", "WIZ Value","WIZ Interest"),lty=c(1,1,1), col =c("red","green","blue"))
#tabelka opisowa

desc_title=c("n","Mean", "Median","Variance","StdDev", "Minimum","Maximum","Kurtosis", "Skewness", "P1", "P5","P10","P90","P95","P99")
desc_VIX_Value=c(length(zoo_VIX_Value), mean(zoo_VIX_Value),median(zoo_VIX_Value),var(zoo_VIX_Value),sqrt(var(zoo_VIX_Value)),min(zoo_VIX_Value),max(zoo_VIX_Value),kurtosis(zoo_VIX_Value),skewness(zoo_VIX_Value),quantile(zoo_VIX_Value,probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
names(desc_VIX_Value)=desc_title
desc_VIX_Interest=c(length(zoo_VIX_Value), mean(zoo_VIX_Interest),median(zoo_VIX_Interest),var(zoo_VIX_Interest),sqrt(var(zoo_VIX_Interest)),min(zoo_VIX_Interest),max(zoo_VIX_Interest),kurtosis(zoo_VIX_Interest),skewness(zoo_VIX_Value),quantile(zoo_VIX_Interest,probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
names(desc_VIX_Interest)=desc_title
desc_VIX_US=c(length(zoo_VIX_US), mean(zoo_VIX_US),median(zoo_VIX_US),var(zoo_VIX_US),sqrt(var(zoo_VIX_US)),min(zoo_VIX_US),max(zoo_VIX_US),kurtosis(zoo_VIX_US),skewness(zoo_VIX_US),quantile(zoo_VIX_US,probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
table_desc2=cbind(desc_VIX_Interest,desc_VIX_Value,desc_VIX_US)
stargazer(table_desc)
options(digits=7)
options(scipen=0)


#tabelka trzy okresy
desc_title=c("n","Mean", "Median","Variance","StdDev", "Minimum","Maximum","Kurtosis", "Skewness", "P1", "P5","P10","P90","P95","P99")
desc_VIX_Value1=c(length(zoo_VIX_Value[1:483]), mean(zoo_VIX_Value[1:483]),median(zoo_VIX_Value[1:483]),var(zoo_VIX_Value[1:483]),sqrt(var(zoo_VIX_Value[1:483])),min(zoo_VIX_Value[1:483]),max(zoo_VIX_Value[1:483]),kurtosis(zoo_VIX_Value[1:483]),skewness(zoo_VIX_Value[1:483]),quantile(zoo_VIX_Value[1:483],probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
desc_VIX_Value2=c(length(zoo_VIX_Value[484:606]), mean(zoo_VIX_Value[484:606]),median(zoo_VIX_Value[484:606]),var(zoo_VIX_Value[484:606]),sqrt(var(zoo_VIX_Value[484:606])),min(zoo_VIX_Value[484:606]),max(zoo_VIX_Value[484:606]),kurtosis(zoo_VIX_Value[484:606]),skewness(zoo_VIX_Value[484:606]),quantile(zoo_VIX_Value[484:606],probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
desc_VIX_Value3=c(length(zoo_VIX_Value[606:781]), mean(zoo_VIX_Value[606:781]),median(zoo_VIX_Value[606:781]),var(zoo_VIX_Value[606:781]),sqrt(var(zoo_VIX_Value[606:781])),min(zoo_VIX_Value[606:781]),max(zoo_VIX_Value[606:781]),kurtosis(zoo_VIX_Value[606:781]),skewness(zoo_VIX_Value[606:781]),quantile(zoo_VIX_Value[606:781],probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
desc_VIX_Interest1=c(length(zoo_VIX_Interest[1:483]), mean(zoo_VIX_Interest[1:483]),median(zoo_VIX_Interest[1:483]),var(zoo_VIX_Interest[1:483]),sqrt(var(zoo_VIX_Interest[1:483])),min(zoo_VIX_Interest[1:483]),max(zoo_VIX_Interest[1:483]),kurtosis(zoo_VIX_Interest[1:483]),skewness(zoo_VIX_Interest[1:483]),quantile(zoo_VIX_Interest[1:483],probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
desc_VIX_Interest2=c(length(zoo_VIX_Interest[484:606]), mean(zoo_VIX_Interest[484:606]),median(zoo_VIX_Interest[484:606]),var(zoo_VIX_Interest[484:606]),sqrt(var(zoo_VIX_Interest[484:606])),min(zoo_VIX_Interest[484:606]),max(zoo_VIX_Interest[484:606]),kurtosis(zoo_VIX_Interest[484:606]),skewness(zoo_VIX_Interest[484:606]),quantile(zoo_VIX_Interest[484:606],probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
desc_VIX_Interest3=c(length(zoo_VIX_Interest[606:781]), mean(zoo_VIX_Interest[606:781]),median(zoo_VIX_Interest[606:781]),var(zoo_VIX_Interest[606:781]),sqrt(var(zoo_VIX_Interest[606:781])),min(zoo_VIX_Interest[606:781]),max(zoo_VIX_Interest[606:781]),kurtosis(zoo_VIX_Interest[606:781]),skewness(zoo_VIX_Interest[606:781]),quantile(zoo_VIX_Interest[606:781],probs=c(0.01,0.05,0.1,0.9,0.95,0.99),names=FALSE))
table_desc2=cbind(desc_VIX_Value1,desc_VIX_Interest1,desc_VIX_Value2,desc_VIX_Interest2,desc_VIX_Value3,desc_VIX_Interest3)
rownames(table_desc2)=desc_title
stargazer(table_desc2,column.labels=c("xd1","XD2","XD3"),column.separate = c(2,2,2))

#panel wykresów
VIX_Dates4$v[532]=40
par(mfrow=c(2,1),mar = c(2,2,1,1.2))
plot(VIX_Dates$v[400:781]~as.Date(VIX_Dates$d[400:781]),type="l",ylab="",xlab="",xaxt="n")
plot(VIX_Dates4$v[400:781]~as.Date(VIX_Dates4$d[400:781]),type="l",ylab="",xlab="",xaxt="n")

