rm(list=ls())
setwd("~/Desktop/Praca licencjacka")
library("readxl")
library("zoo")
library("dplyr")
df_options=read.csv("w20options2018.csv", stringsAsFactors = FALSE)
df_index=read.csv("w20index2018.csv", stringsAsFactors = FALSE)
### Plot uśmiechów zmienności
# smile=df_options[df_options$Name=="OW20C212000",]
# smile=smile[order(smile$Date),]
# plot(smile$Implied,x=as.Date(smile$Date),type = "l")


##############################################################################3
#Kalkulacje za pomocą implikowanej zmienności z ceduły

# ###Test dla jednego dnia
# ##Pobierz dane dla jednego dnia
# options_date=df_options[df_options$Date=="2020-09-04"|df_options$Date=="2020-09-04",]
# index_date=df_index[df_index$Date=="2020-09-03"|df_index$Date=="2020-09-04",]
# index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
# options_oom_K=options_date[options_date$P.C=="K" & options_date$Strike>index_price_avg,]
# options_oom_S=options_date[options_date$P.C=="S"&options_date$Strike<index_price_avg,]
# VIX=weighted.mean(rbind(options_oom_K,options_oom_S)$Implied,rbind(options_oom_K,options_oom_S)$No_Trades)

##Funkcje pomocnicze

f_mean_geom = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# f_days_between =function(x,y){
#   z=as.numeric(as.Date(x) -as.Date(y))
#   return(z)
# }

f_find_nth_lowest_unique = function(x,n){
  y=sort(x%>%unique)
  return(y[n])
}
# #Doklej dni do wygaśnięcia do dfa
# Days_to_expiry=numeric()
# for (x in df_options$X){
#   Days_to_expiry[x]= f_days_between(df_options$Expiry[x],df_options$Date[x])
# }
# df_options=cbind(df_options,Days_to_expiry)
# rm(Days_to_expiry,x)

####################
dates1=df_index$Date%>%unique
f_calc_VIX_unweighted=function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    options_oom_K=options_date[options_date$P.C=="K" & options_date$Strike>index_price_avg,]
    options_oom_S=options_date[options_date$P.C=="S"&options_date$Strike<index_price_avg,]
    options_bind=rbind(options_oom_K,options_oom_S)
    VIX=weighted.mean(options_bind$Implied,options_bind$Trading_value)
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates=f_calc_VIX_unweighted(df_index,df_options,dates1)
VIX_Dates=as.data.frame(VIX_Dates)
names(VIX_Dates)=c("d","v")
VIX_Dates=VIX_Dates[order(VIX_Dates$d),]
plot(x=as.Date(VIX_Dates$d), y=VIX_Dates$v, type = "l",main="1")

#Ważony VIX dla opcji z wygaśnięciem większym niż 7 dni
f_calc_VIX_weighted_7=function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    options_oom_K=options_date[options_date$P.C=="K" & options_date$Strike>index_price_avg,]
    options_oom_S=options_date[options_date$P.C=="S"&options_date$Strike<index_price_avg,]
    options_bind=rbind(options_oom_K,options_oom_S)
    VIX=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry>7],options_bind$Trading_value[options_bind$Days_to_expiry>7])
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates2=f_calc_VIX_weighted_7(df_index,df_options,dates1)
VIX_Dates2=as.data.frame(VIX_Dates2)
names(VIX_Dates2)=c("d","v")
VIX_Dates2=VIX_Dates2[order(VIX_Dates2$d),]
plot(x=as.Date(VIX_Dates2$d), y=VIX_Dates2$v, type = "l",main="2")

#Wazony VIX dla opcji z wygaśnięciem pomiędzy 7 a 90
f_calc_VIX_unweighted_7_90=function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    options_oom_K=options_date[options_date$P.C=="K" & options_date$Strike>index_price_avg,]
    options_oom_S=options_date[options_date$P.C=="S"&options_date$Strike<index_price_avg,]
    options_bind=rbind(options_oom_K,options_oom_S)
    VIX=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry>21&options_bind$Days_to_expiry<90],options_bind$Trading_value[options_bind$Days_to_expiry>21&options_bind$Days_to_expiry<90])
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates3=f_calc_VIX_unweighted_7_90(df_index,df_options,dates1)
VIX_Dates3=as.data.frame(VIX_Dates3)
names(VIX_Dates3)=c("d","v")
VIX_Dates3=VIX_Dates3[order(VIX_Dates3$d),]
plot(x=as.Date(VIX_Dates3$d), y=VIX_Dates3$v, type = "l", main = "3")

#Wazony VIX dla opcji z dwóch następnych okresów rozliczeniowych między 7 a 35 dniem
f_calc_VIX_weighted_7_37=function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    options_oom_K=options_date[options_date$P.C=="K" & options_date$Strike>index_price_avg,]
    options_oom_S=options_date[options_date$P.C=="S"&options_date$Strike<index_price_avg,]
    options_bind=rbind(options_oom_K,options_oom_S)
    closest_exp1=f_find_nth_lowest_unique(options_bind$Days_to_expiry,1)
    closest_exp2=f_find_nth_lowest_unique(options_bind$Days_to_expiry,2)
    closest_exp3=f_find_nth_lowest_unique(options_bind$Days_to_expiry,3)
    if(closest_exp1<7){
      sigma_1=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp2],options_bind$Trading_value[options_bind$Days_to_expiry==closest_exp2])
      sigma_2=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp3],options_bind$Trading_value[options_bind$Days_to_expiry==closest_exp3])
      N1=closest_exp2
      N2=closest_exp3
    }
    else{
      sigma_1=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp1],options_bind$Trading_value[options_bind$Days_to_expiry==closest_exp1])
      sigma_2=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp2],options_bind$Trading_value[options_bind$Days_to_expiry==closest_exp2])
      N1=closest_exp1
      N2=closest_exp2
    }
    VIX=sigma_1*((N2-30)/(N2-N1))+sigma_2*((30-N1)/(N2-N1))
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates4=f_calc_VIX_weighted_7_37(df_index,df_options,dates1)
VIX_Dates4=as.data.frame(VIX_Dates4)
names(VIX_Dates4)=c("d","v")
VIX_Dates4=VIX_Dates4[order(VIX_Dates4$d),]
#Jeśli NAN weź średnią dwóch sąsiadujących
while(length(VIX_Dates4$v[VIX_Dates4$v=="NaN"])!=0){
  for (i in 1:nrow(VIX_Dates4)){
    if(VIX_Dates4$v[i]=="NaN"){
      VIX_Dates4$v[i]=f_mean_geom(c(as.numeric(VIX_Dates4$v[i-1]),as.numeric(VIX_Dates4$v[i+1])))
    }
  }
}
plot(x=as.Date(VIX_Dates4$d), y=VIX_Dates4$v, type = "l",main="4")

#Wazony ilością zawartych pozycji VIX dla opcji z dwóch następnych okresów rozliczeniowych między 7 a 37 dniem
f_calc_VIX_weighted_7_37_oi=function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    options_oom_K=options_date[options_date$P.C=="K" & options_date$Strike>index_price_avg,] 
    options_oom_S=options_date[options_date$P.C=="S"&options_date$Strike<index_price_avg,]
    options_bind=rbind(options_oom_K,options_oom_S)
    closest_exp1=f_find_nth_lowest_unique(options_bind$Days_to_expiry,1)
    closest_exp2=f_find_nth_lowest_unique(options_bind$Days_to_expiry,2)
    closest_exp3=f_find_nth_lowest_unique(options_bind$Days_to_expiry,3)
    if(closest_exp1<7){
      sigma_1=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp2],options_bind$OI_No[options_bind$Days_to_expiry==closest_exp2])
      sigma_2=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp3],options_bind$OI_No[options_bind$Days_to_expiry==closest_exp3])
      N1=closest_exp2
      N2=closest_exp3
    }
    else{
      sigma_1=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp1],options_bind$OI_No[options_bind$Days_to_expiry==closest_exp1])
      sigma_2=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry==closest_exp2],options_bind$OI_No[options_bind$Days_to_expiry==closest_exp2])
      N1=closest_exp1
      N2=closest_exp2
    }
    VIX=sigma_1*((N2-30)/(N2-N1))+sigma_2*((30-N1)/(N2-N1))
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates5=f_calc_VIX_weighted_7_37_oi(df_index,df_options,dates1)
VIX_Dates5=as.data.frame(VIX_Dates5)
names(VIX_Dates5)=c("d","v")
VIX_Dates5=VIX_Dates5[order(VIX_Dates5$d),]
#Jeśli NAN weź średnią dwóch sąsiadujących
while(length(VIX_Dates5$v[VIX_Dates5$v=="NaN"])!=0){
  for (i in 1:nrow(VIX_Dates5)){
    if(VIX_Dates5$v[i]=="NaN"){
      VIX_Dates5$v[i]=f_mean_geom(c(as.numeric(VIX_Dates5$v[i-1]),as.numeric(VIX_Dates5$v[i+1])))
    }
  }
}
plot(x=as.Date(VIX_Dates5$d), y=VIX_Dates5$v, type = "l",main="5")

#Ważony VIX dla opcji ATM  z dwóch okresów między 7 a 37



f_find_options_near_atm = function (options, price){
  vec_price_diff= options$Strike[order(abs(options$Strike - price))]
  vec_price_diff=vec_price_diff%>%unique
  return(c(vec_price_diff[1], vec_price_diff[2]))
}
f_calc_weights = function (x1,x2,goal){
  w1=(goal-x2)/(x1-x2)
  w2=1-w1
  return(c(w1,w2))
}
f_calc_VIX_weighted_ATM_7 = function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    closest_exp1=f_find_nth_lowest_unique(options_date$Days_to_expiry,1)
    closest_exp2=f_find_nth_lowest_unique(options_date$Days_to_expiry,2)
    closest_exp3=f_find_nth_lowest_unique(options_date$Days_to_expiry,3)
    closest_strikes=f_find_options_near_atm(options_date,index_price_avg)
    options_ctm=options_date[options_date$Strike==closest_strikes[1]|options_date$Strike==closest_strikes[2],]
    weight=f_calc_weights(closest_strikes[1],closest_strikes[2], index_price_avg)
    if(closest_exp1<21){
      sigma1_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])
      sigma1_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]])
      sigma1_first=ifelse(is.nan(sigma1_first),0,sigma1_first)
      sigma1_second=ifelse(is.nan(sigma1_second),0,sigma1_second)
      sigma_1=(sigma1_first+sigma1_second)/2
      #sigma_1=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]]))
      sigma2_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]])
      sigma2_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[2]])
      #sigma_2=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]]),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[2]])))
      sigma2_first=ifelse(is.nan(sigma2_first),0,sigma1_first)
      sigma2_second=ifelse(is.nan(sigma2_second),0,sigma1_second)
      sigma_2=(sigma2_first+sigma1_second)/2
      N1=closest_exp2
      N2=closest_exp3
    }
    else{
      sigma1_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[1]])
      sigma1_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[2]])
      #sigma_1=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[2]]))
      sigma1_first=ifelse(is.nan(sigma1_first),0,sigma1_first)
      sigma1_second=ifelse(is.nan(sigma1_second),0,sigma1_second)
      sigma_1=(sigma1_first+sigma1_second)/2
      sigma2_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])
      sigma2_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strik==closest_strikes[2]])
      #sigma_2=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]]),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]])))
      sigma2_first=ifelse(is.nan(sigma2_first),0,sigma1_first)
      sigma2_second=ifelse(is.nan(sigma2_second),0,sigma1_second)
      sigma_2=(sigma2_first+sigma1_second)/2
      N1=closest_exp1
      N2=closest_exp2
    }
    VIX=sigma_1*((N2-30)/(N2-N1))+sigma_2*((30-N1)/(N2-N1))
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates6=f_calc_VIX_weighted_ATM_7(df_index,df_options,dates1)
VIX_Dates6=as.data.frame(VIX_Dates6)
names(VIX_Dates6)=c("d","v")
VIX_Dates6=VIX_Dates6[order(VIX_Dates6$d),]
plot(x=as.Date(VIX_Dates6$d), y=VIX_Dates6$v, type = "l",main="6")

#VIX ATM powyżej 14 dni
f_calc_VIX_weighted_ATM_14 = function(options,index, dates){
  df_f = data.frame(Date=as.Date(character()),VIX=double())
  for (x in dates){
    options_date=df_options[df_options$Date==x,]
    index_date=df_index[df_index$Date==x,]
    index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
    closest_exp1=f_find_nth_lowest_unique(options_date$Days_to_expiry,1)
    closest_exp2=f_find_nth_lowest_unique(options_date$Days_to_expiry,2)
    closest_exp3=f_find_nth_lowest_unique(options_date$Days_to_expiry,3)
    closest_strikes=f_find_options_near_atm(options_date,index_price_avg)
    options_ctm=options_date[options_date$Strike==closest_strikes[1]|options_date$Strike==closest_strikes[2],]
    weight=f_calc_weights(closest_strikes[1],closest_strikes[2], index_price_avg)
    if(closest_exp1<21){
      sigma1_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])
      sigma1_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]])
      sigma1_first=ifelse(is.nan(sigma1_first),0,sigma1_first)
      sigma1_second=ifelse(is.nan(sigma1_second),0,sigma1_second)
      sigma_1=(sigma1_first+sigma1_second)/2
      #sigma_1=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]]))
      sigma2_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]])
      sigma2_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[2]])
      #sigma_2=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]]),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[2]])))
      sigma2_first=ifelse(is.nan(sigma2_first),0,sigma1_first)
      sigma2_second=ifelse(is.nan(sigma2_second),0,sigma1_second)
      sigma_2=(sigma2_first+sigma1_second)/2
      N1=closest_exp2
      N2=closest_exp3
    }
    else{
      sigma1_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[1]])
      sigma1_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[2]])
      #sigma_1=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp1&options_ctm$Strike==closest_strikes[2]]))
      sigma1_first=ifelse(is.nan(sigma1_first),0,sigma1_first)
      sigma1_second=ifelse(is.nan(sigma1_second),0,sigma1_second)
      sigma_1=(sigma1_first+sigma1_second)/2
      sigma2_first=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[1]])
      sigma2_second=weighted.mean(options_ctm$Implied[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]],options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strik==closest_strikes[2]])
      #sigma_2=weighted.mean(c(sigma1_first*weight[1],sigma1_second*weight[2]),c(sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp3&options_ctm$Strike==closest_strikes[1]]),sum(options_ctm$OI_No[options_ctm$Days_to_expiry==closest_exp2&options_ctm$Strike==closest_strikes[2]])))
      sigma2_first=ifelse(is.nan(sigma2_first),0,sigma1_first)
      sigma2_second=ifelse(is.nan(sigma2_second),0,sigma1_second)
      sigma_2=(sigma2_first+sigma1_second)/2
      N1=closest_exp1
      N2=closest_exp2
    }
    VIX=sigma_1*((N2-30)/(N2-N1))+sigma_2*((30-N1)/(N2-N1))
    df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)
  }
  return(df_f)
}
VIX_Dates7=f_calc_VIX_weighted_ATM_14(df_index,df_options,dates1)
VIX_Dates7=as.data.frame(VIX_Dates7)
names(VIX_Dates7)=c("d","v")
VIX_Dates7=VIX_Dates7[order(VIX_Dates7$d),]
plot(x=as.Date(VIX_Dates7$d), y=VIX_Dates7$v, type = "l",main="7")
####################################################################################
#Analiza opisowa
summary(as.numeric(VIX_Dates$v))
sd(as.numeric(VIX_Dates$v))
summary(as.numeric(VIX_Dates2$v))
sd(as.numeric(VIX_Dates2$v))
summary(as.numeric(VIX_Dates3$v))
sd(as.numeric(VIX_Dates3$v))
summary(as.numeric(VIX_Dates4$v))
sd(as.numeric(VIX_Dates4$v))
summary(as.numeric(VIX_Dates5$v))
sd(as.numeric(VIX_Dates5$v))
summary(as.numeric(VIX_Dates6$v))
sd(as.numeric(VIX_Dates6$v))
summary(as.numeric(VIX_Dates7$v))
sd(as.numeric(VIX_Dates7$v))

#ZAPISZ DO CSV
# write.csv(VIX_Dates4, file="VIX_Value.csv")
# write.csv(VIX_Dates5, file="VIX_Interest.csv")







###########Implikowana zmienność liczona ręcznie
##### Wartości wychodzą podobne do tych z ceduły z ,,brzydszym" uśmiechem zmienności - wolę korzystać z ceduły
##Uniroot znajduje pierwiastek dla pierwszego argumentu funkcji 
#S-Kurs WIG20, X- Wykonanie opcji, r- stopa, d- dywidenda, t- czas do wygaśnięcia w latach, sigma - zmienność implikowana, 
# f_find_implied_call = function(sigma,S,X,r,d,t, call_price)
# {
#   d1=(log(S/X) + (r+d-sigma^2/2)*t)/(sigma*sqrt(t))
#   d2=d1-sigma*sqrt(t)
#   out=S*exp(-d*t)*pnorm(d1)-X*exp(-r*t)*pnorm(d2)-call_price
#   return(out)
# }
# 
# f_find_implied_put = function(sigma,S,X,r,d,t, call_price)
# {
#   d1=(log(S/X) + (r+d-sigma^2/2)*t)/(sigma*sqrt(t))
#   d2=d1-sigma*sqrt(t)
#   out=-S*exp(-d*t)*pnorm(-d1)+X*exp(-r*t)*pnorm(-d2)-call_price
#   return(out)
# }
# #Test dla jednego dnia
# options_date=df_options[df_options$Date=="2020-09-03",]
# index_date=df_index[df_index$Date=="2020-09-03",]
# index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
# 
# Si=index_price_avg
# Xi=options_date$Strike[52]
# ri=options_date$Intrest[52]
# di=options_date$Dividend[52]
# 
# call_pricei=f_mean_geom(as.numeric(c(options_date$Open[52],options_date$Last[52],options_date$High[52],options_date$Low[52])))
# t= as.Date(options_date$Expiry[52])-as.Date(options_date$Date[52])
# sigma=uniroot(f=f_find_implied_call, S=Si, X=Xi,d=di,r=ri,t=as.numeric(t/365), call_price=call_pricei,lower=1, upper=200)$root


# if (closest_exp1<7){
#   sigma1=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry=closest_exp2],options_bind$Trading_value[options_bind$Days_to_expiry=closest_exp2])
#   sigma2=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry=closest_exp3],options_bind$Trading_value[options_bind$Days_to_expiry=closest_exp3])
#   N1=closest_exp2
#   N2=closest_exp3
# }
# else{
#   sigma1=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry=closest_exp1],options_bind$Trading_value[options_bind$Days_to_expiry=closest_exp1])
#   sigma2=weighted.mean(options_bind$Implied[options_bind$Days_to_expiry=closest_exp2],options_bind$Trading_value[options_bind$Days_to_expiry=closest_exp2])
#   N1=closest_exp1
#   N2=closest_exp2
# # }
# VIX=sigma_1*((N2-30)/(N2-N1))+((30-N1)/(N2-N1))
# df_f=rbind(df_f,c(x,VIX), stringsAsFactors = FALSE)



###################################################################################################################
### VIX JAK NA WHITEPAPERZE
#T= czas do wygaśnięcia / 365
#r - jak w cedule

###Test dla jednego dnia
##Pobierz dane dla jednego dnia
# options_date=df_options[df_options$Date=="2020-09-04",]
# index_date=df_index[df_index$Date=="2020-09-03",]
# index_price_avg=f_mean_geom(c(index_date$Open,index_date$High,index_date$Low, index_date$Current))
# 
# f_find_smallest_diff_strike = function(options){
#   val=numeric()
#   for(i in 1:length(x)){
#     val[i] = abs(options$Strike[i]- options$)
#   }
# }
# 
# f_calc_forward_index_price=function(strike_price,r,t,call_price,put_price){
#   return(strike_price+exp(r*(t/365))*(call_price-put_price))
# }




