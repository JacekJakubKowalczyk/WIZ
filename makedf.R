rm(list=ls())
setwd("~/Desktop/Praca licencjacka")
library("readxl")
library("zoo")
library("dplyr")
### Dane dla opcji
##Obcinanie nazw plików do dat
f_substrRight_four <- function(x, n){
  substr(x, nchar(x)-n-3, nchar(x)-4)
}


#date_format="%Y%m%d"
#Doklej dni do wygaśnięcia do dfa
f_days_between =function(x,y){
  z=as.numeric(as.Date(x) -as.Date(y))
  return(z)
}


##Lista plików
list_excel=list.files(pattern = "xls$", recursive = TRUE, full.names = FALSE)
list_excel=list_excel[grep("catalyst*",list_excel, invert=TRUE)]
list_excel=list_excel[grep("NC*",list_excel,invert=TRUE)]
##Nazwy kolumn
col_nam_options=c("Expiry", "Multiplier", "P/C", "Strike","ISIN","Name","Currency", "Ref_Price_next", "Last_price", "Ref_price", "Change%","Open","Low", "High", "Last", "Average","No_Trades", "Volume","Trading_value", "OI_No", "OI_Value","Delta","Gamma","Theta","Vega","Rho","Implied","Intrest", "Dividend", "Date" )
#Funkcja do klejenia danych
f_read_data_options=function(x){
  spread=read_excel(x, sheet = "opcje", skip = 9, col_names=FALSE)
  spread=spread[-c(9)]
  ##Greek
  spread2=read_excel(x, sheet = "wskaźniki opcji", skip = 8, col_names=FALSE)
  spread2=spread2[-c(1,2)]
  ##Date
  date=as.Date(f_substrRight_four(x,8),"%Y%m%d")
  ##Final Bind
  spread_final=cbind(spread, spread2,rep(date, nrow(spread)))
  return(spread_final)
}


#Ostateczne spięcie
final_df_options=bind_rows(lapply(list_excel,f_read_data_options)) #blad w danych od 24.05 do 31.05 2019
names(final_df_options)=col_nam_options
#Doklej dni do wygaśnięcia 
Days_to_expiry=numeric()
for (x in (1:nrow(final_df_options))){
  Days_to_expiry[x]= f_days_between(final_df_options$Expiry[x],final_df_options$Date[x])
}
final_df_options=cbind(final_df_options,Days_to_expiry)
rm(Days_to_expiry,x)

##Zapisz csv
write.csv(final_df_options, file="w20options2018.csv")


####################################################

###Dane dla indeksu
#Korzystam z f_substrRight_four zdefiniowanej wczesniej
col_names_index=c("Current","Previous","Change","Open", "Low", "High","Volume","P/BV","P/E","Div_yield", "Date")
f_read_data_index=function(x){ 
  spread=read_excel(x,skip = 14,col_names=FALSE,n_max=1)
  spread=spread[-c(1:4,8,9)]
  date=as.Date(f_substrRight_four(x,8),"%Y%m%d")
  cbind(spread,rep(date, nrow(spread)))
}
final_df_index=bind_rows(lapply(list_excel,f_read_data_index))
names(final_df_index)=col_names_index
##Zapisz jako csv
write.csv(final_df_index, file="w20index2018.csv")







############################## Notatki.zabawy
# #Zwykłe
# sheet=excel_sheets("20200601.xls")
# col_nam=c("Expiry", "Multiplier", "P/C", "Strike","ISIN","Name","Currency", "Ref_Price_next", "Last_price", "Ref_price", "Change%","Open","Low", "High", "Last", "Average","No_Trades", "Volume","Trading_value", "OI_No", "OI_Value" )
# spread=read_excel("20200601.xls", sheet = sheet[11], skip = 9, col_names=FALSE)
# spread=spread[-c(9)]
# colnames(spread)=col_nam
# ##Greckie
# col_nam2=read_excel("20200601.xls", sheet = sheet[12], skip = 5, n_max=1,col_names=FALSE)
# col_nam2=col_nam2[-c(1,2)]
# spread2=read_excel("20200601.xls", sheet = sheet[12], skip = 8, col_names=FALSE)
# spread2=spread2[-c(1,2)]
# colnames(spread2)=col_nam2
# ##Final Bind
# spread_final=cbind(spread, spread2)
# ##Zapisz jako csv
# write.csv(spread_final, file="20200601.csv")

# #Zabawa z plikami
# file_names=list.files()
# n=length(file_names)
# arg1=NULL
# arg2=1
# #Funkcja do sczytywania danych i lepienia ich w jeden plik (POPRAWIĆ ARG1, ARG2 ETC)
# combine_data<-function(arg1, arg2,arg3){
#   workdir=getwd()
#   if(length(list.files (path =(paste(workdir,"/",arg1, sep="")), pattern = "xls$")) = 0){
#     combine_data(list.files()[n],n)
#   }
#   for(x in list.files (path =(paste(workdir,"/",arg1, sep="") ))){
#     sheet=excel_sheets(x)
#     spread=read_excel(x, sheet = sheet[11], skip = 9, col_names=FALSE)
#     spread=spread[-c(9)]
#     colnames(spread)=col_nam
#     ##Greckie
#     col_nam2=read_excel(x, sheet = sheet[12], skip = 5, n_max=1,col_names=FALSE)
#     col_nam2=col_nam2[-c(1,2)]
#     spread2=read_excel(x, sheet = sheet[12], skip = 8, col_names=FALSE)
#     spread2=spread2[-c(1,2)]
#     colnames(spread2)=col_nam2
#     ##Final Bind
#     Date=rep(list.files()[arg2],each=nrow(spread2))
#     spread_final2=cbind(spread, spread2,date)
#     arg2=arg2+1
#     if(exists("spread_final")){
#     spread_final=rbind(spread_final, spread_final2)
#     }
#     else{
#       spread_final=spread_final2
#     }
#   }
# }

# xd=final_df[final_df$Date=="2020-06-01",]
# final_df[is.na(final_df$Date) ,]




