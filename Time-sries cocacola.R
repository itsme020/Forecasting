Avaition


#install.packages(c("forecast","fpp","smooth","tseries"))
#install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)

library(readxl)
aviation<-read.csv("C:/Users/User/Desktop/New folder/Aviation.csv")

View(aviation)



#Converting data into time series object
amts<-ts(aviation$Sales,frequency=4,start=c(86))
View(amts)
plot(amts)

#dividing entire data into training and testing data
train<-amts[1:38]
test<-amts[39:42] #Considering only 4 Quarters of data for testing
#seasonal data

#cnverting time series object
train<-ts(train,frequency=4)
test<-ts(test, frequency=4)

#Plotting time series data
plot(train)# Visualization shows thatit has level, trend,seasonality =>

####Using HoltWinters function####
# Optimum values
# with alpha=0.2 which is defaulyt value
#Assuming time series data has only level parameter.
hw_a<-HoltWinters(train,alpha=0.2,beta=F,gamma=F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
#By looking at the plot the forecasted values are not showing by characters
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100

#with alpha=0.2, beta=0.1
#Assuming time series data has level and trend parameter
hw_ab<-HoltWinters(train,alpha=0.2,beta=0.1,gamma=F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead=4))
#By looking at the plot the forecasted values are still missing some character
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100

#with alpha=0.2, beta=0.1, gamma=0.1
#Assuming time series data has level, trend and seasonality
hw_abg<-HoltWinters(train,alpha=0.2,beta=0.1,gamma=0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead=4))
#By looking at the plot the forecasted values are closesly following
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

#with out optimal value
hw_na<-HoltWinters(train,beta=F,gamma=F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead=4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead=4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100


df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape)) #..........................

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

#Based on the MAPE,value who choose winter exponential technique which assuems the time series
#data level,trend,seasnality character with default value of alpha ,beta and gamma

new_model<-HoltWinters(amts)
plot(forecast(new_model,n.ahead=4))

#Forecasted value of next 4 quarters
forecast_new<-data.frame(predict(new_model,n.ahead=4))


########################################################


 

#hence mape of hwnabg_mape	2.397211 less than others.So this is our best fit model.




