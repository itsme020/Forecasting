library(readr)
windows()
.




plot(CocaCola_Sales_Rawdata$Sales,type="l")
#so creating 11 dummy variables
Q1 <-  ifelse(grepl("Q1",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CocaCola_Sales_Rawdata$Quarter),'1','0')
CocaCola1<-cbind(CocaCola_Sales_Rawdata,Q1,Q2,Q3,Q4)
View(CocaCola1)
colnames(CocaCola1)

#x<-data.frame(outer(rep(quarters(Q1,Q2,Q3,Q4),length=120),month.abb,"==")+0)
#View(x)

#colnames(x)<-month.abb# assigning month names
#View(x)
#trakdata<-cbind(PlasticSales,x)
#View(trakdata)
#colnames(trakdata)[2]<-"Sales"
#colnames(trakdata)
CocaCola1["t"]<-1:42
View(CocaCola1)

CocaCola1["log_sales"]<-log(CocaCola1["Sales"])
CocaCola1["t_square"]<-CocaCola1["t"]*CocaCola1["t"]
#attach(trakdata)

##Data Partition
train<-CocaCola1[1:38,]
test<-CocaCola1[39:42,]


#########################LINEAR MODEL############################################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval = 'predict',newdata = test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm=T))
View(rmse_linear)



###################################Exponential################


expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval = 'predict',newdata = test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm=T))
rmse_expo

################################Quadratic################################

Quad_model<-lm(Sales~t+t_square,data = train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval = 'predict',newdata = test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################3Additive Seasonality######################33

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata = test,interval = 'predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm=T))
rmse_sea_add

#####################Additive Seasonality with Linear#######################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval = 'predict',newdata = test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

############################Additive Seasonality With Quardratic############

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary((Add_sea_Quad_model))
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval = 'predict',newdata = test))                         
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#######################Multiplicative Seasonality#######################################

multi_sea_model<-lm(log_sales~Q1+Q2+Q3+Q4,data=train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata = test,interval = 'predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm=T))
rmse_multi_sea

##########################Multiplicative Seasonality Linear Trend###################3

multi_add_sea_model<-lm(log_sales~t+t+Q1+Q2+Q3+Q4,data=train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,interval = 'predict',newdata = test)) 
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm=T))
rmse_multi_add_sea          

########Preparing Table on model and its rmse values######################

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo",
                                 "rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad",
                                 "rmse_multi_sea","rmse_multi_add_sea","Add_sea_Linear_model"),
                       'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,
                                rmse_multi_sea,rmse_multi_add_sea,rmse_Add_sea_Linear))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#################Multiplicative Seasonality Linear Trend has least value.
new_model<-lmlm(log_sales~t+t+Q1+Q2+Q3+Q4,data=CocaCola1)
# Predict (new_model,N.ahead=1)


#getting Residual
resid<-residuals(new_model)
resid[1:10]      
windows()
acf(resid,lag.max = 10)
# By principle or parcimony we will consider lag-1 as we have so
#many Significant lags
#building Autoreggressive model on residuals consider lag-1
k<-arima(resid,order=c(1,0,0))



windows()
acf(k$residuals,lag.max=15)
pred_res<-predict(arima(k$residuals,order=c(1,0,0)),n.ahead=42)
str(pred_res)                  
pred_res$pred
acf(k$residuals)
write.csv(CocaCola1,file="Cocacola1.csv", col.names= F,row.names = F)


getwd()





################3predicting new data####################
library(readxl)
test_data<-read_excel(file.choose(),1)    #load predict_new.xlsx
view(test_data)
pred_new<-data.frame(predict(new_model,newdata = test_data,interval = 'predict'))
view(pred_new)
pred_re<-pred_res$pred[1:12]
pred_new$fit<-pred_new$fit+pred_res$pred[1:12]
view(pred_new)



