##Functions to calculate error / accuracy
rmse <- function(actual, predicted)
  round((sum((actual - predicted) ^ 2) / length(actual)) ^ .5, 2)
mape <- function(actual, predicted)
  round(mean(100 * abs((actual - predicted) / actual)), 2)



######## Section to write Data sets #############

write.csv(tslm_forecast,
          "tslm_forecast.csv",
          row.names = F)

#########################################################

setwd("C:\\Users\\v-abthya\\Desktop\\Scripts\\New2")
setwd("C:\\Users\\Abhishek\\Desktop\\Abhi")

install.packages('car')
install.packages('forecast', dependencies = TRUE)

library(prophet)
library(lubridate)
library(data.table)
require(xts)
library(forecast)
library(MASS)
library(car)
library(ggplot2)


############################ IMP DATA SETS ##############################
df <- read.csv("file.csv", header = T)
df$Date <- dmy(df$Date)
# all desired dates.
alldates <-
  data.table(Date = seq.Date(min(df$Date), max(df$Date), by = "day"))
# merge
dt <- merge(df, alldates, by = "Date", all = TRUE)
# now carry forward last observation (alternatively, set NA's to 0)


dt2 <- na.locf(dt)
dt2$USDAUD <- as.numeric(dt2$USDAUD)
dt2$Date <- as.Date(dt2$Date)
usdaud <- as.numeric(dt2$USDAUD)
prophet_dataframe <- dt2[, c(1, 2)]

predictors <- dt2[,-c(1,2)]
predictors <- data.frame(sapply( predictors, as.numeric ))


reg_Data <- cbind(usdaud, predictors)




#######################################################################
#######################################################################
#####     Model 1 - ARIMA (WITH NO PREDICTORS) ###############
#######################################################################

####################ARIMA FULL MODEL ####################
dt2$Date <- as.Date(dt2$Date)
dt2$USDAUD <- as.numeric(dt2$USDAUD)

tseries <- dt2[, c(2)]
curts <- ts(tseries, frequency = 365.25);curts;plot(curts)

difts <- diff(curts); plot(difts);
acf(curts)
acf(difts);Pacf(difts)

####Model Fitting#########
x <- auto.arima(curts);summary(x)
fore_arima <- forecast(x, h = 30);fore_arima;summary(fore_arima)
arima_forecast_dataframe <- as.data.frame(fore_arima)
plot(acf(fore_arima$residuals))
plot(fore_arima$residuals)
######Validation of Errors 
plot.ts(fore_arima$residuals)
hist(fore_arima$residuals)
#tsdiag(x)

plot(forecast(x, h = 30))

summary(auto.arima(curts))

####################ARIMA TRAIN TEST (NO PREDICTORS) ####################

arima_train <- prophet_dataframe[1:1425, ]
arima_test <- prophet_dataframe[1426:2025, ]

arima_train_tseries <- arima_train[, c(2)]
curts <- ts(arima_train_tseries, frequency = 365.25);curts; plot(curts)

####Model Fitting#########
x_train <- auto.arima(curts);summary(x_train)
fore_arima_train <- forecast(x_train, h = nrow(arima_test));fore_arima;summary(fore_arima)
arima_forecast_dataframe <- as.data.frame(fore_arima_train)

######Validation of Errors 
plot.ts(fore_arima_train$residuals)
hist(fore_arima_train$residuals)
#tsdiag(x)

plot(forecast(x, h = 600))
accuracy(fore_arima_train)
View(cbind(arima_train$USDAUD,fore_arima_train$fitted))

########################################################
###RMSE and MAPE for ARIMA
########################################################


#print("ARIMA RMSE / NP / ALL DATA");c(rmse(fore_arima$x, fore_arima$fitted))
#(rmse(arima_train$USDAUD, fore_arima_train$fitted),rmse(prophet_test$y, prophet_test$yhat))

print("ARIMA MAPE / NP / ALL DATA");c(mape(fore_arima$x, fore_arima$fitted))
print("ARIMA MAPE NP TRAIN/TEST ");c(mape(arima_train$USDAUD, fore_arima_train$fitted),mape(arima_test$USDAUD, arima_forecast_dataframe$`Point Forecast`))



#######################################################################
#######################################################################
#####     Model 2- PROPHET (WITH NO PREDICTORS) ###############
#######################################################################


install.packages('prophet')
library(prophet)
library(dplyr)

prophet_dataframe <- dt2[, c(1, 2)]
colnames(prophet_dataframe) <- c('ds', 'y')

prophet_train <- prophet_dataframe[1:1425, ]
prophet_test <- prophet_dataframe[1426:2025, ]

pdf <- prophet(prophet_train)
future <- make_future_dataframe(pdf, periods = 600)
prophet_forecast <- predict(pdf, future)

plot(pdf, prophet_forecast)
#write.csv(cbind(fore_arima$x, fore_arima$fitted),"checkingarimamape.csv",row.names = F)
#plot(forecast)
getwd()

########################################################
############################RMSE and MAPE for Prophet
########################################################

n <- nrow(prophet_train)
ntest <- nrow(prophet_test)
prophet_train$yhat <- prophet_forecast$yhat[1:n]
prophet_test$yhat <- prophet_forecast$yhat[(n + 1):(n + ntest)]

c(rmse(prophet_dataframe$y, forecast$yhat))
print("PROPHET MAPE / NP / ALL DATA");c(mape(prophet_dataframe$y, prophet_forecast$yhat))
c(rmse(prophet_train$y, prophet_train$yhat),rmse(prophet_test$y, prophet_test$yhat))
print("PROPHET MAPE NP / TRAIN / TEST");c(mape(prophet_train$y, prophet_train$yhat),mape(prophet_test$y, prophet_test$yhat))


#######################################################################
#######################################################################
#####     Model 3- ARIMAX (WITH PREDICTORS) ###############
#######################################################################

################### Train Test ########################
predictors_Train <- predictors[1:1345, ]
predictors_test <- predictors[1346:1918, ]

arimax_train <- prophet_dataframe[1:1345,]
arimax_test <- prophet_dataframe[1346:1918, ]

#sum(is.na(predictors_test))

#Model Fitting 
tseries <- prophet_dataframe[1:1345, c(2)]
curts <- ts(tseries, frequency = 365.25);curts;plot(curts)

x <-auto.arima(curts,xreg = predictors_Train,stepwise = FALSE,approximation = F);summary(x)

arimax_fore <-forecast(x, h = nrow(predictors_test), xreg = predictors_test);arimax_fore;summary(arimax_fore)

arimax_forecast_dataframe <- data.frame(arimax_fore)

plot(forecast(x, h = nrow(predictors_test), xreg = predictors_test))
#oops <- predict(x,n.ahead = nrow(predictors_test), newxreg = predictors_test)

############## MAPE 
print("ARIMAX MAPE / P / ALL DATA");c(mape(arimax_fore$x, arimax_fore$fitted))
print("ARIMAX MAPE P TRAIN/TEST ");c(mape(arimax_train$USDAUD, arimax_fore$fitted),mape(arimax_test$USDAUD, arimax_forecast_dataframe$Point.Forecast))

###############
############ 30 day forecast# Assuming we have predictors for the next 30 days ######
###############
predictors_Train <- predictors[1:1888, ]
predictors_test <- predictors[1889:1918, ]
arimax_train <- prophet_dataframe[1:1888,]
arimax_test <- prophet_dataframe[1889:1918, ]

#sum(is.na(predictors_test))

#Model Fitting 
tseries <- prophet_train[1:1888, c(2)]
curts <- ts(tseries, frequency = 365.25);curts;plot(curts)

x <-auto.arima(curts,xreg = predictors_Train,stepwise = FALSE,approximation = F);summary(x)

arimax_fore <-forecast(x, h = nrow(predictors_test), xreg = predictors_test);arimax_fore;summary(arimax_fore)

arimax_forecast_dataframe <- data.frame(arimax_fore)

###########PLOTTING##########################
#######################################
plot_data <- dt2[1:1888,c(1,2)]
plot_data$Legend <- "Actuals"
plot_forecast <- data.frame(dt2$Date[1889:1918],arimax_forecast_dataframe$Point.Forecast)
colnames(plot_forecast) <- c('Date', 'USDAUD')
plot_forecast$Legend <- "Forecasted"
final_plot <- rbind(plot_data,plot_forecast)

ggplot() + geom_line(data=final_plot, aes(x = Date, y = USDAUD, color = Legend)) + 
  stat_smooth(data=plot_forecast, aes(x = Date, y = USDAUD, color = Legend))





#######################################################################
#######################################################################
#####     Model 4- Regression (WITH PREDICTORS) ###############
#######################################################################

usdaud <- dt2$USDAUD
reg_Data <- cbind(usdaud, predictors)

nrow(na.omit(reg_Data))
names(reg_Data)
reg_final <- reg_Data[, c(1, 4, 6, 9, 10, 11)]
reg_train <- reg_final[1:1888, ]#1345
reg_test <- reg_final[1889:1918, ]

reg_model <- lm(usdaud ~ ., data = reg_train);summary(reg_model);vif(reg_model)
reg_forecast <- data.frame(predict(reg_model, reg_test, interval = "confidence"))
predict(reg_model)

############PLOT####################
#######################################
plot_data <- dt2[1:1888,c(1,2)]
plot_data$Legend <- "Actuals"
plot_forecast <- data.frame(dt2$Date[1889:1918],reg_forecast$fit)
colnames(plot_forecast) <- c('Date', 'USDAUD')
plot_forecast$Legend <- "Forecasted"
final_plot <- rbind(plot_data,plot_forecast)

ggplot() + geom_line(data=final_plot, aes(x = Date, y = USDAUD, color = Legend)) + 
  stat_smooth(data=plot_forecast, aes(x = Date, y = USDAUD, color = Legend))


########################## MAPE #############
print("REGRESSION MAPE / P / ALL DATA");c(mape(reg_train$usdaud, reg_model$fitted.values))
print("REGRESSION MAPE P TRAIN/TEST ");c(mape(reg_train$usdaud, reg_model$fitted.values),mape(reg_test$usdaud, reg_forecast$fit))
fitted(reg_model)
plot(reg_train)

stepAIC(reg_model)
cor(reg_train[, -1])

plot(reg_model)
hist(reg_model$residuals)

#######################################################################
#######################################################################
#####     Model 5- Regression TSLM  (WITH PREDICTORS) ###############
#######################################################################
nrow(na.omit(reg_Data))
names(reg_Data)
reg_final <- na.omit(reg_Data[,c(1,4,6,9,10,11)])
colnames(reg_final) <- c("usdaud" ,"Debits" ,"USCAD",  "USPMI" , "USIIP","AUPMI" ) 
reg_final$usdaud <- ts(reg_final$usdaud,frequency = 365.25 )

reg_train <- reg_final[1:1888,]
reg_train$usdaud <- ts(reg_train$usdaud,frequency = 365.25 )

colnames(reg_train) <- c("x.usdaud" ,"x.Debits" ,"x.USCAD",  "x.USPMI" , "x.USIIP","x.AUPMI" ) 
reg_train$x.usdaud <- ts(reg_train$x.usdaud,frequency = 365.25 )


reg_test <- reg_final[1889:1918,]
reg_test$usdaud <- ts(reg_test$usdaud,frequency = 365.25 )

reg_model <- tslm(x.usdaud~.,data = reg_train);summary(reg_model);
plot(reg_model)
#predict.lm(reg_model,h=418 ,newdata = data.frame(x=reg_test[,-1])) 
tslm_x1 <- forecast(object = reg_model,newdata = data.frame(x=reg_test))
tslm_forecast<- data.frame(forecast(object = reg_model,newdata = data.frame(x=reg_test)))

# View(data.frame(x=reg_test))# summary(x1)# x1$class(reg_test)# View(x1$fitted)
###########PLOTTING##########################
#######################################
plot_data <- dt2[1:1888,c(1,2)]
plot_data$Legend <- "Actuals"
plot_forecast <- data.frame(dt2$Date[1889:1918],tslm_forecast$Point.Forecast)
colnames(plot_forecast) <- c('Date', 'USDAUD')
plot_forecast$Legend <- "Forecasted"
final_plot <- rbind(plot_data,plot_forecast)

ggplot() + geom_line(data=final_plot, aes(x = Date, y = USDAUD, color = Legend)) + 
  stat_smooth(data=plot_forecast, aes(x = Date, y = USDAUD, color = Legend))

plot()


c(mape(reg_test$usdaud[1:20],x1$Point.Forecast[1:20]))




