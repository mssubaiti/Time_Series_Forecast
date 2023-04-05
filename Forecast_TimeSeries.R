library(fBasics)
library(forecast) 
library(fGarch)
library(car)

# Reading the data

data<- read.csv('data/Homework2DATA.csv',header=TRUE,sep=";",dec=",", check.names = F)

y<-data[,2]

################  1. The best time series model for the variable “ibex”  ##################

par(mfrow=c(3,1))
ts.plot(y)
acf(y,104)
pacf(y,104) 

# Clearly the data is not stationary both in the mean and variance (slow decrease in ACF)
# Let's check with the formal tests

s=52      # seasonal parameter for weekly data

nsdiffs(y,m=s,test=c("ocsb"))  # we don't need seasonal difference
ndiffs(y, alpha=0.05, test=c("adf")) # we need 1 regular difference


# Fitting ARIMA(0,1,0)
fit<-arima(y,order=c(0,1,0))    
fit # we find the information about the estimated parameters

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals)
pacf(fit$residuals)  

Box.test(fit$residuals,lag=104)  #residuals are white noise

# testing for normality 
shapiro.test(fit$residuals)    # residuals are normally distributed

par(mfrow=c(2,1))
acf(fit$residuals^2, 104)
pacf(fit$residuals^2, 104) 
Box.test(fit$residuals^2,lag=104)    # residuals are SWN

# Forecasting
y.pred<-predict(fit,n.ahead=4)
y.pred$pred # point predictions
y.pred$se  # standard error for the point predictions to compute confidence intervals

# The prediction doesn't look good
ts.plot(y)
lines(y.pred$pred,col="red")
lines(y.pred$pred+1.96*y.pred$se,col="red",lty=3)
lines(y.pred$pred-1.96*y.pred$se,col="red",lty=3)

################## 2. Find the best regression model  for  “ibex”. ##########################

# Exploring the correlation with the target
cor(data)
# looks like all of our columns are highly correlated

# Let's construct a model with multicollenearity and check whether the residuals are white noise

model_1 <- lm(y~., data = data[,-c(1,2)])
summary(model_1)    #R square is 0.9455

# Checking whether residuals are white noise - no, they are not graphically
par(mfrow=c(3,1))
plot(model_1$residuals,type='l')
acf(model_1$residuals,lag=104)
pacf(model_1$residuals,lag=104)

Box.test(model_1$residuals)   # it's not white noise but we can't do anything about it here
ndiffs(model_1$residuals, alpha=0.05, test=c("adf"))  # we don't need regular difference
nsdiffs(model_1$residuals,m=s,test=c("ocsb"))  # we don't need seasonal difference

# Checking for multicollenearity
car::vif(model_1)   # there is multicollenearity with long term rate

# Formal test on heteroscedasticity
lmtest::bptest(model_1)  # Reject H0 - we have heteroscedasticity


########### 3. Find the best regression model with time series errors for  “ibex”. ############
# Our previous model
model_1 <- lm(y~., data = data[,-c(1,2)])
summary(model_1)            # R sq = 0.9455


par(mfrow=c(3,1))
plot(model_1$residuals,type='l')
acf(model_1$residuals)
pacf(model_1$residuals)

Box.test(model_1$residuals)  # not white noise
ndiffs(model_1$residuals, alpha=0.05, test=c("adf"))  # we don't need regular difference
nsdiffs(model_1$residuals,m=s,test=c("ocsb"))  # we don't need seasonal difference


time_model <- arima(y,order=c(2,0,0),xreg = data[,-c(1,2)])
time_model # ar2 is not significant and short term too

time_model_2 <- Arima(y,order=c(1,0,0), xreg = as.matrix(data[,-c(1,2,4)], nrow = 109, ncol = 2))
time_model_2 

par(mfrow=c(2,1))
plot(time_model_2$residuals,type='l')
acf(time_model_2$residuals)
pacf(time_model_2$residuals)  # white noise


Box.test(time_model_2$residuals)  # white noise
ndiffs(time_model_2$residuals, alpha=0.05, test=c("adf"))  # we don't need regular difference
nsdiffs(time_model_2$residuals,m=s,test=c("ocsb"))  

# testing for normality 
shapiro.test(time_model_2$residuals)    # residuals are normally distributed

par(mfrow=c(2,1))
acf(time_model_2$residuals^2, 104)
pacf(time_model_2$residuals^2, 104) 

Box.test(time_model_2$residuals^2,lag=104)    # residuals are SWN


################################## Comparing the models  #####################################
## ARIMA(0,1,0)
sqrt(sum(fit$residuals^2)/(length(y)-2))    # residual se 80.41  (sqrt(sigma)^2 from summary)

## Ordinary regression
sigma(model_1)          # residual se 129.32

## Regression with time series errors
sqrt(sum(time_model_2$residuals^2)/(length(y)-2))     # residual se 56.87 - the best model

################################ Predicting one step ahead ###################################
x <- matrix(0,length(y),2) 
x[,1]=data$`Exchange rate \x80/` 
x[,2] = data$`Long term rate` 

time_model <- Arima(y,order=c(1,0,0),xreg=x) 
time_model 

pred_x <- t(cbind(c(0.781,10.76))) 
prediction <-forecast(time_model,xreg=pred_x) 
prediction


prediction2=predict(time_model,newxreg = pred_x) 
prediction2

plot(prediction)




