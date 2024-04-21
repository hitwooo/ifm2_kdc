library('parallel')
library('rugarch')
library(car)
library(zoo)
library(lmtest)
library(rio)
library(urca)
library(Hmisc)
library(Metrics)
library(tseries)
library(forecast)
library(FinTS)
library(readxl)

data <- read_excel("C:\\Users\\nguye\\OneDrive - National Economics University\\2324\\II_2324\\IFM2\\Assignment\\KDC.xlsx")
data$time <- as.Date(data$time, format = "%Y-%m-%d")
kdc <- data$price_KDC
plot(kdc, type = 'l')
dkdc <- diff(kdc)
print(dkdc)
plot(dkdc, type = 'l')
lkdc <- log(kdc)
plot(lkdc, type = 'l')
dlkdc <- diff(lkdc)
dlkdc_train <- dlkdc[-((length(dlkdc)-24):length(dlkdc))]
dlkdc_test <- dlkdc[(length(dlkdc)-24):length(dlkdc)]

plot(dlkdc, type = 'l')

acf.appl=acf(dlkdc,main='ACF Difference Log kdc',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(dlkdc,main='PACF Difference Log kdc',lag.max=100,ylim=c(-0.5,1))

arima202 <- Arima(dlkdc,order=c(2,0,2))
summary(arima202)
autoplot(arima202)
checkresiduals(arima202)


garchspec <- ugarchspec(
  mean.model = list(armaOrder = c(2,2), include.mean = FALSE),
  variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
  distribution.model = 'norm'
)

garchfit <- ugarchfit(data = dlkdc_train,
                      spec = garchspec)

garchforecast <- ugarchforecast(fitORspec = garchfit,
                                n.ahead = 25)

dlkdc_fit <- fitted(garchforecast)[,1]
plot(dlkdc_test, type = 'l', col = 'green')
lines(dlkdc_fit, col = 'red')

dlkdc_fit
exp(dlkdc_fit)

kdc_fit 
k = 1
print(kdc_fit)
for (i in 476:500){
  kdc_fit[k] <- kdc[i-1]*exp(dlkdc_fit)[k]
  k = k + 1
}
kdc_fit
plot(kdc_fit, type = 'l', col = 'green')
lines(kdc[475:500], col = 'red')