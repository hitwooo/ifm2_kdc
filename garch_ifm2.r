library(dplyr)
library(urca)
library(xts)
library(ggplot2)
library(psych)
library(forecast)
library(tseries)
library(readxl)
library(reshape2)
library(rugarch)

# data <- read_excel("C:\\Users\\nguye\\OneDrive - National Economics University\\2324\\II_2324\\IFM2\\Assignment\\KDC.xlsx")
data <- read_excel('/Users/hitwooo/Library/CloudStorage/OneDrive-NationalEconomicsUniversity/2324/II_2324/IFM2/Assignment/KDC.xlsx')

data$time <- as.Date(data$time, format = "%Y-%m-%d")
kdc <- data$price_KDC
kdc <- ts(kdc, start = 1, end = 500)
ggplot(data, aes(x = data$time, y = kdc)) + #plot for closing price
  geom_line(color = "black") +  # Line color
  labs(title = "Time Series Plot For kdc", x = "Date", y = "kdc") +  # Labels and title
  theme_gray()  
dkdc <- diff(kdc)
plot(dkdc, type = 'l')
lkdc <- log(kdc)
ggplot(data, aes(x = data$time, y = lkdc)) + #plot for closing price
  geom_line(color = "black") +  # Line color
  labs(title = "Time Series Plot For ln(kdc)", x = "Date", y = "lkdc") +  # Labels and title
  theme_gray() 
dlkdc <- diff(lkdc)
dlkdc
dlkdc_train <- dlkdc[-((length(dlkdc)-24):length(dlkdc))]
dlkdc_test <- dlkdc[(length(dlkdc)-24):length(dlkdc)]

summary(ur.df(dlkdc_train,type = 'trend'))
summary(ur.df(dlkdc_train,type = 'drift'))
summary(ur.df(dlkdc_train,type = 'none'))

acf(dlkdc_train) #1,4
pacf(dlkdc_train) #1,4

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

kdc_fit <- vector(mode = 'numeric')
k = 1
print(kdc_fit)
kdc_fit[1] <- kdc[475]
for (i in 1:25){
  kdc_fit[i+1] <- kdc_fit[i]*exp(dlkdc_fit)[k]
  k = k + 1
}

plot(dlkdc_fit, type = 'l', col = 'blue')
plot(kdc_fit, type = 'l', col = 'green')
plot(kdc[475:500], col = 'red', type = 'l')

garchforecast_100 <- ugarchforecast(fitORspec = garchfit, n.ahead = 100)
dlkdc_fit_100 <- fitted(garchforecast_100)[,1]
kdc_fit_100 <- vector(mode = 'numeric')
k = 1
for (i in 476:500){
  kdc_fit[k] <- kdc[i-1]*exp(dlkdc_fit)[k]
  k = k + 1
}

plot(dlkdc_test, type = 'l', col = 'green')
lines(dlkdc_fit, col = 'red')