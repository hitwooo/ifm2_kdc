attach(KDC)
train.price <- ts(price_KDC, start = 1, end = 475, frequency = 1) #train set
test.price <- price_KDC[476:500]
log.price <- log(train.price) #natural logarithm for train set

summary(ur.df(log.price, type = "drift", lag =0 )) #stationary at alpha = 10%
plot(log.price)
acf(log.price)
pacf(log.price)
#=> from acf and pacf => the series follows AR(1)
reg.ar1 <- arma(log.price,order = c(1,0))
summary(reg.ar1) #ok tai muc alpha 10%

beta_0 <- 0.265575  
phi <- 0.975934

X_bar <- beta_0/(1-phi)
X_bar

#Model: X_t = c + 0.975394(X_t-1- X_bar) + epsilon_t

#Fitting model and forecast 

logprice_fit <- numeric(25)  # Initialize logprice_fit vector with 25 elements
logprice_fit[1] <- X_bar + phi * (log.price[475] - X_bar)  # Calculate logprice_fit[1] based on log.price[475]
for (k in 2:25) {
  logprice_fit[k] <- X_bar + phi * (logprice_fit[k-1] - X_bar)  # Calculate subsequent values based on previous logprice_fit
}
logprice_fit

price_fit <- exp(logprice_fit) #forecasted price based on forecasted log price

price.difference <- abs(test.price-price_fit)
price.difference
test.price

plot(price_fit, type = "l",col = 'green')
lines(test.price, col="red")

