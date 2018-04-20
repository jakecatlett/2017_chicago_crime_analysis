library(forecast)

  #Forecasts and plots for total crimes

my_series <- ts(crimes_total_YearMo$no_rows,
                start=c(2001, 12),
                end=c(2017, 7),
                frequency = 12)

seasonal_series <- stl(my_series, s.window="period")
plot(seasonal_series, xlab="Total")

plot(aggregate(my_series, FUN=mean), xlab="Total")

boxplot(my_series~cycle(my_series), xlab="Total")

adf.test(diff(log(my_series)),
         alternative = "stationary",
         k=0)

acf(diff(log(my_series)))
pacf(diff(log(my_series)))

fit <- arima(log(my_series), c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

predictions <- predict(fit, n.ahead = 5*12)

ts.plot(my_series, 2.718^predictions$pred, log="y", lty=c(1,3), xlab="Total")


#######################################################################3


#Forecasts and plots for non-violent crimes

my_series <- ts(nonV_crimes_YearMo$no_rows,
                start=c(2001, 12),
                end=c(2017, 7),
                frequency = 12)

seasonal_series <- stl(my_series, s.window="period")
plot(seasonal_series, xlab="Non-Violent")

plot(aggregate(my_series, FUN=mean), xlab="Non-Violent")

boxplot(my_series~cycle(my_series), xlab="Non-Violent", main="Non-Violent Crimes per Month")

adf.test(diff(log(my_series)),
         alternative = "stationary",
         k=0)

acf(diff(log(my_series)))
pacf(diff(log(my_series)))

fit <- arima(log(my_series), c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

predictions <- predict(fit, n.ahead = 5*12)

ts.plot(my_series, 2.718^predictions$pred, log="y", lty=c(1,3), xlab="Non-Violent",
        main="Historical and Predicted \n Trends For Non-Violent Crime", col="blue")


#######################################################################3


#Forecasts and plots for domestic crimes

my_series <- ts(dom_crimes_YearMo$no_rows,
                start=c(2001, 12),
                end=c(2017, 7),
                frequency = 12)

seasonal_series <- stl(my_series, s.window="period")
plot(seasonal_series, xlab="Domestic")

plot(aggregate(my_series, FUN=mean), xlab="Domestic")

boxplot(my_series~cycle(my_series), xlab="Domestic", main="Domestic Crimes per Month")

adf.test(diff(log(my_series)),
         alternative = "stationary",
         k=0)

acf(diff(log(my_series)))
pacf(diff(log(my_series)))

fit <- arima(log(my_series), c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

predictions <- predict(fit, n.ahead = 5*12)

ts.plot(my_series, 2.718^predictions$pred, log="y", lty=c(1,3), xlab="Domestic",
        main="Historical and Predicted \n Trends For Domestic Crime", col="blue")

#######################################################################3


#Forecasts and plots for violent crimes

my_series <- ts(v_crimes_YearMo$no_rows,
                start=c(2001, 12),
                end=c(2017, 7),
                frequency = 12)

seasonal_series <- stl(my_series, s.window="period")
plot(seasonal_series, xlab="Violent")

plot(aggregate(my_series, FUN=mean), xlab="Violent")

boxplot(my_series~cycle(my_series), xlab="Violent", main="Violent Crimes per Month")

adf.test(diff(log(my_series)),
         alternative = "stationary",
         k=0)

acf(diff(log(my_series)))
pacf(diff(log(my_series)))

fit <- arima(log(my_series), c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

predictions <- predict(fit, n.ahead = 5*12)

ts.plot(my_series, 2.718^predictions$pred, log="y", lty=c(1,3), 
        xlab="Violent", main="Historical and Predicted \n Trends For Violent Crime", col="blue")

#######################################################################3


#Forecasts and plots for murders

my_series <- ts(murder_YearMo$no_rows,
                start=c(2001, 12),
                end=c(2017, 7),
                frequency = 12)

seasonal_series <- stl(my_series, s.window="period")
plot(seasonal_series, xlab="Murder")

plot(aggregate(my_series, FUN=mean), xlab="Murder")

boxplot(my_series~cycle(my_series), xlab="Murder", main="Murders per Month")

adf.test(diff(log(my_series)),
         alternative = "stationary",
         k=0)

acf(diff(log(my_series)))
pacf(diff(log(my_series)))

fit <- arima(log(my_series), c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))

predictions <- predict(fit, n.ahead = 5*12)

ts.plot(my_series, 2.718^predictions$pred, log="y", lty=c(1,3), 
        xlab="Murder", main="Historical and Predicted \n Trends For Murder", col="blue")

