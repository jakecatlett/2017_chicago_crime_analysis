test_crimes <- crimes_per_day_monthly[1:nTest,]

validate_crimes <- crimes_per_day_monthly[132:199, ]

test_series <- ts(test_crimes$no_crimes,
                  start=c(2001, 12),
                  end=c(2011, 11),
                  frequency = 12)

test_fit <- arima(log(test_series), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

predictions <- predict(test_fit, n.ahead = 68)

E_test <- validate_crimes$no_crimes - 2.718^predictions$pred
E_test <- as.data.frame(E_test)
colnames(E_test) <- c("errors")
E_test$sqrErrors <- E_test$errors^2

RMSE <- sqrt(sum(E_test$sqrErrors) / nrow(E_test))
