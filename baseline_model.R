############################################################
# Baseline model for forecasting
# Simple univariate autoregressive model on GDP
############################################################

# Choose end of forecast year
end_pred   <- 2030 # user choice for end of forecast
# Uncertainty parameters
int1 <- 60 # first confidence interval  (in %)
int2 <- 95 # second confidence interval (in %)

# Define forecast window
start_data <- min(data$Year)
end_data   <- max(data$Year)
start_pred <- end_data + 1
pred_len <- (end_pred - start_pred)+1
window <- c(start_data,end_data,start_pred,end_pred)



############################################################
# Time series analysis of GDP serie
############################################################

# GDP serie in 2010 constant prices (UN data)
gdp <- ts(data=data$gdp,
          start=start_data,
          end=max(data$Year))

plot.ts(gdp, col = "red")
abline(h = mean(gdp), col = "blue")

# Take delta logs of gdp
dgdp <- diff(log(gdp))
plot.ts(dgdp, col = "red")
abline(h = mean(dgdp), col = "blue")

# Is growth rate stationary ?
# Dickey-Fuller test
adf.test(dgdp, alternative="stationary", k=0)
# looks like gdp growth rate is stationnary enough


# plot ACF / PACF of relevent ts
# ACF: auto-correlation of the serie with its lagged values
# PACF: partial auto-correlation function, ie correlation of the residuals 
# (which remains after removing the effects which are already explained by the earlier lag(s)) with the next lag value

# AR(p) process: ACF declines gradually / PACF cuts off after p lags
# MA(p) process: PACF declines gradually / ACF cuts off after p lags

par(mfrow = c(2, 1))
acf(dgdp) ; pacf(dgdp)




# Check if there is not a strong structural change in the series
# (risk of important model specification if there is a clear one)
# detecting abreakpoint using "strucchange" package
mod_breakpoint <- Fstats(dgdp ~ 1) # perform Chow tests
breakpoint <- data$Year[mod_breakpoint$breakpoint]

print(paste("Potential breakpoint at year",breakpoint))
print("Check graphs to see if it looks relevant")

# So far it doesn't look very relevant to estimate a different model from 2008
rm(mod_breakpoint, breakpoint) # clean variable


############################################################
# Model estimation
############################################################


# We could manually estimate several ARMA(p,q) models based on the analysis of ACF/PACF
# Then compute fit metrics such as the information criterions AIC or BIC
# Then decide which model is our best choice

# But it is easier to use the auto.arima function
# that will try every possible model and return the best one

# For more details, the choosing algorithm behind auto.arima is
# Hyndman-Khandakar algorithm for automatic ARIMA modelling
# see procedure here: https://otexts.com/fpp2/arima-r.html


# Automatic ARIMA Forecasting
auto.arima(dgdp, # my time series 
           stationary = TRUE, # restrict search to stationary models
           d = 0, # differences
           D = 0, # seasonal differences
           max.p = 3, # max AR terms
           max.q = 3, # max MA terms
           max.P = 0, # max SAR terms
           max.Q = 0, # max SMA terms
           trace = TRUE, # show models as estimated
           stepwise = FALSE)

# Based on AIC, AICc and BIC
# Best models are ARMA(1,0) / ARMA(0,1) / ARMA(1,1)
# For simplicity purpose, lets's fit an AR(1) with non-zero mean

baseline_model <- arima(dgdp, c(1, 0, 0))

# Accuracy and AR roots ckeck of the baseline model
accuracy(baseline_model)
autoplot(baseline_model)

# Forecast
# NB: confidence intervals can be estimated using bootstrapped errors
# (i.e., selected randomly from the estimated residuals)
fc <- forecast(baseline_model,level=c(int1,int2),h=pred_len)
fc <- forecast(baseline_model,level=c(int1,int2),h=pred_len,bootstrap=T)



title <- paste("Real growth forecast from baseline model, with",
           int1,"% and",
           int2,"% confidence intervals")


autoplot(fc,main=title) # plot forecasts and confidence intervals
