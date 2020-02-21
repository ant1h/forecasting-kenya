############################################################
# Statistical modelling to forecast Kenya economic growth
# 
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

# The approach here is to forecast each expenditure component of GDP by expenditure
# (i.e. households consumption, government consumption, investments, exports, imports)
# The approach here is purely statistical, which means that we fit pure autoregressive models
# without any economic asumptions about behaviors


# Lets use Automatic ARIMA Forecasting for each component



# Estimate ARIMA models for each expenditure of GDP
# (We build a list of models for each component)
id_col <- c("h_cons","gov_cons","gfcf","invent","exp","imp") # variables for wich we want to estimate a model

models <- lapply(id_col,function(i){
  mod <- auto.arima(data_ev[,i],
                    stationary = TRUE, # restrict search to stationary models
                    d = 0, # differences
                    D = 0, # seasonal differences
                    max.p = 2, # max AR terms
                    max.q = 2, # max MA terms
                    max.P = 0, # max SAR terms
                    max.Q = 0, # max SMA terms
                    trace = TRUE, # show models as estimated
                    stepwise = FALSE)
  return(mod)
})


# Forecast and plot a GDP component
# (example)
id_exp <- 'exp'
mod <- models[[which(id_col==id_exp)]]
summary(mod)
fc <- forecast(mod,10)
plot(fc)
autoplot(fc)


# Forecast each GDP component and then GDP
# Forecast each evolution rate base on the best ARIMA model identified
M <- NULL
for (i in id_col){
  f <- forecast(models[[which(id_col==i)]],pred_len)$mean
  M <- as.data.frame(cbind(M,c(f)))
}
colnames(M) <- id_col
M$Year <- c(start_pred:end_pred)

# Add forecast to data_ev
data_ev <- bind_rows(data_ev,M)
#print(data_ev,n=58)


# Forecast evolution rate of GDP
for (y in start_pred:end_pred){
  exp_rates  <- data_ev[data_ev$Year==y,id_col] # forecast for each expenditure for year y
  exp_weight <- data[data$Year==(y-1),id_col] / as.numeric(data[data$Year==(y-1),'gdp']) # rate of component in GDP
  exp_weight['imp'] <- -exp_weight['imp'] # Imports contributes negatively
  data_ev[data_ev$Year==y,'gdp'] <- sum(exp_rates*exp_weight) # compute GDP evolution rate forcast
  
  # Now we can infer levels for year y before doing year y+1
  levels_y <-exp(data_ev[data_ev$Year==y,c(id_col,'gdp')] + log(data[data$Year==(y-1),c(id_col,'gdp')]))
  data <- bind_rows(data,c(Year=y,levels_y))
}




# Update contributions to GDP growth with forcasts
# Compute for each expenditure the contribution to the evolution of GDP
data_contrib <- contrib(data,'gdp')
data_contrib[,'imp'] <- -data_contrib[,'imp'] # NB: imports growth contributes negatively to GDP
data_contrib$other <- data_contrib$gdp -( # Compute the contribution of "others" as a residual
  data_contrib$h_cons +
    data_contrib$gov_cons +
    data_contrib$gcf +
    data_contrib$exp +
    data_contrib$imp)
