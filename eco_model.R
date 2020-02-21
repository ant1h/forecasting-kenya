############################################################
# Economic modelling to forecast Kenya economic growth
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


# The approach here is to estimate an equation for each GDP component
# now adding economic asumptions and using exogenous data



# The first thing we need to do here is to check that the new regressors are also stationary
# so that we can include them in our equations

adf.test(exdata$wg, alternative="stationary", k=0)
# world growth appears to be stationnary

adf.test(exdata$dint[!is.na(exdata$dint)], alternative="stationary", k=0)
# real interest rate evolution appears to be stationnary

adf.test(exdata$doil[!is.na(exdata$doil)], alternative="stationary", k=0)
# oil prices evolution appears to be stationnary

adf.test(exdata$dexch[!is.na(exdata$dexch)], alternative="stationary", k=0)
# exchange rate evolution appears to be stationnary




############################################################
############################################################
# I - Equations estimations
############################################################
############################################################


# Compute evolution rates (as delta-log)
data_ev <- evol_d(data)
# Let's add external series inr our working dataset
data_ev <- data.frame( (cbind(data_ev,exdata[-1,c('wg','ssg','dint','doil','dexch','dprice','int')])))

# Convert to time series format
data_ev <- ts(data_ev,start=data_ev[1,'Year'])



############################################################
# Exports
############################################################

# Plot data
autoplot(data_ev[,c("exp","wg",'ssg','dexch')]) +
  ylab("% change") + xlab("Year")


# Scatterplot matrix of the variables
# First column shows the relationships between the forecast variable (exports) and each of the predictors
# The strength of these relationships are shown by the correlation coefficients across the first row
# NB: on the diagonal are the observed density of each variable
data_ev[,c("exp","wg",'ssg','dexch')] %>%
  as.data.frame() %>%
  GGally::ggpairs()

# So there is a clear positive correlation between exports growth and world growth
# That also looks positive with the variation of the exchange rate

# Possible models :
auto.arima(data_ev[,"exp"],xreg=data_ev[,c("wg")],allowmean=T)
auto.arima(data_ev[,"exp"],xreg=data_ev[,c("ssg")],allowmean=T)
auto.arima(data_ev[,"exp"],xreg=data_ev[,c("dexch")],allowmean=T)
auto.arima(data_ev[,"exp"],xreg=data_ev[,c("wg","dexch")],max.p=5,max.q=5,allowmean=T)
auto.arima(data_ev[,"exp"],xreg=data_ev[,c("ssg","dexch")],max.p=5,max.q=5,allowmean=T)

# Model with SSG and DEXCH has the lowest information criterions
# Plus, it is nice to ba ebla to take into acount both african demand and exchange rate
# in our exports forecast scenarios
# So we definitely choose this model
fit_exp <- auto.arima(data_ev[,"exp"],xreg=data_ev[,c("ssg","dexch")],max.p=5,max.q=5,allowmean=T)

# Check if models errors look like a white noise series
checkresiduals(fit_exp)
# That is reasonnably good


############################################################
# Imports
############################################################

# Plot data
autoplot(data_ev[,c("imp","doil",'dexch')]) +
  ylab("% change") + xlab("Year")

# Scatterplot matrix of the variables
data_ev[,c("imp","exp","doil",'dexch')] %>%
  as.data.frame() %>%
  GGally::ggpairs()

# Possible models 
auto.arima(data_ev[,"imp"],allowmean=T)
auto.arima(data_ev[,"imp"],xreg=data_ev[,c("doil")],allowmean=T)
auto.arima(data_ev[,"imp"],xreg=data_ev[,c("dexch")],allowmean=T)
auto.arima(data_ev[,"imp"],xreg=data_ev[,c("doil","dexch")],max.p=5,max.q=5,allowmean=T)
auto.arima(data_ev[,"imp"],xreg=data_ev[,c("exp","dexch")],max.p=5,max.q=5,allowmean=T)
auto.arima(data_ev[,"imp"],xreg=data_ev[,c("exp","doil")],max.p=5,max.q=5,allowmean=T)

# Model with exports and oil appears to hav the best fit
fit_imp <- auto.arima(data_ev[,"imp"],xreg=data_ev[,c("exp","doil")],max.p=5,max.q=5,allowmean=T)

# Check if models errors look like a white noise series
checkresiduals(fit_imp)


############################################################
# Total Investment (Gross fixed capital formation)
############################################################
autoplot(data_ev[,c("gfcf","dint","int")]) +
  ylab("% change") + xlab("Year")

# Scatterplot matrix of the variables
data_ev[,c("gfcf","int","dint")] %>%
  as.data.frame() %>%
  GGally::ggpairs()

# So the correlation with interest rate variation an with interest rate itself are really close
# Let's check the models

# Possible models 
auto.arima(data_ev[,"gfcf"],allowmean=T)
auto.arima(data_ev[,"gfcf"],xreg=data_ev[,c("dint")],allowmean=T)
auto.arima(data_ev[,"gfcf"],xreg=data_ev[,c("int")],allowmean=T)

# Here all models are really close according to information criterions but the last one is slightly better

fit_gfcf <- auto.arima(data_ev[,"gfcf"],xreg=data_ev[,c("int")],allowmean=T)

# Check if models errors look like a white noise series
checkresiduals(fit_gfcf)


############################################################
# Prices (GDP deflator)
############################################################
# Scatterplot matrix of the variables
data_ev[,c("dprice","int","doil","dexch")] %>%
  as.data.frame() %>%
  GGally::ggpairs()

auto.arima(data_ev[,"dprice"],xreg=data_ev[,c("int")],allowmean=T)
auto.arima(data_ev[,"dprice"],xreg=data_ev[,c("doil")],allowmean=T)
auto.arima(data_ev[,"dprice"],xreg=data_ev[,c("dexch")],allowmean=T)
auto.arima(data_ev[,"dprice"],xreg=data_ev[,c("int","dexch")],allowmean=T)

# Best model: ARMA(0,1) with real interest rate and exchange rate
fit_dprice <- auto.arima(data_ev[,"dprice"],xreg=data_ev[,c("int","dexch")],allowmean=T)

# Check if models errors look like a white noise series
checkresiduals(fit_dprice)


############################################################
# Household consumption
############################################################
autoplot(data_ev[,c("h_cons","dprice","int","dint")]) +
  ylab("% change") + xlab("Year")


# Scatterplot matrix of the variables
data_ev[,c("h_cons","dprice","int","dint")] %>%
  as.data.frame() %>%
  GGally::ggpairs()


# Possible models 
auto.arima(data_ev[,"h_cons"],allowmean=T)
auto.arima(data_ev[,"h_cons"],xreg=data_ev[,c("dint")],allowmean=T)
auto.arima(data_ev[,"h_cons"],xreg=data_ev[,c("dprice")],allowmean=T)
auto.arima(data_ev[,"h_cons"],xreg=data_ev[,c("dint","dprice")],allowmean=T)

fit_h_cons <- auto.arima(data_ev[,"h_cons"],xreg=data_ev[,c("dint","dprice")],allowmean=T)

# Check if models errors look like a white noise series
checkresiduals(fit_h_cons)


############################################################
# Other components
############################################################

# The remaining components are predicted with pure autoregressive equations
# (no additional data or economic assumptions)

fit_gov_cons <- auto.arima(data_ev[,"gov_cons"],allowmean=T) # AR1
fit_invent   <- auto.arima(data_ev[,"invent"],allowmean=T)
fit_other    <- auto.arima(data_ev[,"other"],allowmean=T)






############################################################
############################################################
# II - Forecasting
############################################################
############################################################

# prepare array for forecasts
data_ev_pred <- ts(array(NA,dim=c(end_pred-start_pred+1,dim(data_ev)[2])))
colnames(data_ev_pred) <- colnames(data_ev)
data_ev_pred <- ts(data_ev_pred,start=start_pred)


############################################################
# Exogenous variables
############################################################

# Some exogenous variable are conventionally considered constant in forecasting
# Oil prices, exchange rate, real interest rate
data_ev_pred[,c('dint','doil','dexch')] <- 0
data_ev_pred[,c('int')] <- tail(data_ev[,c('int')],n=1)

# Rest of the world real growth is forcasted with a simple ARIMA
data_ev_pred[,c('wg')] <- forecast(auto.arima(data_ev[,"wg"],allowmean=T),h=end_pred-start_pred+1)$mean
data_ev_pred[,c('ssg')] <- forecast(auto.arima(data_ev[,"ssg"],allowmean=T),h=end_pred-start_pred+1)$mean


############################################################
# Endogenous variables
############################################################

# NB: order is important here since there are some dependencies
# between variables

# Exports
f_exp <- forecast(fit_exp,h=end_pred-start_pred+1, xreg=data_ev_pred[,c("ssg","dexch")])
autoplot(f_exp) + xlab("Year") +   ylab("Percentage change")
data_ev_pred[,'exp'] <- f_exp$mean

# imports
f_imp <- forecast(fit_imp,h=end_pred-start_pred+1, xreg=data_ev_pred[,c("exp","doil")])
autoplot(f_imp) + xlab("Year") +   ylab("Percentage change")
data_ev_pred[,'imp'] <- f_imp$mean

# Gross fixed capital formation
f_gfcf <- forecast(fit_gfcf,h=end_pred-start_pred+1, xreg=data_ev_pred[,c("int")])
autoplot(f_gfcf) + xlab("Year") +   ylab("Percentage change")
data_ev_pred[,'gfcf'] <- f_gfcf$mean

# Prices
f_dprice <- forecast(fit_dprice,h=end_pred-start_pred+1, xreg=data_ev_pred[,c("int","dexch")])
autoplot(f_dprice) + xlab("Year") +   ylab("Percentage change")
data_ev_pred[,'dprice'] <- f_dprice$mean

# Household consumption
f_h_cons <- forecast(fit_h_cons,h=end_pred-start_pred+1, xreg=data_ev_pred[,c("dint","dprice")])
autoplot(f_h_cons) + xlab("Year") +   ylab("Percentage change")
data_ev_pred[,'h_cons'] <- f_h_cons$mean

# Other component
f_gov_cons   <- forecast(fit_gov_cons,h=end_pred-start_pred+1)
f_invent     <- forecast(fit_invent  ,h=end_pred-start_pred+1)
f_other      <- forecast(fit_other   ,h=end_pred-start_pred+1)
data_ev_pred[,'gov_cons'] <- f_gov_cons$mean
data_ev_pred[,'invent'] <- f_invent$mean
data_ev_pred[,'other'] <- f_other$mean



############################################################
# GDP forecasting
############################################################

# Now that we have forcasted each component, we can forecast real GDP growth
id_col <- c("h_cons","gov_cons","gfcf","invent","exp","imp")
data_ev_pred <- as_tibble(data_ev_pred)
data_ev <- as_tibble(data_ev)
data_ev_pred$Year <- (start_pred:end_pred)
data_pred <- data_ev_pred[,id_col]


data_ev <- bind_rows(data_ev,data_ev_pred)

print(data_ev,n=end_pred-start_data+1)
print(data,n=end_pred-start_data+1)

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

print(data_ev,n=end_pred-start_data+1)
print(data,n=end_pred-start_data+1)
