############################################################
# Functionnal forms for estimating models and forecasting
# 
############################################################


# NB: this file defines functions that estimates the models and forecast accordingly
# The functionnal forms is more convenient to do model comparison, to evaluate accuracy
# when changing parameters or estimation window


# i.e. this file does the same as baseline_model.R / stat_model.R / eco_model.R
# but with less details, analysis explorations, graphs
# and in a functionnal form "f(input) returns outputs"


# Baseline model estimation
# Inputs
#   data: GDP data by expenditure component
#   exdat: external data (see: 'data.R' file)
#   wind: time window chosen by user (start od data / end of data / start of pred / end of pred)


bmodel <- function(data,exdata,wind){
  
  # Estimation and forecast windows
  start_data <- wind[1]
  end_data   <- wind[2]
  start_pred <- wind[3]
  end_pred   <- wind[4]
  
  # GDP serie in 2010 constant prices (UN data)
  gdp <- ts(data=data$gdp,
            start=start_data,
            end=end_data)

  # Take delta logs of gdp
  ddata <- evol_d(data)
  dgdp  <- diff(log(gdp))
  
  # Estimate autoregressive model
  baseline_model <- arima(dgdp, c(1, 0, 0))
  
  # Forecast GDP
  fc <- forecast(baseline_model,level=c(int1,int2),h=end_pred-start_pred+1,bootstrap=T)
  
  # Put forecasts in ddata
  ddata <- bind_rows(ddata,ddata[1:(end_pred-start_pred+1),]*NA) #expand ddata to forecast window with NA
  ddata$Year <-  (start_data+1):end_pred # fill Year with forecast window
  ddata[ddata$Year>end_data,]$gdp  <- fc$mean # fill with GDP forecast
  
  # Compute corresponding levels in data
  id_col <- colnames(data)[colnames(data) != 'Year']
  data <- bind_rows(data,data[1:(end_pred-start_pred+1),]*NA) #expand ddata to forecast window with NA
  data$Year <-  (start_data):end_pred # fill Year with forecast window
  for (y in start_pred:end_pred){
    data[data$Year==y,id_col] <- exp( ddata[ddata$Year==y,id_col] + log(data[data$Year==(y-1),id_col]) )
  }
  
  

  # Prepare output
  L <- NULL # empty list
  
  L[[1]]  <-  baseline_model  # model
  #L[[2]]  <-  fc              # GDP growth forcast with confidence intervals
  L[[2]]  <-  data            # data with forecats
  L[[3]]  <-  ddata           # ddata with forecasts
  
  return(L)
  
}


# Statistical model estimation
# Inputs
#   data: GDP data by expenditure component
#   exdat: external data (see: 'data.R' file)
#   wind: time window chosen by user (start od data / end of data / start of pred / end of pred)


smodel <- function(data,exdata,wind){
  
  # Estimation and forecast windows
  start_data <- wind[1]
  end_data   <- wind[2]
  start_pred <- wind[3]
  end_pred   <- wind[4]
  pred_len   <- end_pred - start_pred + 1

  # Take delta logs of GDP series
  ddata <- evol_d(data)
  
  
  # Estimate ARIMA models for each expenditure of GDP
  # (We build a list of models for each component)
  id_col <- c("h_cons","gov_cons","gfcf","invent","exp","imp") # variables for wich we want to estimate a model
  
  models <- lapply(id_col,function(i){
    mod <- auto.arima(ddata[,i],
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
  
  
  
  # Forecast each GDP component and then GDP
  # Forecast each evolution rate base on the best ARIMA model identified
  
  fc <- lapply(id_col,function(i){
    f <- forecast(models[[which(id_col==i)]],pred_len)
    return(f)
  })  
  
  
  # Add expenditures forecasts in ddata
  M <- NULL
  for (f in fc){
    M <- as.data.frame(cbind(M,c(f$mean)))
  }
  colnames(M) <- id_col
  M$Year <- c(start_pred:end_pred)
  # Add forecast to ddata
  ddata <- bind_rows(ddata,M)
  
  
  
  # Forecast evolution rate of GDP
  for (y in start_pred:end_pred){
    exp_rates  <- ddata[ddata$Year==y,id_col] # forecast for each expenditure for year y
    exp_weight <- data[data$Year==(y-1),id_col] / as.numeric(data[data$Year==(y-1),'gdp']) # rate of component in GDP
    exp_weight['imp'] <- -exp_weight['imp'] # Imports contributes negatively
    ddata[ddata$Year==y,'gdp'] <- sum(exp_rates*exp_weight) # compute GDP evolution rate forcast
    
    # Now we can infer levels for year y before doing year y+1
    levels_y <-exp(ddata[ddata$Year==y,c(id_col,'gdp')] + log(data[data$Year==(y-1),c(id_col,'gdp')]))
    data <- bind_rows(data,c(Year=y,levels_y))
  }
  
  print(data,n=dim(data)[1])
  print(ddata,n=dim(ddata)[1])
  
 
 # Prepare output
 L <- NULL # empty list
 
 L[[1]]  <-  models # model
 #L[[2]]  <-  fc              # GDP growth forcast with confidence intervals
 L[[2]]  <-  data            # data with forecats
 L[[3]]  <-  ddata           # ddata with forecasts
  
  return(L)
  
}


# Economic model estimation
# Inputs
#   data: GDP data by expenditure component
#   exdat: external data (see: 'data.R' file)
#   wind: time window chosen by user (start od data / end of data / start of pred / end of pred)


emodel <- function(data,exdata,wind){
  
  # Estimation and forecast windows
  start_data <- wind[1]
  end_data   <- wind[2]
  start_pred <- wind[3]
  end_pred   <- wind[4]
  pred_len   <- end_pred - start_pred + 1
  
  
  # Take delta logs of gdp components
  ddata <- evol_d(data)
    # Let's add external series inr our working dataset
  ddata <- data.frame( (cbind(ddata,exdata[-1,c('wg','ssg','dint','doil','dexch','dprice','int')])))
    # Convert to time series format
  ddata <- ts(ddata,start=ddata[1,'Year'])
  
  
  # Estimate equations
  # NB: detailed analysis and explainations on modelling choices can be found in eco_model.T
  #   Here we directly estimate chosen equations
  fit_exp       <- auto.arima(ddata[,"exp"],xreg=ddata[,c("ssg","dexch")],max.p=5,max.q=5,allowmean=T)
  fit_imp       <- auto.arima(ddata[,"imp"],xreg=ddata[,c("exp","doil")],max.p=5,max.q=5,allowmean=T)

  fit_gfcf      <- auto.arima(ddata[,"gfcf"],xreg=ddata[,c("int")],allowmean=T)
  
  fit_dprice    <- auto.arima(ddata[,"dprice"],xreg=ddata[,c("int","dexch")],allowmean=T)
  fit_h_cons    <- auto.arima(ddata[,"h_cons"],xreg=ddata[,c("dint","dprice")],allowmean=T)
  fit_gov_cons  <- auto.arima(ddata[,"gov_cons"],allowmean=T) # AR1
  fit_invent    <- auto.arima(ddata[,"invent"],allowmean=T)
  fit_other     <- auto.arima(ddata[,"other"],allowmean=T)
  
  
  
  
  # Give proper serie name
  fit_exp$series       <- 'exp'
  fit_imp$series       <- 'imp'
  fit_gfcf$series      <- 'gfcf'
  fit_dprice$series    <- 'dprice'
  fit_h_cons$series    <- 'h_cons'
  fit_gov_cons$series  <- 'gov_cons'
  fit_invent$series    <- 'invent'
  fit_other$series     <- 'other'
  
  
  
  # Forecasting
  
  # prepare array for forecasts
  ddata_pred <- ts(array(NA,dim=c(end_pred-start_pred+1,dim(ddata)[2])))
  colnames(ddata_pred) <- colnames(ddata)
  ddata_pred <- ts(ddata_pred,start=start_pred)
  
  
  # Some exogenous variable are conventionally considered constant in forecasting
  # Oil prices, exchange rate, real interest rate
  ddata_pred[,c('dint','doil','dexch')] <- 0
  ddata_pred[,c('int')] <- tail(ddata[,c('int')],n=1)
  # Rest of the world real growth is forcasted with a simple ARIMA
  ddata_pred[,c('wg')] <- forecast(auto.arima(ddata[,"wg"],allowmean=T),h=end_pred-start_pred+1)$mean
  ddata_pred[,c('ssg')] <- forecast(auto.arima(ddata[,"ssg"],allowmean=T),h=end_pred-start_pred+1)$mean
  
  
  
  
  # Endogenous variables
  # Exports
  f_exp <- forecast(fit_exp,h=end_pred-start_pred+1, xreg=ddata_pred[,c("ssg","dexch")])
  ddata_pred[,'exp'] <- f_exp$mean
  # imports
  f_imp <- forecast(fit_imp,h=end_pred-start_pred+1, xreg=ddata_pred[,c("exp","doil")])
  ddata_pred[,'imp'] <- f_imp$mean
  # Gross fixed capital formation
  f_gfcf <- forecast(fit_gfcf,h=end_pred-start_pred+1, xreg=ddata_pred[,c("int")])
  ddata_pred[,'gfcf'] <- f_gfcf$mean
  # Prices
  f_dprice <- forecast(fit_dprice,h=end_pred-start_pred+1, xreg=ddata_pred[,c("int","dexch")])
  ddata_pred[,'dprice'] <- f_dprice$mean
  # Household consumption
  f_h_cons <- forecast(fit_h_cons,h=end_pred-start_pred+1, xreg=ddata_pred[,c("dint","dprice")])
  ddata_pred[,'h_cons'] <- f_h_cons$mean
  # Other component
  f_gov_cons   <- forecast(fit_gov_cons,h=end_pred-start_pred+1)
  f_invent     <- forecast(fit_invent  ,h=end_pred-start_pred+1)
  f_other      <- forecast(fit_other   ,h=end_pred-start_pred+1)
  ddata_pred[,'gov_cons'] <- f_gov_cons$mean
  ddata_pred[,'invent'] <- f_invent$mean
  ddata_pred[,'other'] <- f_other$mean
  
  
  
  # GDP forecasting
  
  # Now that we have forcasted each component, we can forecast real GDP growth
  id_col <- c("h_cons","gov_cons","gfcf","invent","exp","imp")
  ddata_pred <- as_tibble(ddata_pred)
  ddata <- as_tibble(ddata)
  ddata_pred$Year <- (start_pred:end_pred)
  data_pred <- ddata_pred[,id_col]
  
  
  ddata <- bind_rows(ddata,ddata_pred)
  
  print(ddata,n=end_pred-start_data+1)
  print(data,n=end_pred-start_data+1)
  
  # Forecast evolution rate of GDP
  for (y in start_pred:end_pred){
    exp_rates  <- ddata[ddata$Year==y,id_col] # forecast for each expenditure for year y
    exp_weight <- data[data$Year==(y-1),id_col] / as.numeric(data[data$Year==(y-1),'gdp']) # rate of component in GDP
    exp_weight['imp'] <- -exp_weight['imp'] # Imports contributes negatively
    ddata[ddata$Year==y,'gdp'] <- sum(exp_rates*exp_weight) # compute GDP evolution rate forcast
    
    # Now we can infer levels for year y before doing year y+1
    levels_y <-exp(ddata[ddata$Year==y,c(id_col,'gdp')] + log(data[data$Year==(y-1),c(id_col,'gdp')]))
    data <- bind_rows(data,c(Year=y,levels_y))
  }
  
  
  
  # Compute GDP forecast confidence interval 
  #   from expenditures confidence intervals
  
  fc <- f_exp #TO DO
  
  
  
  
  # List of models
  models <- list(
    fit_exp     ,
    fit_imp     ,
    fit_gfcf    ,
    fit_dprice  ,
    fit_h_cons  ,
    fit_gov_cons,
    fit_invent  ,
    fit_other   
  )
  
  
  
  # Prepare output
  L <- NULL # empty list
  
  L[[1]]  <-  models  # model
  #L[[2]]  <-  fc              # GDP growth forcast with confidence intervals
  L[[2]]  <-  data            # data with forecats
  L[[3]]  <-  ddata           # ddata with forecasts
  
  return(L)
  
}


#print(ddata,n=dim(ddata)[1])
#print(data,n=dim(data)[1])
#rdata <- data


#autoplot(ts(data$gdp,start=start_data)) + geom_vline(xintercept = start_pred-0.5, color='blue')
#autoplot(ts(ddata$gdp,start=start_data)) + geom_vline(xintercept = start_pred-0.5, color='blue')


