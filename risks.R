
# Quantification of risks



# Risks quantification for the statistical model
# Input: 
#   a model output from smodel
#   a window
#   N: number of simulations used to estimate risks probabilities
# Output:
#   a 3 dimensionnal array with confidence intervals
#   serie (GDP, export,...)   x   years of forecast  x   intervals
risk_stat <- function(model,wind,N){
  
  # Estimation and forecast windows
  start_data <- wind[1]
  end_data   <- wind[2]
  start_pred <- wind[3]
  end_pred   <- wind[4]
  pred_len   <- end_pred - start_pred + 1
  
  models <- model[[1]]
  data <- model[[2]]
  ddata <- model[[3]]
  id_col <- c("h_cons","gov_cons","gfcf","invent","exp","imp") # variables for wich we want to estimate a model
  
  
  # First we need to have each component weight in GDP dynamically
  # This means that weight are updated each year of the forecast
  dw <- NULL
  for (i in id_col){
    we <- (data[data$Year %in% start_pred:end_pred,i] / data[data$Year %in% start_pred:end_pred,'gdp'])
    dw <- as.data.frame(cbind(dw,as.numeric(unlist(we))))
  }
  colnames(dw) <- id_col
  dw['imp'] <- -dw['imp']
  #rowSums(dw)
  
  
  # Now we do N simulations of GDP expenditures
  # N=1000
  gdp_sim <- NULL
  for (i in 1:N){
    simu <- NULL
    for (mod in models){
      s <- simulate(mod,future=T,nsim=pred_len)
      simu <- cbind(simu,s)
    }
    colnames(simu) <- id_col
    gdp_sim = rbind(gdp_sim,c('gdp',rowSums(data.frame(simu)*dw,na.rm=T)))
    gdp_sim = rbind(gdp_sim, unname(cbind(colnames(simu),t(simu))))
  }
  colnames(gdp_sim) <- c('serie',as.character(start_pred:end_pred))
  
  
  r <- as_tibble(gdp_sim[,c('serie',as.character(start_pred))])
  #r <- r[!(r$serie %in% c('imp','invent')),]
  colnames(r) <- c('serie','forecast')
  r$forecast <- as.numeric(r$forecast)
  
  plot_dens <- ggplot(r, aes(forecast, fill = serie, colour = serie)) +
    geom_density(alpha = 0.1)
  
  # Now we can keep only GDP lines
  gdp_sim = gdp_sim[gdp_sim[,'serie']=='gdp',-1]
  
  # Estimate distribution of risks with the N simulations
  conf_int <- NULL
  for (y in colnames(gdp_sim)){
    p <- as.numeric(gdp_sim[,y])
    conf_int <- rbind(conf_int,quantile(p, prob=c(0.5,1-int2/100,1-int1/100,int1/100, int2/100)))
  }
  rownames(conf_int) <- colnames(gdp_sim)
  
  
  
  # Finaly we add all confidence intervals for each GDP expenditure
  # And store them in a 3 dimensionnal array
  L <- array(NA,dim=c(length(id_col)+1,dim(conf_int)))
  dimnames(L) <- list(c('gdp',id_col),
                      dimnames(conf_int)[[1]],
                      dimnames(conf_int)[[2]])
  
  L['gdp',,] <- conf_int
  
  for (id in id_col){
    i = which(id_col==id)
    fc <- forecast(model[[1]][[i]],h=end_pred-start_pred+1,level=c(int1,int2))
    L[i+1,,] <- cbind(fc$mean,fc$lower,fc$upper)    
  }
  

  output=list(L,plot_dens)
  return(output)
}





# Risks quantification for the economic model
# Input: 
#   a model output from emodel
#   a window
#   N: number of simulations used to estimate risks probabilities
# Output:
#   a 3 dimensionnal array with confidence intervals
#   serie (GDP, export,...)   x   years of forecast  x   intervals
risk_eco <- function(model,wind,N){
  
  # Estimation and forecast windows
  start_data <- wind[1]
  end_data   <- wind[2]
  start_pred <- wind[3]
  end_pred   <- wind[4]
  pred_len   <- end_pred - start_pred + 1
  
  models <- model[[1]]
  data <- model[[2]]
  ddata <- model[[3]]
  
  # delete unwanted models (dprice, other)
  to_del=c()
  for (i in 1:length(models)){
    if (models[[i]]$series %in% c('other','dprice')){
      to_del <- c(to_del,i)
    }
  }
  fit_dprice <- models[[to_del[1]]]
  models[[to_del[1]]]<-NULL
  models[[to_del[2]-1]]<-NULL
  
  
  # Get columns names from models in correct order
  id_col <- c()
  for (i in 1:length(models)){
    id_col <- c(id_col, models[[i]]$series)
  }
  
  
  
  # First we need to have each component weight in GDP dynamically
  # This means that weight are updated each year of the forecast
  dw <- NULL
  for (i in id_col){
    we <- (data[data$Year %in% start_pred:end_pred,i] / data[data$Year %in% start_pred:end_pred,'gdp'])
    dw <- as.data.frame(cbind(dw,as.numeric(unlist(we))))
  }
  colnames(dw) <- id_col
  dw['imp'] <- -dw['imp']
  #rowSums(dw)
  
  
  
  # Then we need to forecast all exogenous vaiables
  # prepare array for forecasts
  exd <- ts(array(NA,dim=c(end_pred-start_pred+1,dim(exdata)[2])))
  colnames(exd) <- colnames(exdata)
  exd <- ts(exd,start=start_pred)
  # Some exogenous variable are conventionally considered constant in forecasting
  # Oil prices, exchange rate, real interest rate
  exd[,c('dint','doil','dexch')] <- 0
  exd[,c('int')] <- rep(as.numeric(tail(exdata[,c('int')],n=1)),end_pred-start_pred+1)
  # Rest of the world real growth is forcasted with a simple ARIMA
  exd[,c('wg')]  <- forecast(auto.arima(exdata[,"wg"],allowmean=T),h=end_pred-start_pred+1)$mean
  exd[,c('ssg')] <- forecast(auto.arima(exdata[,"ssg"],allowmean=T),h=end_pred-start_pred+1)$mean
  # Prices 
  exd[,c('dprice')] <- forecast(fit_dprice,h=end_pred-start_pred+1, xreg=as.matrix(exd[,c("int","dexch")]) )$mean
  # We will also need to add exports forecast (to simulate imports)
  exd <- cbind(exd,exp=0,xreg=exd[,c('int')])
  colnames(exd) <- c(colnames(exdata),'exp','xreg')
  
  
  
  
  # Now we do N simulations of GDP expenditures
  # N=1000
  gdp_sim <- NULL
  for (i in 1:N){
    simu <- NULL
    for (mod in models){
      s <- simulate(mod,future=T,nsim=pred_len,xreg=exd[,colnames(mod$xreg)])
      simu <- cbind(simu,s)
      
      if (mod$series=='exp' ){ # NB: we need to add exports to exd to simulate imports
        exd[,'exp'] <- s
      }
    }
    colnames(simu) <- id_col
    gdp_sim = rbind(gdp_sim,c('gdp',rowSums(data.frame(simu)*dw,na.rm=T)))
    gdp_sim = rbind(gdp_sim, unname(cbind(colnames(simu),t(simu))))
  }
  colnames(gdp_sim) <- c('serie',as.character(start_pred:end_pred))
  
  
  r <- as_tibble(gdp_sim[,c('serie',as.character(start_pred))])
  #r <- r[!(r$serie %in% c('imp','invent')),]
  colnames(r) <- c('serie','forecast')
  r$forecast <- as.numeric(r$forecast)
  
  plot_dens <- ggplot(r, aes(forecast, fill = serie, colour = serie)) +
    geom_density(alpha = 0.1)
  
  # Now we can keep only GDP lines
  gdp_sim = gdp_sim[gdp_sim[,'serie']=='gdp',-1]
  
  # Estimate distribution of risks with the N simulations
  conf_int <- NULL
  for (y in colnames(gdp_sim)){
    p <- as.numeric(gdp_sim[,y])
    conf_int <- rbind(conf_int,quantile(p, prob=c(0.5,1-int2/100,1-int1/100,int1/100, int2/100)))
  }
  rownames(conf_int) <- colnames(gdp_sim)
  
  
  
  # Finaly we add all confidence intervals for each GDP expenditure
  # And store them in a 3 dimensionnal array
  L <- array(NA,dim=c(length(id_col)+1,dim(conf_int)))
  dimnames(L) <- list(c('gdp',id_col),
                      dimnames(conf_int)[[1]],
                      dimnames(conf_int)[[2]])
  
  L['gdp',,] <- conf_int
  

  
  for (mod in models){
    id <- mod$series
    xregs = exd[,colnames(mod$xreg)]
    fc <- forecast(mod,h=end_pred-start_pred+1,level=c(int1,int2),xreg=xregs)
    
    L[which(id_col==id)+1,,] <- cbind(fc$mean,fc$lower,fc$upper)
    
    if (mod$series=='exp' ){ # NB: we need to add exports to exd to simulate imports
      exd[,'exp'] <- fc$mean
    }
  }
  
  
  output=list(L,plot_dens)
  return(output)
}


