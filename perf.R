# Evaluation and comparison of forecasting performances of our models



# Choose estimation/forecasting windows
n_year <- 3 # number of year used for forecasting accuracy evaluation
start_eval <- 2006 # starting year for evaluation
end_eval   <- window[2] - n_year +1 # until the end of available data

# prepare a data frame to store metrics
perf <- NULL
for (y in start_eval:end_eval){
  
  # define window
  w <- c(window[1],y-1,y,y+n_year-1)
  
  # estimate each model on window
  bmod <-       bmodel(data[data$Year %in% w[1]:w[2],], exdata[exdata$Year %in% w[1]:w[2],], w)
  smod <- quiet(smodel(data[data$Year %in% w[1]:w[2],], exdata[exdata$Year %in% w[1]:w[2],], w))
  emod <- quiet(emodel(data[data$Year %in% w[1]:w[2],], exdata[exdata$Year %in% w[1]:w[2],], w))
  
  # compute metrics and fill in perf data frame
  obs <- data_ev[data_ev$Year %in% w[3]:w[4],]$gdp # actual data
  nrow <- 3*3 # 3 models / 3 metrics
  M <- cbind(rep(y,nrow),
             c(rep('baseline',3), rep('statistical',3), rep('economic',3)),
             c(rep(c('bias','variance','mse'),3)),
             c(pred_met(bmod[[3]][bmod[[3]]$Year %in% w[3]:w[4],]$gdp,obs),
               pred_met(smod[[3]][smod[[3]]$Year %in% w[3]:w[4],]$gdp,obs),
               pred_met(emod[[3]][emod[[3]]$Year %in% w[3]:w[4],]$gdp,obs))
  )
  perf <- rbind(perf,M)
}

perf <- data.frame(perf,stringsAsFactors = F)
colnames(perf) <- c('year','model','metric','value')
perf$value <- as.numeric(as.character(perf$value))



# We can now compute mean metrics over years for each model/metric
mperf <- aggregate(value ~ model + metric, data=perf, FUN=mean)


# A nice barplot would be great
pal <- wes_palette("Zissou1",3, type = "continuous")
p1<-ggplot(data=mperf[mperf$metric=='bias',], aes(x=model, y=value)) +
  geom_bar(stat="identity", fill=pal)+
  labs(y="",x="Model")+
  theme_minimal()

p2<-ggplot(data=mperf[mperf$metric=='variance',], aes(x=model, y=value)) +
  geom_bar(stat="identity", fill=pal)+
  labs(y="",x="Model")+
  theme_minimal()

p3<-ggplot(data=mperf[mperf$metric=='mse',], aes(x=model, y=value)) +
  geom_bar(stat="identity", fill=pal)+
  labs(y="",x="Model")+
  theme_minimal()


# plot_grid(p1, p2, p3, labels=c("Bias", "Variance","MSE"), ncol = 3, nrow = 1)


# so here we illustrate a crual but well-known conclusion
# simplicity is difficult to be in macroeconomic forecast

# it can be seen as a trad-off between interpretability and performance
# So if 


#
#print(smod[[2]],n=dim(smod[[2]])[1])
#print(smod[[3]],n=dim(smod[[3]])[1])

