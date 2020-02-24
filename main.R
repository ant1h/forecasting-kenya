# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Evaluation of macro fiscal risks for Kenya
# Main program
# Antoine Herlin - 09/02/2020
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load packages and specific functions required
source('utils.R')
# Prepare, organize and pre-process data
source('data.R')

# Choose end of forecast year
end_pred   <- 2030 # user choice for end of forecast
window=c(min(data$Year),max(data$Year),max(data$Year)+1,end_pred)
pred_len <- (end_pred - max(data$Year))

# Uncertainty parameters
int1 <- 80 # first confidence interval  (in %)
int2 <- 95 # second confidence interval (in %)





# Models estimation
source('model_functions.R')

# Estimate the baseline model
baseline_model <- bmodel(data,exdata,window)
baseline_model
print(baseline_model[[3]],n=dim(baseline_model[[3]])[1])
autoplot(ts(baseline_model[[3]]$gdp,start=window[1])) +
  geom_vline(xintercept = window[3]-1.5, color='blue') +
  annotate("text", x=window[3]+3, y=max(baseline_model[[3]]$gdp), label= "Forecast", color='blue',size=5)

# Estimate the statistical model
stat_model <- quiet(smodel(data,exdata,window))
print(stat_model[[3]],n=dim(stat_model[[3]])[1])
autoplot(ts(stat_model[[3]]$gdp,start=window[1])) +
  geom_vline(xintercept = window[3]-1.5, color='blue') +
  annotate("text", x=window[3]+3, y=max(stat_model[[3]]$gdp), label= "Forecast", color='blue',size=5)


# Estimate the economic model
eco_model <- quiet(emodel(data,exdata,window))
print(eco_model[[3]],n=dim(eco_model[[3]])[1])
autoplot(ts(eco_model[[3]]$gdp,start=window[1])) +
  geom_vline(xintercept = window[3]-1.5, color='blue') +
  annotate("text", x=window[3]+3, y=max(eco_model[[3]]$gdp), label= "Forecast", color='blue',size=5)




# Performance evaluation
# see: in 'perf.R' file


# Quantification of risks
source('risks.R')


# From eco model
r <- risk_eco(eco_model,window,100) # estimate forecast uncertainty from 1000 simulations
  # Density plot (froms simulations)
plot(r[[2]])
  # barplot of quantiles
start_pred=window[3]
br <- as_tibble(r[[1]][,as.character(start_pred),]) # keep only start_pred
br <- bind_cols(serie=dimnames(r[[1]])[[1]],br) # add dimnames
dta <- eco_model[[2]]
br[,-1] <- br[,-1] * t((dta[dta$Year==start_pred,br$serie]/as.numeric(dta[dta$Year==start_pred,'gdp']))) # multiply by weight in gdp
for (i in 3:6){br[,i]<-br[,i]-br[,2]}
br <- select(br,-2) # get rid of 50%
br <- gather(br,key='level',value='risk',-serie)
br[br$serie=='imp','risk']<- -br[br$serie=='imp','risk']
br$level <- factor(br$level,ordered = T,levels=c('95%','80%','5%','20%'))
br$serie <- factor(br$serie,ordered = T,levels=c('gdp','exp','imp','h_cons','gov_cons','gfcf','invent'))
br$risk_lab <- 100*round(br$risk,3)
ggplot(data=br, aes(x=serie, y=risk, fill=level)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(y=(risk), label=risk_lab), vjust=1.6,color="white", size=3.5)+
  labs(title = "Prediction intervals for real GDP and contribution of GDP expenditures",
       y='Prediction intervals',
       subtitle= "for Y+1 forecast (2019)")+
  theme_minimal()

