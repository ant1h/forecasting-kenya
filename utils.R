# Required Libraries
library(tseries)
library(forecast)
library(urca)
library(strucchange)
library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(wesanderson)
library(vctrs)
library(GGally)



# Specific functions


# Compute growth rate of a vector
evol <- function(VECT){#numerical vector
  return(diff(VECT)/VECT[-length(VECT)])
}



# Compute delta-log of a dataframe
evol_d <- function(mat){
  M <- data.frame(lapply(mat, function(x) {diff(log(x))}))
  M$Year <- mat$Year[-1]
  return(as_tibble(M))
}



# Compute contibution of sub-aggregate to delta-log of aggregate (ex: GDP)
contrib <- function(mat,agg){
  # mat: data.frame of sub-aggregates
  # agg: string indicating which conames of mat is the aggregate
  M <- data.frame(lapply(mat, function(x) {diff(log(x))}))
  for (col in colnames(mat)){
    M[col] <- M[col] * head(mat[col]/mat[agg],-1)
  }
  M$Year <- mat$Year[-1]
  return(as_tibble(M))
}

#writeClipboard(contrib(data_uncons,"gdp"))





# Format a number as a percentage
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}




# Quiet function
# see: http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 



# Prediciton metrics
# Compute forecast bias, variance and MSE of a prediction
pred_met <- function(pred,obs){
  
  m <- mean(pred - obs) #bias
  v <- mean( ((pred- obs) - m )^2) # variance
  mse <- m^2 + v
  
  return(c(m,v,mse))
}




# A usefuul function to copy data.frame in clipboard
# And then Ctrl+V in Excel for exemple

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}



