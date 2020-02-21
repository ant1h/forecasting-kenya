
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Evaluation of macro fiscal risks for Kenya
# Data loading and preprocessing
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


################################################################
# Load UN data from CSV file
# GDP by expenditure in constant prices (in Kenyan Shillings)
# Download CSV file: https://unstats.un.org/unsd/snaama/CountryProfile?ccode=404
################################################################

# Load data from CSV
data <- data.frame((read.csv("un_gdp_constant.csv",
                                   fill=TRUE,
                                   blank.lines.skip=TRUE,
                                   sep=";",
                                   stringsAsFactors = FALSE
      )),
      stringsAsFactors = FALSE)

# Here we will use the tidy data framework
# see: https://tidyr.tidyverse.org/
# store data in a tibble
data <- as_tibble(data)



# Variables names
# Let's simplify a bit the name of our variables here
simplified_ids <- c(
  "Year",         #"Year"                                                                                    
  "tot_cons",     #"Final.consumption.expenditure"                                                           
  "h_cons",     #"Household.consumption.expenditure..including.Non.profit.institutions.serving.households."
  "gov_cons",      #"General.government.final.consumption.expenditure"                                        
  "gcf",      #"Gross.capital.formation"                                                                 
  "gfcf",      #"Gross.fixed.capital.formation..including.Acquisitions.less.disposals.of.valuables."      
  "invent",      #"Changes.in.inventories"                                                                  
  "exp",      # "Exports.of.goods.and.services"                                                           
  "imp",      # "Imports.of.goods.and.services"                                                           
  "gdp"      # "Gross.Domestic.Product..GDP."  
)


# Simplify variables ids
colnames(data) <- simplified_ids

# Test that sums are correct (in constant prices) in 2010 (base year)
# Total final concumption is households' + government's
abs((data$h_cons + data$gov_cons - data$tot_cons)[])<2
# Gross capital formation is invesment + change in inventories
abs((data$gfcf + data$invent - data$gcf)[])<2
# GDP is C + I + X - M
abs(data$tot_cons + data$gcf + data$exp - data$imp - data$gdp)<2



# We see that there exist a significant resudual amonf GDP expenditures
# It dos not look to be explained by chained prices
# Might be working days correction, Non-profit institutions serving households
# Anyway, the best way to deal with it might be to creat a "other GDP expenditure line"
data$other <- data$gdp - (data$tot_cons + data$gcf + data$exp - data$imp)




################################################################
# Load additionnal data from CSV file

# We prepare a CSV file with additional time series from various sources

# KSH exchange rate: https://unstats.un.org/unsd/snaama/CountryProfile?ccode=404

# Real interest rate: https://data.worldbank.org/indicator/FR.INR.RINR?locations=KE
#   [Real interest rate is the lending interest rate adjusted for inflation as measured by the GDP deflator]

# Average crude oil prices (from World Bank Commodity Price Data (The Pink Sheet))
#   https://www.worldbank.org/en/research/commodity-markets

# Gross disposable income
#   http://data.un.org/Data.aspx?q=kenya&d=SNA&f=group_code%3a103%3bcountry_code%3a404

# World growth and sub-saharan Africa growth (World bank)

# Deflator of GDP (as general prices)
#   https://unstats.un.org/unsd/snaama/CountryProfile?ccode=404

################################################################


# Load data from CSV
exdata <- data.frame((read.csv("ext_data.csv",
                             fill=TRUE,
                             blank.lines.skip=TRUE,
                             sep=";",
                             stringsAsFactors = FALSE
)),
stringsAsFactors = FALSE)


# rename columns to simple names
colnames(exdata) <- c('Year',
                      'exch',
                      'int',
                      'wg',
                      'oil',
                      'ssg',
                      'price')

# store data in a tibble
exdata <- as_tibble(exdata)


# Convert world growth to a percentage
exdata$wg <- exdata$wg/100
exdata$ssg <- exdata$ssg/100
exdata$int <- exdata$int/100

# compute evolution rate of exch / int / oil / gdi
# world growth is already an evolution rate
exdata$dexch <- c(NA,evol(exdata$exch))
exdata$dint  <- c(NA,diff(exdata$int))
exdata$doil  <- c(NA,evol(exdata$oil))
exdata$dprice  <- c(NA,evol(exdata$price))


# Remove potential blank lines
exdata <- exdata[!is.na(exdata$Year),]




################################################################
# Some basic data preparation
################################################################

# Compute evolution rates (as delta-log)
data_ev <- evol_d(data)

# Compute for each expenditure the contribution to the evolution of GDP
data_contrib <- contrib(data,'gdp')
data_contrib[,'imp'] <- -data_contrib[,'imp'] # NB: imports growth contributes negatively to GDP
data_contrib$other <- data_contrib$gdp -( # Compute the contribution of "others" as a residual
  data_contrib$h_cons +
    data_contrib$gov_cons +
    data_contrib$gcf +
    data_contrib$exp +
    data_contrib$imp)





# A few plots to understand data


# Plot a specific serie (ex here: exports)
g <- ggplot(data, aes(x=Year, y=exp)) + geom_line()
g <- g + labs(title="Exports",
              subtitle="In 2010 constant prices, Kenyan Shillings",
              y="Exports",
              x="Time",
              caption="UN National Accounts Database")


# Plot evolution rate (ex: exports) with long-term mean
g <- ggplot(data_ev, aes(x=Year, y=exp)) + geom_line() #+ geom_hline(yintercept = mean(data_ev$exp), color="blue")
g <- g + geom_hline(aes(yintercept=mean(exp)),color='blue') +
  geom_text(aes(data$Year[1],mean(exp),label = percent(mean(exp)), vjust = -1))
g <- g + labs(title="Exports",
              subtitle="In 2010 constant prices, Kenyan Shillings",
              y="Exports",
              x="Time",
              caption="UN National Accounts Database")
g <- g + scale_y_continuous(labels=scales::percent)






# Plot GDP by expenditures over time
id_contrib <- colnames(data)[!(colnames(data) %in% c('Year','gdp','other'))] # list of variables for wich we want to plot contribution to gdp
pal <- wes_palette("Zissou1",length(id_contrib), type = "continuous")
data_g <- pivot_longer(data,cols=id_contrib,names_to = "Expenditures")

data_g[data_g$Expenditures=='imp',]$value <- -data_g[data_g$Expenditures=='imp',]$value

g <- ggplot(data_g, aes(fill=Expenditures, y=value, x=Year)) + 
  geom_bar(position="stack", stat="identity")
g <- g + geom_line(aes(x=Year, y=gdp))
g <- g + labs(title="GDP composition by expentidures",
              subtitle="In 2010 constant prices, Kenyan Shillings",
              y="Kenyan Shillings",
              x="Time",
              caption="UN National Accounts Database")
g <- g +scale_fill_manual(values=pal)


gdpbyexp_plot <- g



# Plot contribution to GDP over time
# NB: for ggplot2 we need to reshape data from wide to long using pivot_longer
id_contrib <- colnames(data_contrib)[!(colnames(data_contrib) %in% c('Year','gdp','other','gcf','tot_cons'))] # list of variables for wich we want to plot contribution to gdp
pal <- wes_palette("Zissou1",length(id_contrib), type = "continuous")
data_g <- pivot_longer(data_contrib,cols=id_contrib,names_to = "Expenditures")

g <- ggplot(data_g, aes(fill=Expenditures, y=value, x=Year)) + 
  geom_bar(position="stack", stat="identity")
g <- g + geom_line(aes(x=Year, y=gdp))
g <- g + labs(title="Contribution to GDP growth by expentidures",
              subtitle="In 2010 constant prices, Kenyan Shillings",
              y="Contributions",
              x="Time",
              caption="UN National Accounts Database")
g <- g + scale_y_continuous(labels=scales::percent)
g <- g +scale_fill_manual(values=pal)

contribgdp_plot <- g

