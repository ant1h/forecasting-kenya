## Forecasting the economy of Kenya with R

This repository contains a macroeconomic model I developped for the Kenyan economy with the [R language](https://www.r-project.org/).

I wrote a quick presentation page [here](https://ant1h.github.io/forecasting-kenya/).


### Files description

| File          | Description           |
| ------------- |-------------|
| utils.R     | Load required libraries and functions |
| data.R      | Load and prepare data    |
| baseline_model.R | Detailed estimation of a baseline model     |
| stat_model.R | Detailed estimation of a statistical model     |
| eco_model.R | Detailed estimation of an economic model     |
| model_function.R | Quick estimation of models in a function   |
| perf.R | Out-of-sample performance evaluation of models   |
| risks.R | Uncertainty evaluation around forecasts  |



### Data inputs

| File          | Description           |
| ------------- |-------------|
| un_gdp_constant.csv    | Kenya GDP components in constant prices from UN national accounts|
| ext_data.csv  | External data from UN, IMF, World bank   |