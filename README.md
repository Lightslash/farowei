# What is all about ? plot and play - data and forecasting 

This is an open source **Community Project** that may be enhanced by any of us

This is a webapp dedicated at the same time for those who want to play get a feeling of the cancer incidence data (WHO source) and the 
worldbank data (WorldBank source) and as much to data-scientists to test some correlation between variables and explore statistical models, classical in the field of time series. 

To get a custom plot, the user has only to select 

- type of cancer
- country
- gender
- age range

Some predictice forecasting are ran: @t the moment of the writing - 07.09.17 - the models implemented are `ARIMA` and `Exponential Smoothing`. The interested user may adjust incidence forecasting with WorlBank variable.   

# Running the webapp on local

Required tools 

```RStudio```

- Put the `preditiviz` folder in your `R projects` folder, or clone it there

- install the follwing package

```
install.packages(c("devtools","Rcpp"))
library(devtools)
library(Rcpp)
install_github('ramnathv/rCharts')
```

```
install.packages(c("ggplot2","shinyjs","xts","tseries","forecast","zoo","dplyr","rdrop2","gridExtra"))
```

- set your working directory to the `preditiviz` folder's root

```setwd(/your/path/to/predictiviz)```

- run the app by clicking on `Run App` top bar button

Have fun now ;)

# ToDos

- ensure comparability between models: split train and validation set and compute AIC criterion
- enhance the library of models
- enhance UI
- any other suggestions or feature requests can be added on the platform

## Contact
mehdi@epidemium.cc