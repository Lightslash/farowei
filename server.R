options(shiny.maxRequestSize=3000*1024^2)
options(shiny.fullstacktrace = TRUE)

library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(stringi)
library(gridExtra)
library(rCharts)
library(xts)
library(tseries)
library(forecast)
library(curl)
library(vars)
library(astsa)
library(urca)
library(fGarch)
source("loadData.R")
source("helper.R")

prepare_incidence_data <- function(){
  #file <- file.path("data", "training_IARC.csv") 
  #epi_file <- read.csv(file,sep=";",header=FALSE,nrows=2000000)
  epi_file <- loadIncidenceData(2000000)
  names(epi_file) <- c("type","gender","age","country","locality","ethnicity","period","incidence")  
  epi_file
}

# prepare_mortality_data <- function(){
#   mortality_file <- read.csv("/Users/mehdibenchoufi/Downloads/epidemium_dataset/formatted/world_mortality.csv",sep=",")
#   mortality_file$X <- NULL
#   mortality_file
# }

prepare_label <- function(){
  age_range_list <- c()
  number_range <- 17
  
  for(i in 1:number_range){
    age_range_list[i]=paste(5*(i-1),"-",5*i-1,sep="")
  }
  age_range_list <- c(age_range_list,"85+","age unknown")
  
  age_range_list
}

prepare_cancer_codes <- function(){
  #file <- file.path("data", "cancer_codes.csv") 
  #cancer_codes <- read.csv(file,sep="\t",header=FALSE)
  cancer_codes <- loadCancerCodes()
  names(cancer_codes) <- c("code","type")
  cancer_codes
}

prepare_worldbank_data <- function(){
  #file <- file.path("data", "WorldBank_Data.csv")
  #worldbank_data <- read.csv(file,sep=",",header=TRUE)
  worldbank_data <- loadWorldBankData()
  if(colnames(worldbank_data)[2]=="area")
    colnames(worldbank_data)[2] <- "country"
  if(colnames(worldbank_data)[3]=="year")
    colnames(worldbank_data)[3] <- "period"
  worldbank_data
}

prepare_worldbank_indicators <- function(){
  #file <- file.path("data", "WorldBank_Indicators.csv")
  #worldbank_indicators <- read.csv(file,sep=",",header=TRUE)
  worldbank_indicators <- loadWorldBankIndicators()
  worldbank_indicators
}

filter_by_name <- function(data,country){
  filtered <- filter(data,data$country==stringi::stri_trans_totitle(country))
}

filter_by_country <- function(data,country_name){
  filtered <- filter(data,data$country==country_name)
}

filter_by_locality <- function(data,locality_name){
  filtered <- filter(data,data$locality==locality_name)
}

filter_by_ethnicity <- function(data,ethnicity_name){
  filtered <- filter(data,data$ethnicity==ethnicity_name)
}

filter_by_age <- function(data,range_age){
  filtered <- filter(data,data$age==range_age)
}

filter_by_gender <- function(data,b_gender){
  filtered <- filter(data,data$gender==b_gender)
}

filter_by_type <- function(data,cancer_codes,type_of_cancer){
  cancer_code <- filter(cancer_codes,cancer_codes$type==type_of_cancer)
  filtered <- filter(data,data$type==as.character(cancer_code$code[[1]]))
  filtered
}

filter_by_period <- function(data,start_year,end_year){
  if(exists("start_year")&&exists("end_year")){
    converted_data <- data.frame(period<-data$period,incidence<-data$incidence)
    converted_data <- filter(converted_data,converted_data$period %in% c(start_year:end_year)) 
    data <- converted_data
  }
  data
}

grep_filter_by_type <- function(data,type_of_cancer){
  filtered <- filter(data,grepl(type_of_cancer,data$type))
}

transform_levels <- function(data){
  data$age <- factor(data$age)
  age_range_list <- prepare_label()
  levels(data$age) <- age_range_list
  data
}

group_by_country <- function(data){
  data_output <- data %>% group_by(data$period) %>% summarise(incidence=sum(incidence))
  data_output
}

sum_by_gender <- function(data){
  data_output <- data %>% group_by(period,age) %>% summarise(incidence=sum(incidence)) 
  data_output
}

worldbank_by_country <- function(data,name_of_country){
  name <- extract_country_from_locality(name_of_country) 
  data <- filter(data,data$country==name)
  data
}

interpolation_optimisation <- function(data){
  if(length(data)>0){
    polyfit <- function(i) data <- AIC(lm(data$incidence~poly(data$period,i)))
    min_poly_interpolation <- min(nrow(data),20)
    if(min_poly_interpolation > 1)
      as.integer(optimize(polyfit,interval = c(1,min_poly_interpolation-1))$minimum)
    else{
      NULL
    }
  }
  else{
    NULL
  }
}

convert_to_dataframe <- function(data){
  indexFormat(data) <- "%Y"
  data <- data.frame(period = index(data),incidence = data, row.names=NULL)
  data
}

clean_data <- function(data){
  data <- na.omit(data)
  data
}

extract_year <- function(data){
  format(as.Date(index(data)[1], format="%d/%m/%Y"),"%Y")
}

extract_indicator_from_worldbank_data <- function(indicators_list,indicator){
  indicators_list <- lapply(indicators_list, as.character)
  indicator_title <- which(sapply(indicators_list, FUN=function(X) indicator %in% X))
  names(indicator_title)
}

ggplot_theme <- theme( 
  panel.background = element_rect(fill = '#F3ECE2'), 
  plot.background = element_rect(fill = '#F3ECE2'), 
  panel.grid.major = element_line(color = "#DFDDDA"), 
  panel.grid.minor = element_line(color = "#DFDDDA"),
  axis.title.x = element_text(color = "#B2B0AE"),
  axis.title.y = element_text(color = "#B2B0AE"),
  title = element_text(color = "#606060",hjust = 0.5))

print_null_plot <- function (error_title) {
  p <- plot(1, type="n", xlab="year", ylab="", xlim=c(1958, 2002), ylim=c(0,10), main=error_title)
  print(p)
}


function(input, output) { 
  epi_file <- prepare_incidence_data()
  #mortality_file <- prepare_mortality_data()
  cancer_codes <- prepare_cancer_codes()
  age_range <- prepare_label()
  worldbank_file <- prepare_worldbank_data()
  worldbank_indicators <- prepare_worldbank_indicators()
  
  indicators_list <- list()
  wb_indicators_length <- nrow(worldbank_indicators)
  
  for(i in 1:wb_indicators_length){
    indicators_list[as.character(worldbank_indicators$Indicator.Name[i])]=as.character(worldbank_indicators$Code[i])
  }

  dataset <- reactive({
    data <- filter_by_locality(epi_file,extract_locality_from_string(input$locality))
    data <- filter_by_type(data,cancer_codes,input$type)
    data
  })
  
  dataset_by_gender <- reactive({
    data <- dataset()
    if(input$gender !=3)
      data <- filter_by_gender(data,input$gender)
    data
  })
  
  dataset_by_age <- reactive({
    data <- dataset()
    data <- filter_by_age(data,input$age)
    data
  })
  
  filter_worldbank_by_country <- reactive({
    data <- worldbank_file    
    data <- worldbank_by_country(data,extract_country_from_locality(input$locality))
    data
  }) 
  
  interpolation_order <- reactive({
    input$interpolation
  })
  
  extract_gender_choice_from_input <- reactive({
    options <- c("male","female","male+female")
    choice <- match(input$gender,options)
    choice
  })
  
  test_stationnarity <- function(data){
    test_stationnarity <- clean_data(data)
    #test_stationnarity <- adf.test(test_stationnarity,alternative="stationary")
    test_stationnarity <- ur.df(test_stationnarity,type="none")
    test_stationnarity
  }
  
  test_stationnarity_type_drift <- function(data){
    test_stationnarity <- clean_data(data)
    test_stationnarity <- ur.df(test_stationnarity,type="drift")
    test_stationnarity
  }
  
  test_stationnarity_type_trend <- function(data){
    test_stationnarity <- clean_data(data)
    test_stationnarity <- ur.df(test_stationnarity,type="trend")
    test_stationnarity
  }
  
  prepare_acf_pacf <- function(){
    data <- dataset_by_age()
    if(input$gender !=3)
      data <- filter_by_gender(data,input$gender)
    else
      data <- sum_by_gender(data)
    data <- xts(data$incidence, as.Date(as.character(data$period), format='%Y'))
    if(length(data)>0){
    interpolation_order <- interpolation_order()
    if(interpolation_order>0){
      data <- diff(data, interpolation_order)
      }
    data
    }
    else{
      NULL
    }
  }
  
  get_prediction_fitted_data <- function(data){
    fit <- auto.arima(data) 
    fit
  }
  
  get_correlated_data <- function(who_data,db_data){
    fit <- auto.arima(who_data,xreg=db_data)
    fit
  }
  
  get_spe_prediction_fitted_data <- function(data,ar_p,diff_d,ma_q){
    fit <- arima(data, order = c(ar_p,diff_d,ma_q), include.mean = FALSE)
    fit
  }
  
  get_sarima_prediction_fitted_data <- function(data,ar_p,diff_d,ma_q){
    fit <- sarima(data,ar_p,diff_d,ma_q)
    fit
  }
  
  get_es_prediction_fitted_data <- function(data,error_type,trend_type,season_type){
    es_model <- paste(error_type,trend_type,season_type,sep="")
    fit <- ets(data,model=es_model)
    fit
  }
  
  get_prediction_data <- function(start_year,end_year){
    data <- dataset_by_age()
    data <- sum_by_gender(data)
    if(!missing(start_year) && !missing(end_year))
      data <- filter_by_period(data,start_year,end_year)
    data
  } 
  
  convert_to_timeseries_data <- function(data){
    data <- xts(data$incidence, as.Date(as.character(data$period), format='%Y'))
    min_year <- extract_year(data)
    data <- ts(data, start= c(as.numeric(min_year),1), frequency=1)
    data
  }
  
  param_convert_to_timeseries_data <- function(data,indicator){
    data <- xts(data[[indicator]], as.Date(as.character(data$period), format='%Y'))
    min_year <- extract_year(data)
    data <- ts(data, start= c(as.numeric(min_year),1), frequency=1)
    data
  }
  
  get_arima_parameter <- reactive({
   c(input$p,input$d,input$q)
  })
  
  get_es_parameter <- reactive({
    c(input$error_type,input$trend_type,input$season_type)
  })
    
#   mortality_dataset <- reactive({
#     data_0 <- filter_by_country(mortality_file,"Canada")
#     data_1 <- filter_by_age(data_0,11)
#     data_2 <- filter_by_gender(data_1,1)
#     data_3 <- grep_filter_by_type(data_2,"34")
#     data <- group_by_country(data_3)
#     data
#   })
  
  output$global <- renderChart2({
    dataset <- dataset_by_gender()
    dataset <- transform_levels(dataset)
    dataset <- sum_by_gender(dataset)
    plot_title <- paste("Incidence cancer by age range", input$type)
    
    p <- nPlot(incidence ~ period, group="age", dataset, type = "lineChart")
    
    p$xAxis(axisLabel="Period")
    p$yAxis(axisLabel="Incidence")
    p$set(title=plot_title)
    
    return(p)
  })
  
  output$gender <- renderPlot({    
    data <- dataset_by_age()
    data_m <- filter_by_gender(data,1)
    
    plot_title <- paste("male cancer incidence for ",input$type,"\nrange age:",age_range[as.integer(input$age)],"years old")
    p <- ggplot(data_m, aes_string(x = data_m$period, y = data_m$incidence),fill = "#383837") + geom_line()
    p <- p + stat_smooth(method="lm",formula = y ~ poly(x, 20),fullrange = TRUE,alpha=.2) + 
    geom_line() 
    p <- p + ggplot_theme +  labs(x = "period", y = "incidence", title =labs(title = plot_title)) + labs(caption = "(based on data from WHO)")
  
    data_f <- filter_by_gender(data,2)
    plot_title <- paste("female cancer incidence for",input$type,"\nrange age:",age_range[as.integer(input$age)],"years old")
    q <- ggplot(data_f, aes_string(x = data_f$period, y = data_f$incidence),fill = "#383837") + geom_line()
    q <- q + stat_smooth(method="lm",formula = y ~ poly(x, 20),fullrange = TRUE,alpha=.2) + 
      geom_line() 
    q <- q + ggplot_theme +  labs(x = "period", y = "incidence", title =labs(title = plot_title)) + labs(caption = "(based on data from WHO)")
    
    grid.arrange(p, q, ncol=2)
  })

  output$education <- renderPlot({
    data_m <- filter_worldbank_by_country()
    
    filled_data_m <- na.locf(data_m$SE.PRM.UNER.MA.ZS,fromLast=TRUE)
    data_m <- data_m[1:length(filled_data_m),]
    data_m$SE.PRM.UNER.MA.ZS <- filled_data_m
    data_wb_cross_validation <- data_m
    plot_title <- "Some example: Children out of school, pupils (% male)"
    p <- ggplot(data_m, aes_string(x = data_m$period, y = data_m$SE.PRM.UNER.MA.ZS),fill = "#383837") + geom_line()
    p <- p + stat_smooth(method="lm",formula = y ~ poly(x, 20),fullrange = TRUE,alpha=.2) + 
      geom_line() 
    p <- p + ggplot_theme +  labs(x = "period", y = "(% male)", title =labs(title = plot_title)) + labs(caption = "(based on data from Worldbank)")
    
    data_f <- data_m
    data_f$SE.PRM.UNER.FE.ZS <- na.locf(data_f$SE.PRM.UNER.FE.ZS,fromLast=TRUE)
    plot_title <- "Some example: Children out of school (% female)"
    q <- ggplot(data_f, aes_string(x = data_f$period, y = data_f$SE.PRM.UNER.FE.ZS),fill = "#383837") + geom_line()
    q <- q + stat_smooth(method="lm",formula = y ~ poly(x, 20),fullrange = TRUE,alpha=.2) + 
      geom_line() 
    q <- q + ggplot_theme +  labs(x = "period", y = "(% female)", title =labs(title = plot_title)) + labs(caption = "(based on data from Worldbank)")
    
    grid.arrange(p, q, ncol=2)
  })

  output$acf <- renderPlot({
    data <- prepare_acf_pacf()
    
    if(length(data)>0){
      test_stationnarity <- test_stationnarity(data)
      data <- convert_to_dataframe(data)
      data <- clean_data(data)
      
      plot_title <- paste("Auto-Correlation Function for", input$type,"\nrange age:",age_range[as.integer(input$age)],"years old",input$gender)
      p <- acf(data$incidence, xlab="lags", ylab="ACF", main=plot_title)
      #mtext(format(capture.output(summary(test_stationnarity))), 3, line=-1:-6,
       #     col="#979693")
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$pacf <- renderPlot({
    data <- prepare_acf_pacf()
    
    if(length(data)>0){
      
      test_stationnarity <- test_stationnarity(data)
      data <- convert_to_dataframe(data)
      data <- clean_data(data)
      
      if(length(data)>0){
        plot_title <- paste("Partial Auto-Correlation Function for ", input$type,"\nrange age:",age_range[as.integer(input$age)],"years old")
        p <- pacf(data$incidence, xlab="lags", ylab="ACF", main=plot_title)
        #mtext(format(test_stationnarity), 3, line=-1:-6,
        #     col="#979693")
        #print(capture.output(summary(test_stationnarity)))
        h3(textOutput("caption"))
      }
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$df_test_output_none <- renderPrint({
    data <- prepare_acf_pacf()

    if(length(data)>0){
      test_stationnarity <- test_stationnarity(data)
      test_stationnarity_drift <- test_stationnarity_type_drift(data)
      test_stationnarity_trend <- test_stationnarity_type_trend(data)
      HTML(paste0("<div>",capture.output(summary(test_stationnarity)),
                  "</div>"))
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$df_test_output_drift <- renderPrint({
    data <- prepare_acf_pacf()
    
    if(length(data)>0){
      test_stationnarity_drift <- test_stationnarity_type_drift(data)
      HTML(paste0("<div>",capture.output(summary(test_stationnarity_drift)),
                  "</div>"))
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$df_test_output_trend <- renderPrint({
    data <- prepare_acf_pacf()
    
    if(length(data)>0){
      test_stationnarity_trend <- test_stationnarity_type_trend(data)
      HTML(paste0("<div>",capture.output(summary(test_stationnarity_trend)),
                  "</div>"))
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })
  
  output$interpolation <- renderPlot({
      data <- dataset_by_age()
      data <- sum_by_gender(data)
      interpolation_order <- as.integer(interpolation_order())
      
      if(nrow(data)>interpolation_order+1){
        if(interpolation_order==0)
          interpolation_order=1
        interpolation_optimisation <- interpolation_optimisation(data)
        
        plot_title <- paste("Best polynomial interpolation order: ",interpolation_optimisation)
        q <- ggplot(data, aes_string(x = data$period, y = data$incidence),fill = "#383837") + geom_line()
        q <- q + stat_smooth(method="lm",formula = y ~ poly(x, interpolation_order),fullrange = TRUE,alpha=.2)
        q <- q + ggplot_theme +  labs(x="period",y="incidence", title =labs(title=plot_title)) + labs(caption="(based on data from WHO)")
        interpolated <- lm(data$period ~ poly(data$incidence, interpolation_order, raw=TRUE))
        print(q)
      }
      else{
        print_null_plot("no sufficient DATA")
      }
    })

  output$cross_correlation <- renderPlot({
    data_wb_cross_validation <- filter_worldbank_by_country()
        
    filled_data_wb_cross_validation <- na.locf(data_wb_cross_validation[[input$wb_indicator]],fromLast=TRUE)
    data_wb_cross_validation <- data_wb_cross_validation[1:length(filled_data_wb_cross_validation),]
        
    data_who_cross_validation <- dataset_by_age()
    data_who_cross_validation <- filter_by_gender(data_who_cross_validation,2)
    
    filled_data_who_cross_validation <- na.locf(data_who_cross_validation$incidence,fromLast=TRUE)
    data_who_cross_validation <- data_who_cross_validation[1:length(filled_data_who_cross_validation),]
    
    if(length(filled_data_who_cross_validation)>0 && length(filled_data_wb_cross_validation)>0){
      data_wb_cross_validation[[input$wb_indicator]] <- filled_data_wb_cross_validation
      data_who_cross_validation$incidence <- filled_data_who_cross_validation
      
      indicator_title <- extract_indicator_from_worldbank_data(indicators_list,input$wb_indicator)
      plot_title <- paste("Correlation", input$type, "Cancer" , indicator_title)
      ccf(data_wb_cross_validation[[input$wb_indicator]], data_who_cross_validation$incidence, ylab = "cross-correlation", main=plot_title)
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$Granger_causality <- renderPrint({
    data_wb_cross_validation <- filter_worldbank_by_country()
    
    filled_data_wb_cross_validation <- na.locf(data_wb_cross_validation[[input$wb_indicator]],fromLast=TRUE)
    data_wb_cross_validation <- data_wb_cross_validation[1:length(filled_data_wb_cross_validation),]
    
    data_who_cross_validation <- dataset_by_age()
    data_who_cross_validation <- filter_by_gender(data_who_cross_validation,2)
    
    filled_data_who_cross_validation <- na.locf(data_who_cross_validation$incidence,fromLast=TRUE)
    data_who_cross_validation <- data_who_cross_validation[1:length(filled_data_who_cross_validation),]
    
    if(length(filled_data_who_cross_validation)>0 && length(filled_data_wb_cross_validation)>0){
      data_wb_cross_validation[[input$wb_indicator]] <- filled_data_wb_cross_validation
      data_who_cross_validation$incidence <- filled_data_who_cross_validation
      for (i in 1:4)
      {
        cat("LAG =", i)
        causal_test <-causality(VAR(cbind(data_wb_cross_validation[[input$wb_indicator]], data_who_cross_validation$incidence), p = i, type = "const"), cause=)$Granger
        #causal_test <- capture.output(causal_test)
        text <- paste(causal_test,"\n")
        print(causal_test)
        #print(summary(data_wb_cross_validation[[input$wb_indicator]]))
      }
    }
    else{
      print_null_plot("no sufficient DATA")
    }
    
  })
    
  output$predict <- renderPlot({
    arima_parameter = get_arima_parameter()
    head = 4
    start_year = 2003
    data <- get_prediction_data(1958,2002)
    hidden_data <- get_prediction_data(2003,2007)
    names(data) <- c("period","incidence")
    names(hidden_data) <- c("period","incidence")

    if(nrow(data)>0){
      fit <- get_spe_prediction_fitted_data(data$incidence,
                                            arima_parameter[1],
                                            arima_parameter[2],
                                            arima_parameter[3]) 
    
      fcast <- as.data.frame(predict(fit, n.ahead = head+1))
        
      end_year <- start_year + head
      fcast$period <- c(start_year:end_year) 
      
      residuals <- 0
      for(i in 1:end_year-start_year){
        residuals = residuals + (fcast$pred[i]-hidden_data$incidence[i])^2
      }
      residuals <- residuals/(end_year-start_year)
            
      plot_title <- paste("Interpolated ARIMA (",arima_parameter[1],arima_parameter[2],arima_parameter[3],") for prediction at 4 years\n",
                          "Residuals: ",residuals)
      
      qplot(period, incidence, data = data, geom = "line",main=plot_title) +
        geom_ribbon(aes(ymax = pred + 2*se, ymin = pred - 2*se, y = NULL), 
                    data = fcast, alpha = 0.2) +
        geom_line(aes(y = pred), data = fcast, linetype = "dashed") +
        geom_line(aes(y = incidence), data = hidden_data, linetype = "dashed")
    }
    else{
      print_null_plot("no sufficient DATA")
    }
    
    #plot_title <- paste("ARIMA prediction at 10 years")
    #sub_title <- paste("AICC",fcast$model$aicc,"\nBICC",fcast$model$bic)
    #p <- plot(fcast, xlab="year", ylab="incidence")
    #mtext(sub_title,3,
    #      col="#979693",line=-2)
    
    #cast <- as.data.frame(fcast)
    
#     qplot(data$period, data$incidence, data, geom = "line") + 
#       geom_ribbon(aes(ymin = pred - 2*se, ymax = pred + 2*se, y = NULL), data = cast, alpha = 0.2) +
#       geom_line(aes(y = pred), data = cast) +
#       big_font
      
    # print(p)
    #grid.arrange(p, p, ncol=2, top="Main Title")
    })

  output$predict_summary <- renderPlot({
    arima_parameter = get_arima_parameter()
    head = 4
    start_year = 2003
    data <- get_prediction_data()
    
    if(nrow(data)>0){
      sfit <- get_sarima_prediction_fitted_data(data$incidence,
                                                arima_parameter[1],
                                                arima_parameter[2],
                                                arima_parameter[3])
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

#   output$residuals_title <- renderPrint({    
#       HTML(paste0("<div>Residuals for prediction at 4 years</div>"))
#   })
    
  output$residuals <- renderPlot({
    data <- get_prediction_data()
    if(nrow(data)>0){
      data <- convert_to_timeseries_data(data)
      fit <- get_prediction_fitted_data(data) 
      fcast <- forecast(fit, h=4)
      method <- fcast$method
      
      plot_title <- paste("Residuals for automated prediction at 4 years\n","Forecast method: ", method)
      p <- plot(residuals(fcast), lag.max=45, main=plot_title)
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$interpol_arima <- renderPlot({
    data <- get_prediction_data()
    interpolation_optimisation <- interpolation_optimisation(data)
    nrow <- nrow(data)
    split_param <- 0.8
    split_row <- as.integer(split_param*nrow(data))
    train_tdata <- data[1:split_row,]
    test_tdata <- data[split_row+1:nrow,]
    if(nrow(data)>0){
      tdata <- convert_to_timeseries_data(data)
      ttrain_tdata <- convert_to_timeseries_data(train_tdata)
  
      fit <- auto.arima(ttrain_tdata)
      fcast <- forecast(fit, h=nrow-split_row)
      
      residuals <- 0
      for(i in 1:nrow-split_row){
        residuals = residuals + (fcast$mean[i]-test_tdata$incidence[i])^2
      }
      residuals <- sqrt(residuals)
      
      head <- 10
      fit <- auto.arima(tdata)
      fcast <- forecast(fit, h=head)  
      
      sub_title <- paste("\nPrediction residuals",residuals)
      mtext(sub_title,3,
           col="#b74233",line=-2)
      #p <- tsdisplay(residuals(fcast), lag.max=45, main='(1,1,1) Model Residuals')
      p <- plot(fcast, xlab="year", ylab="incidence")
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$alternate_interpol_arima <- renderPlot({
    data <- get_prediction_data()
    interpolation_optimisation <- interpolation_optimisation(data)
    arima_parameter = get_arima_parameter()
    
    if(nrow(data)>0){
      tdata <- convert_to_timeseries_data(data)
      
      head <- 4 
      start_year <- 2003
      ## 
      interpol_model <- loess(incidence ~ period, data = data, span = 0.6, control = loess.control(surface = "direct"))    
      interpol_prediction <- as.data.frame(predict(interpol_model, data.frame(period = c(2003:2006)), se = TRUE))
      #data$fitted <- fitted(interpol_model)
      ##
      data$res <- data$incidence #- fitted(interpol_model)
      
      arima_model <- with(data, arima(res, order = c(arima_parameter[1],
                                                     arima_parameter[2],
                                                     arima_parameter[3]), include.mean = FALSE))
      res_prediction <- as.data.frame(predict(arima_model, n.ahead = head))
      
      interpol_prediction$pred <- res_prediction$pred
      interpol_prediction$se <- res_prediction$se
      interpol_prediction$period <- c(start_year:start_year+head) 
      
      plot_title <- paste("Interpolated ARIMA for prediction at 4 years")
      qplot(period, incidence, data = data, geom = "line",main=plot_title) +
        geom_ribbon(aes(ymax = pred + 2*se, ymin = pred - 2*se, y = NULL), 
                    data = interpol_prediction, alpha = 0.2) +
        geom_line(aes(y = fit), data = interpol_prediction, linetype = "dashed")
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  output$correlated_arima <- renderPlot({  
    data <- get_prediction_data()
    data <- filter(data,!is.na(incidence))
    data <- convert_to_timeseries_data(data)
    nrow <- nrow(data)
    split_param <- 0.8
    
    data_wb_cross_validation <- filter_worldbank_by_country()
    filled_data_wb_cross_validation <- na.locf(data_wb_cross_validation[[input$wb_indicator]],fromLast=TRUE)
    
    if(length(filled_data_wb_cross_validation)>0){
      data_wb_cross_validation[[input$wb_indicator]][1:length(filled_data_wb_cross_validation)] <- filled_data_wb_cross_validation      
      data_wb_cross_validation <- filter(data_wb_cross_validation,!is.na(input$wb_indicator))
      min_period <- min(data_wb_cross_validation$period)
      data_wb_cross_validation <- param_convert_to_timeseries_data(data_wb_cross_validation,input$wb_indicator)  
      
      who_split_row <- as.integer(split_param*nrow(data))
      wb_split_row <- as.integer(split_param*nrow(data_wb_cross_validation))
      
      if(who_split_row <= wb_split_row ){
        who_train_data <- window(data,start=start(data),end=start(data)[1]+who_split_row)
        #who_test_data <- data[who_split_row+1:nrow,]
        wb_train_data <- window(data_wb_cross_validation,start=start(data_wb_cross_validation),
                                end=start(data_wb_cross_validation)[1]+who_split_row)
        wb_test_data <- window(data_wb_cross_validation,start=start(data_wb_cross_validation)[1]+who_split_row+1,end=end(data_wb_cross_validation))
        head <- end(data_wb_cross_validation)[1] - start(data_wb_cross_validation)[1] - who_split_row
      }
      else{
        who_train_data <- window(data,start=start(data),end=start(data)[1]+wb_split_row)
        wb_train_data <- window(data_wb_cross_validation,start=start(data_wb_cross_validation),
                                end=start(data_wb_cross_validation)[1]+wb_split_row)
        wb_test_data <- window(data_wb_cross_validation,start=start(data_wb_cross_validation)[1]+wb_split_row+1,end=end(data_wb_cross_validation))
        
        head <- end(data_wb_cross_validation)[1] - start(data_wb_cross_validation)[1] - wb_split_row 
      }
      
      fit <- auto.arima(who_train_data, xreg=wb_train_data)
      fcast <- forecast(fit,xreg=wb_test_data,h=head)
      
      age_range <- prepare_label()
      indicator_title <- extract_indicator_from_worldbank_data(indicators_list,input$wb_indicator)
      plot_title <- paste(input$type, "Cancer Prediction regressed on", indicator_title,"\nrange age:",age_range[as.integer(input$age)],"years old")
      plot(fcast,main=plot_title)
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

  
  output$es_predict <- renderPlot({
    data <- get_prediction_data()
    es_parameter <- get_es_parameter()
    
    if(nrow(data)>0){
      data <- convert_to_timeseries_data(data)
      fit <- get_es_prediction_fitted_data(data,es_parameter[1],
                                                es_parameter[2],
                                                es_parameter[3]) 
      fcast <- forecast(fit, h=10)  
      
      plot_title <- paste("Prediction at 4 years, exponential smoothing")
      p <- plot(fcast, xlab="year", ylab="incidence",main=plot_title)
    }
    else{
      print_null_plot("no sufficient DATA")
    }
  })

#   output$garch_predict <- renderPlot({
#     data <- get_prediction_data()
#     arima_parameter = get_arima_parameter()
#     head = 4
#     start_year = 2003
#     data <- get_prediction_data(1958,2002)
#     hidden_data <- get_prediction_data(2003,2007)
#     names(data) <- c("period","incidence")
#     names(hidden_data) <- c("period","incidence")
#     
#     if(nrow(data)>0){
#       tdata <- convert_to_timeseries_data(data)
#       fit <- garchFit(~garch(1,1), data = tdata, trace = FALSE) 
#       fcast <- predict(fit, n.ahead = 5) 
#       
#       end_year <- start_year + head
#       fcast$period <- c(start_year:end_year) 
#       
#       residuals <- 0
#       for(i in 1:end_year-start_year){
#         residuals = residuals + (fcast$pred[i]-hidden_data$incidence[i])^2
#         print(fcast$pred[i])
#         print(hidden_data$incidence[i])
#         print(residuals)
#       }
#       residuals <- residuals/(end_year-start_year)
#       
#       plot_title <- paste("Interpolated GARCH (",arima_parameter[1],arima_parameter[2],arima_parameter[3],") for prediction at 4 years\n",
#                           "Residuals: ",residuals)
#       
#       qplot(period, incidence, data = data, geom = "line",main=plot_title) +
#         geom_ribbon(aes(ymax = pred + 2*se, ymin = pred - 2*se, y = NULL), 
#                     data = fcast, alpha = 0.2) +
#         geom_line(aes(y = pred), data = fcast, linetype = "dashed") +
#         geom_line(aes(y = incidence), data = hidden_data, linetype = "dashed")
#     }
#     else{
#       print_null_plot("no sufficient DATA")
#     }
#   })

  output$models <- renderUI({
    dyn_ui <- NULL
    if(input$models==1&&input$conditionedPanels==1){
      dyn_ui <- tabsetPanel(
        h5("Arima parameters"),
        sliderInput("p", h6("Arima p"),
                    min = 0, max = 10, value = 0, step=1),
        sliderInput("d", h6("Arima d"),
                    min = 0, max = 10, value = 0, step=1),
        sliderInput("q", h6("Arima q"),
                    min = 0, max = 10, value = 0, step=1)
      )
    }
    if(input$models==2){
      dyn_ui <- tabsetPanel(
        h5("Exponential smoothing parameters"),
        fluidRow(column(12,
                        selectInput("error_type", label = "trend type", choices = 
                                      list("A","M","Z"), 
                                    width=validateCssUnit("100%")))),
        fluidRow(column(12,
                        selectInput("trend_type", label = "trend type", choices = 
                                      list("N","A","M","Z"), 
                                    width=validateCssUnit("100%")))),
        fluidRow(column(12,
                        selectInput("season_type", label = "season type", choices = 
                                      list("N","A","M","Z"), 
                                    width=validateCssUnit("100%"))))
      )
      print(input$error_type)
    }
    
    return(dyn_ui)
  })
}
