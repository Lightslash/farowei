library(shiny)
library(ggplot2)
library(dplyr)
library(rCharts)
library(curl)
source("loadData.R")
source("helper.R")

prepare_incidence_data <- function(){
  #file <- file.path("data", "training_IARC.csv") 
  #epi_file <- read.csv(file,sep=";",header=FALSE,nrows=2000000)
  epi_file <- loadIncidenceData(2000000)
  names(epi_file) <- c("type","gender","age","country","locality","ethnicity","period","incidence")  
  epi_file
}

prepare_locality_data <- function(){
  #file <- file.path("data", "localite_codes.csv") 
  #locality_file <- read.csv(file,sep="\t",header=FALSE)
  locality_file <- loadLocalityFile()
  names(locality_file) <- c("code","name")
  locality_file
}

prepare_cancer_codes <- function(){
  #file <- file.path("data", "cancer_codes.csv") 
  #cancer_codes <- read.csv(file,sep="\t",header=FALSE)
  cancer_codes <- loadCancerCodes()
  names(cancer_codes) <- c("code","type")
  cancer_codes
}

prepare_worldbank_indicators <- function(){
  #file <- file.path("data", "WorldBank_Indicators.csv")
  #worldbank_indicators <- read.csv(file,sep=",",header=TRUE)
  worldbank_indicators <- loadWorldBankIndicators()
  worldbank_indicators
}

# prepare_mortality_data <- function(){
#   mortality_file <- read.csv("/Users/mehdibenchoufi/Downloads/epidemium_dataset/formatted/world_mortality.csv",sep=",")
#   mortality_file$X <- NULL
#   mortality_file
# }

epi_file <- prepare_incidence_data()
locality_file <- prepare_locality_data()
cancer_codes <- prepare_cancer_codes()
worldbank_indicators <- prepare_worldbank_indicators()

age_list <- c()
age_range_list <- list()
number_range <- 17

for(i in 1:number_range){
  age_list[i]=paste(5*(i-1),"-",5*i-1,sep="")
  age_range_list[[age_list[i]]]=i
}

indicators_list <- list()
wb_indicators_length <- nrow(worldbank_indicators)

for(i in 1:wb_indicators_length){
  indicators_list[as.character(worldbank_indicators$Indicator.Name[i])]=as.character(worldbank_indicators$Code[i])
}

#we create a list containing each indicator name and the associated code from
#the file worldbank_indicators

#age_list <- c(age_list,"85+","age unknown")


fluidPage(
  
  shinyjs::useShinyjs(),
  
  titlePanel("epidemium #WorldIncidence #Cancer"),
  
  br(),
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==0 || input.conditionedPanels==1",
                     h3("WorldBank indicator"),
                     selectInput('wb_indicator',h4("worldbank source"),
                                 choices=indicators_list,selected=indicators_list[305])
                     
    ),
    conditionalPanel(condition="input.conditionedPanels==1",
                     h3("Predicitve models"),
                     fluidRow(column(12,
                                     selectInput("models", label = "Models", choices = 
                                                   list("ARIMA"=1,"exponential smoothing"=2,"GARCH"=3), 
                                                 width=validateCssUnit("100%"))))
    ),
    uiOutput("models"),
        
    tags$head( # CSS insert
      tags$style(HTML("      
      td, th, tr {
        padding-right: 10px;
        padding-left: 10px;
        border: 1px solid #d3d3d3;
      }
    "))
    ),
    
    h3("Cancer typology"),
    
    selectInput('type',h4("classified by CIM10"),c(levels(cancer_codes$type)),selected=levels(cancer_codes$type)[117]),
    
    h3("Space & Time"),
    
    selectInput('locality',h4("Locality"),c(levels(locality_file$name)),selected=c(levels(locality_file$name))[10]),
    
    code("check Manitoba, Canada, to get insight"),
    
    h3("Population"),
    
    fluidRow(column(3, 
                    radioButtons("gender", 
                              label = h4("Gender"), 
                              choices = list("male"=1, 
                 
                                             "female"=2,"male+female"=3), selected=3))),
    
    
    fluidRow(column(12,
           selectInput("age", label = "Age range", 
                       choices = age_range_list, width=validateCssUnit("100%"), selected = 11))),
        
    sliderInput("year", h4("Year"),
                min = 1958, max = 2007, value = c(1958,2007),format = "yyyy"),
    
    code("recommended: 10 year interval"),
    
    sliderInput("interpolation", "Interpolation Order", 
                min =0, max=25, value=1, step=1)
    ),
  
  mainPanel(
    
    tabsetPanel(position=c("right"),
                tabPanel(strong("Plots"),
                         br(),
                         showOutput("global", "nvd3"),
                         plotOutput("gender"),
                         plotOutput("education"),
                         value=2
                         ),
                tabPanel(strong("Analysis Method"),
                         br(),
                         plotOutput("acf"),
                         plotOutput("pacf"),
                         uiOutput("df_test_output_none"),
                         uiOutput("df_test_output_drift"),
                         uiOutput("df_test_output_trend"),
                         plotOutput("interpolation")),
                tabPanel(strong("Correlation WB data"),
                         br(),
                         plotOutput("cross_correlation"),
                         value=0),
                tabPanel(strong("Causality Analysis"),
                         br(),
                         textOutput("Granger_causality"),
                         value=0),
                tabPanel(strong("Prediction Analysis"),
                         br(),
                         conditionalPanel("input.models==1",
                         plotOutput("predict"),
                         #plotOutput("alternate_interpol_arima"),
                         plotOutput("predict_summary"),
                         plotOutput("residuals"),
                         #plotOutput("interpol_arima"),
                         plotOutput("correlated_arima")),
                         conditionalPanel("input.models==2",
                                          plotOutput("es_predict"),
                                          plotOutput("es_residuals")),
                         conditionalPanel("input.models==3",
                                          plotOutput("garch_predict")),
                         value=1),
#                  tabPanel(strong("Code"),
#                           br()),
#                  tabPanel(strong("Epidemium"),
#                           br()),
                id = "conditionedPanels")
  )
)