library(curl)
library(rdrop2)

#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
#token <- readRDS("droptoken.rds")
#drop_acc(dtoken = token)

outputDir <- "data"
URL <- "http://qa.epidemium.cc/data/shinyapp/"

prepareLoadData <- function() {
  # Read all the files into a list  
  filesInfo <- drop_dir(outputDir)
  filesInfo
}

# load the folder "data", located in dropbox
# to do that use the file droptoken.rds which
# provides the keys (password, login) to reach the dropbox
# dropbox account (of Mehdi in our case)

loadCancerCodes <- function(){
  #filesInfo<- prepareLoadData()
  #filePaths <- filesInfo$path
  #cancer_codes <- drop_read_csv(filePaths[1],sep="\t",header=FALSE)
  #URL <- paste(URL,"cancer_codes.csv",sep="")
  #download.file(URL, destfile = "./data/cancer_codes.csv")
  #file <- file.path("data", "cancer_codes.csv") 
  cancer_codes <- read.csv("/Users/mehdibenchoufi/Desktop/Sites/R\ projects/incidence_viz/predictiviz_test/data/cancer_codes.csv",sep="\t",header=FALSE)
  cancer_codes
}

# the 1st file in "data" corresponds to the cancer codes, hence the instruction
# filePaths[1], etc

loadLocalityFile <- function(){
  #filesInfo<- prepareLoadData()
  #filePaths <- filesInfo$path
  #locality_file <- drop_read_csv(filePaths[2],sep="\t",header=FALSE)
  #URL <- paste(URL,"localite_codes.csv",sep="")
  #download.file(URL, destfile = "./data/localite_codes.csv")
  #file <- file.path("data", "localite_codes.csv") 
  locality_file<- read.csv("/Users/mehdibenchoufi/Desktop/Sites/R\ projects/incidence_viz/predictiviz_test/data/locality_codes.csv",sep="\t",header=FALSE)
  locality_file
}

loadIncidenceData <- function(n_rows){
  #filesInfo<- prepareLoadData()
  #filePaths <- filesInfo$path
  #URL <- paste(URL,"training_IARC.csv",sep="")
  #download.file(URL, destfile = "./data/training_IARC.csv")
  #file <- file.path("data", "training_IARC.csv") 
  data <- read.csv("/Users/mehdibenchoufi/Desktop/Sites/R\ projects/incidence_viz/predictiviz_test/data/dataset.csv",sep=";",header=FALSE)#,nrows=2000000)
  #data <- read.csv(file,sep=";",header=FALSE,nrows=2000000)
  #data <- drop_read_csv(filePaths[3],sep=";",header=FALSE,nrows=n_rows)
  data
}


# load the n_rows first lines of the incidence dataset, retrieved by filePaths[3]

loadWorldBankData <- function(){
  #filesInfo<- prepareLoadData()
  #filePaths <- filesInfo$path
  #worldbank_data <- drop_read_csv(filePaths[4],sep=",",header=TRUE)
  #URL <- paste(URL,"WorldBank_Data.csv",sep="")
  #download.file(URL, destfile = "./data/WorldBank_Data.csv")
  #file <- file.path("data", "WorldBank_Data.csv") 
  worldbank_data <- read.csv("/Users/mehdibenchoufi/Desktop/Sites/R\ projects/incidence_viz/predictiviz_test/data/WorldBank_Data.csv",sep=",",header=TRUE)
  worldbank_data
}

loadWorldBankIndicators <- function(){
  #filesInfo<- prepareLoadData()
  #filePaths <- filesInfo$path
  #worldbank_indicators <- drop_read_csv(filePaths[5],sep=",",header=TRUE)
  #URL <- paste(URL,"WorldBank_Indicators.csv",sep="")
  #download.file(URL, destfile = "./data/WorldBank_Indicators.csv")
  #file <- file.path("data", "WorldBank_Indicators.csv") 
  worldbank_indicators <- read.csv("/Users/mehdibenchoufi/Desktop/Sites/R\ projects/incidence_viz/predictiviz_test/data/WorldBank_Indicators.csv",sep=",",header=TRUE)
  worldbank_indicators
}