# Rose Wang
# July 16, 2019
# ICEES P(XYZ) Table
# Roadway Distance, Max Daily PM 2.5 Exposure, ED/Inpatient Visits
#
rm(list=ls())
setwd("/Users/rosewang/Downloads/RCompSci")
#
source("myfunctions.R")
#
library(dplyr)
library(ggplot2)
library(jsonlite)
library(httr)

# cohort creation
cohort <- c()
for(i in 0:9){
  urls <- paste("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A",i+110,"",sep = "")
  bodys <- paste('{"TotalEDInpatientVisits":{"operator":"=","value":',i,'}}', sep = "")
  data <- PUT(as.character(urls), body = as.character(bodys), accept_json(), content_type("application/json"), encode = "json")
  data.text <- content(data, "text")
  data.json <- fromJSON(data.text, flatten = TRUE)
  c <- data.json[["return value"]][["cohort_id"]]
  cohort <- c(cohort, c)
}
cohort <- strsplit(cohort, "COHORT:")

# Roadway PM2.5 table creation
total <- data.frame("TotalEDInpatientVisits" = c(), "MajorRoadwayHighwayExposure" = c(), "MaxDailyPM2.5Exposure_qcut" = c(), "frequency" = c(), "column_percentage" = c(), "row_percentage" = c(), "total_percentage" = c())
for(i in 1:10){
  urls <- paste('https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A',cohort[[i]][2],'/feature_association2', sep = "")
  MajPM2.5data <- POST(as.character(urls),
                        body ='{
                        "feature_a": {
                        "MajorRoadwayHighwayExposure": [{
                        "operator": "=",
                        "value": 1
                        },{
                        "operator": "=",
                        "value": 2
                        },{
                        "operator": "=",
                        "value": 3
                        },{
                        "operator": "=",
                        "value": 4
                        },{
                        "operator": "=",
                        "value": 5
                        }]},
                        "feature_b": {
                        "MaxDailyPM2.5Exposure_qcut": [{
                        "operator": "=",
                        "value": 1
                        },{
                        "operator": "=",
                        "value": 2
                        },{
                        "operator": "=",
                        "value": 3
                        },{
                        "operator": "=",
                        "value": 4
                        },{
                        "operator": "=",
                        "value": 5
                        }]}}',
          accept_json(),
          content_type("application/json"),
          encode = "json")
  MajPM2.5data_text <- content(MajPM2.5data, "text")
  
  # convert into data frame
  MajPM2.5data_json <- fromJSON(MajPM2.5data_text, flatten = TRUE)
  MajPM2.5data_json[["terms and conditions"]] <- NULL
  MajPM2.5data_json[["version"]] <- NULL
  MajPM2.5.df <- data.frame("frequency" = c(), "column_percentage" = c(), "row_percentage" = c(), "total_percentage" = c(),"TotalEDInpatientVisits" = c(), "MajorRoadwayHighwayExposure" = c(), "MaxDailyPM2.5Exposure_qcut" = c())
  for(n in 1:5){
    MajPM2.5feature_matrix.df <- as.data.frame(MajPM2.5data_json[["return value"]][["feature_matrix"]][[n]])
    t <- MajPM2.5feature_matrix.df %>%
      mutate(TotalEDInpatientVisits = i-1, MajorRoadwayHighwayExposure = 1:5, MaxDailyPM2.5Exposure_qcut = n )
    MajPM2.5.df <- rbind(MajPM2.5.df, t)
  }
  total <- rbind(total, MajPM2.5.df)
}
total <- total[,c("TotalEDInpatientVisits", "MajorRoadwayHighwayExposure", "MaxDailyPM2.5Exposure_qcut", "frequency", "column_percentage", "row_percentage", "total_percentage")]
