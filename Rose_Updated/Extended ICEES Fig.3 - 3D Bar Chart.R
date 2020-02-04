# Rose Wang
# June 13, 2019
# Extended ICEES Fig.3 - 3D Bar Chart
# Roadway Distance, Max Daily PM 2.5 Exposure, % ED Patients
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
library(latticeExtra)
library(lattice)
library(TeachingDemos)
#
EDvisitsdata <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A22/associations_to_all_features",
              body = '{"feature":{"TotalEDInpatientVisits":{"operator":"<","value":2}},"maximum_p_value":0.1}',
              accept_json(),
              content_type("application/json"),
              encode = "json")
EDvisitsdatatext <- content(EDvisitsdata, "text")
#
EDvisitsdatajson <- fromJSON(EDvisitsdatatext, flatten = TRUE)
PM2.5hist.df <- data.frame("TotalEDInpatientVisits" = c(0), "MaxDailyPM2.5Exposure" = c(0), "frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0))
for (i in 1:5) {
  PM2.5data.df <- as.data.frame(EDvisitsdatajson$"return value"$feature_matrix[[10]][[i]])
  PM2.5features.df <- data.frame("TotalEDInpatientVisits" = c("<2",">=2"), "MaxDailyPM2.5Exposure" = c(i,i))
  PM2.5data.df <- PM2.5data.df %>%
    bind_cols(PM2.5features.df, PM2.5data.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1))
  PM2.5hist.df <- rbind(PM2.5hist.df, PM2.5data.df)
}
PM2.5hist.df <- PM2.5hist.df[-c(1), ]
PM2.5hist.df <- PM2.5hist.df %>%
  filter(TotalEDInpatientVisits==">=2")
#
MajRoadwayhist.df <- data.frame("TotalEDInpatientVisits" = c(0), "MajorRoadwayHighwayExposure" = c(0), "frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0))
for (i in 1:6) {
  MajRoadwaydata.df <- as.data.frame(EDvisitsdatajson$"return value"$feature_matrix[[21]][[i]])
  RoadwayFeatures.df <- data.frame("TotalEDInpatientVisits" = c("<2",">=2"), "MajorRoadwayHighwayExposure" = c(i,i))
  MajRoadwaydata.df <- MajRoadwaydata.df %>%
    bind_cols(RoadwayFeatures.df, MajRoadwaydata.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1))
  MajRoadwayhist.df <- rbind(MajRoadwayhist.df, MajRoadwaydata.df)
}
MajRoadwayhist.df <- MajRoadwayhist.df[-c(1), ]
MajRoadwayhist.df <- MajRoadwayhist.df %>%
  filter(TotalEDInpatientVisits==">=2")
#
MajPM2.5data_ <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A22/feature_association2",
              body ='{
              "feature_a": {
              "MajorRoadwayHighwayExposure": [
              {
              "operator": "=",
              "value": 1
              },
              {
              "operator": "=",
              "value": 2
              },
              {
              "operator": "=",
              "value": 3
              },
              {
              "operator": "=",
              "value": 4
              },
              {
              "operator": "=",
              "value": 5
              },
              {
              "operator": "=",
              "value": 6
              }
              ]
              },
              "feature_b": {
              "MaxDailyPM2.5Exposure": [
              {
              "operator": "=",
              "value": 1
              },
              {
              "operator": "=",
              "value": 2
              },
              {
              "operator": "=",
              "value": 3
              },
              {
              "operator": "=",
              "value": 4
              },
              {
              "operator": "=",
              "value": 5
              }
              ]
              }
              }',
          accept_json(),
          content_type("application/json"),
          encode = "json")
MajPM2.5data_text <- content(MajPM2.5data_, "text")
#
MajPM2.5data_json <- fromJSON(MajPM2.5data_text, flatten = TRUE)
MajPM2.5data_json$"terms and conditions" <- NULL
MajPM2.5data_json$"version" <- NULL
MajRoadway.df <- data.frame("MajorRoadwayHighwayExposure" = c(1,2,3,4,5,6))
ICEES <- data.frame("frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0),"MajorRoadwayHighwayExposure" = c(0), "MaxDailyPM2.5Exposure" = c(0))
for(i in 1:5){
  MajPM2.5feature_matrix.df <- as.data.frame(MajPM2.5data_json$"return value"$feature_matrix[[i]])
  t <- MajPM2.5feature_matrix.df %>%
    bind_cols(MajRoadway.df, MajPM2.5feature_matrix.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1)) %>%
    mutate(MaxDailyPM2.5Exposure = i)
  t <- as.data.frame(t)
  ICEES <- rbind(ICEES, t)
}
ICEES <- ICEES[-c(1), ]
ICEES$column_percentage <- NULL
ICEES$row_percentage <- NULL
ICEES$total_percentage <- NULL
#
finalfreq.df <- data.frame("MaxDailyPM2.5Exposure" = c(0), "row_percentage" = c(0), "MajorRoadwayHighwayExposure" = c(0), "row_percentage1" = c(0))
for(i in 1:5){
  t <- PM2.5hist.df %>%
    filter(MaxDailyPM2.5Exposure == i)
  for(i in 1:6){
    s <- MajRoadwayhist.df %>%
      filter(MajorRoadwayHighwayExposure == i)
    r <- bind_cols(t, s)
    r$TotalEDInpatientVisits <- NULL
    r$TotalEDInpatientVisits1 <- NULL
    r$column_percentage <- NULL
    r$frequency <- NULL
    r$total_percentage <- NULL
    r$column_percentage1 <- NULL
    r$frequency1 <- NULL
    r$total_percentage1 <- NULL
    finalfreq.df <- rbind(finalfreq.df, r)
  }
}
finalfreq.df <- finalfreq.df[-c(1), ]
#
final <- data.frame("MaxDailyPM2.5Exposure" = c(0), "MajorRoadwayHighwayExposure" = c(0), "EDcount" = c(0))
PM2.5filler <- data.frame("MaxDailyPM2.5Exposure" = c(0), "row_percentage" = c(0), "MajorRoadwayHighwayExposure" = c(0), "row_percentage1" = c(0), "frequency" = c(0), "PM2.5freq" = c(0))
MajRoadwayfiller <- data.frame("MaxDailyPM2.5Exposure" = c(0), "row_percentage" = c(0), "MajorRoadwayHighwayExposure" = c(0), "row_percentage1" = c(0), "frequency" = c(0), "PM2.5freq" = c(0), "MajRoadwayfreq" = c(0))
q <- select(ICEES, frequency)
q <- as.data.frame(q)
p <- bind_cols(finalfreq.df, q)
for(i in 1:5){
  o <- p %>%
    filter(MaxDailyPM2.5Exposure == i) %>%
    mutate(PM2.5freq = frequency * row_percentage)
  PM2.5filler <- rbind(PM2.5filler, o)
}
for(i in 1:6){
  n <- PM2.5filler %>%
    filter(MajorRoadwayHighwayExposure == i) %>%
    mutate(MajRoadwayfreq = frequency * row_percentage1)
  MajRoadwayfiller <- rbind(MajRoadwayfiller, n)
}
final.df <- mutate(MajRoadwayfiller, EDcount = MajRoadwayfreq + PM2.5freq)
final.df <- final.df[-c(1), ]
#
x <- final.df$MajorRoadwayHighwayExposure
y <- final.df$MaxDailyPM2.5Exposure
z <- final.df$EDcount

cloud(EDcount~MajorRoadwayHighwayExposure+MaxDailyPM2.5Exposure, final.df,
  panel.3d.cloud=panel.3dbars,
  distance = 0,
  screen=list(z=100, x=-60),
  xlab = "Major Roadway Highway Exposure (Bins)",
  ylab = "Maximum Daily PM2.5 Exposure (Bins)",
  zlab = "Number of Patients with 2+ ED Inpatient Visits",
  zlim = range(0,800),
  col.facet = "grey",
  scales=list(arrows=FALSE),
  par.settings = list(axis.line = list(col = "transparent")))

