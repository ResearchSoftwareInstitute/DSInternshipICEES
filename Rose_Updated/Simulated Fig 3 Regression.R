# Rose Wang
# June 13, 2019
# Extended ICEES Fig.3 - Regression from Simulated Data
# Roadway Distance, Max Daily PM 2.5 Exposure, Number of ED Patients
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
library(reshape2)
library(lattice)
library(Hmisc)
library(rms)
library(mefa)
library(scatterplot3d)
#
PM2.5data <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A77/feature_association2",
                     body ='{
                     "feature_a": {
                     "TotalEDInpatientVisits": [
                     {
                     "operator": "=",
                     "value": 0
                     }, {
                     "operator": "=",
                     "value": 1
                     }, {
                     "operator": "=",
                     "value": 2
                     },{
                     "operator": "=",
                     "value": 3
                      },{
                     "operator": "=",
                    "value": 4
                     }, {
                     "operator": "=",
                     "value": 5
                     }, {
                     "operator": "=",
                     "value": 6
                     }, {
                     "operator": "=",
                     "value": 7
                     }, {
                     "operator": "=",
                     "value": 8
                     }, {
                     "operator": "=",
                      "value": 9
                     }
                     ]
                     },
                     "feature_b": {
                     "MaxDailyPM2.5Exposure_qcut": [
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
PM2.5datatext <- content(PM2.5data, "text")
#
PM2.5data_json <- fromJSON(PM2.5datatext, flatten = TRUE)
PM2.5data_json$"terms and conditions" <- NULL
PM2.5data_json$"version" <- NULL
features.df <- data.frame("EDvisits" = c(0,1,2,3,4,5,6,7,8,9))
PM2.5.df <- data.frame("frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0),"EDvisits" = c(0), "MaxDailyPM2.5Exposure_qcut" = c(0))
for(i in 1:5){
  PM2.5feature_matrix.df <- as.data.frame(PM2.5data_json$"return value"$feature_matrix[[i]])
  t <- PM2.5feature_matrix.df %>%
    bind_cols(features.df, PM2.5feature_matrix.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1)) %>%
    mutate(MaxDailyPM2.5Exposure_qcut = i)
  t <- as.data.frame(t)
  PM2.5.df <- rbind(PM2.5.df, t)
}
PM2.5.df <- PM2.5.df[-c(1), ]
#
MajRoadwaydata <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A77/feature_association2",
                  body ='{
                  "feature_a": {
                  "TotalEDInpatientVisits": [
                     {
                  "operator": "=",
                  "value": 0
                  }, {
                  "operator": "=",
                  "value": 1
                  }, {
                  "operator": "=",
                  "value": 2
                  },{
                  "operator": "=",
                  "value": 3
                  },{
                  "operator": "=",
                  "value": 4
                  }, {
                  "operator": "=",
                  "value": 5
                  }, {
                  "operator": "=",
                  "value": 6
                  },
                  {
                  "operator": "=",
                  "value": 7
                  }, {
                  "operator": "=",
                  "value": 8
                  }, {
                  "operator": "=",
                  "value": 9
                  }
                  ]
                  },
                  "feature_b": {
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
                  }
                  ]
                  }
                  }',
          accept_json(),
          content_type("application/json"),
          encode = "json")
MajRoadwaydatatext <- content(MajRoadwaydata, "text")
#
MajRoadwaydata_json <- fromJSON(MajRoadwaydatatext, flatten = TRUE)
MajRoadwaydata_json$"terms and conditions" <- NULL
MajRoadwaydata_json$"version" <- NULL
features.df <- data.frame("EDvisits" = c(0,1,2,3,4,5,6,7,8,9))
MajRoadway.df <- data.frame("frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0),"EDvisits" = c(0), "MajorRoadwayHighwayExposure" = c(0))
for(i in 1:5){
  MajRoadwayfeature_matrix.df <- as.data.frame(MajRoadwaydata_json$"return value"$feature_matrix[[i]])
  t <- MajRoadwayfeature_matrix.df %>%
    bind_cols(features.df, MajRoadwayfeature_matrix.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1)) %>%
    mutate(MajorRoadwayHighwayExposure = i)
  t <- as.data.frame(t)
  MajRoadway.df <- rbind(MajRoadway.df, t)
}
MajRoadway.df <- MajRoadway.df[-c(1), ]
#
MajPM2.5data_ <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A77/feature_association2",
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
                      }
                      ]
                      },
                      "feature_b": {
                      "MaxDailyPM2.5Exposure_qcut": [
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
features.df <- data.frame("MajorRoadwayHighwayExposure" = c(1,2,3,4,5))
MajPM2.5.df <- data.frame("frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0),"MajorRoadwayHighwayExposure" = c(0), "MaxDailyPM2.5Exposure_qcut" = c(0))
for(i in 1:5){
  MajPM2.5feature_matrix.df <- as.data.frame(MajPM2.5data_json$"return value"$feature_matrix[[i]])
  t <- MajPM2.5feature_matrix.df %>%
    bind_cols(features.df, MajPM2.5feature_matrix.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1)) %>%
    mutate(MaxDailyPM2.5Exposure_qcut = i)
  t <- as.data.frame(t)
  MajPM2.5.df <- rbind(MajPM2.5.df, t)
}
MajPM2.5.df <- MajPM2.5.df[-c(1), ]
#
RoadwayWorking.df <- data.frame("MajorRoadwayHighwayExposure" = c(0), "RoadwayED" = c(0))
PM2.5Working.df <- data.frame("MaxDailyPM2.5Exposure_qcut" = c(0), "PM2.5ED" = c(0))
for(i in 1:5) {
  RoadwayEDsum <- MajRoadway.df %>%
    filter(MajorRoadwayHighwayExposure==i) %>%
    mutate(RoadwayED = frequency*EDvisits) %>%
    summarise(sum(RoadwayED))
  RoadwayED <- pull(RoadwayEDsum, "sum(RoadwayED)")
  PM2.5EDsum <- PM2.5.df %>%
    filter(MaxDailyPM2.5Exposure_qcut==i) %>%
    mutate(PM2.5ED = frequency*EDvisits) %>%
    summarise(sum(PM2.5ED))
  PM2.5ED <- pull(PM2.5EDsum, "sum(PM2.5ED)")
  a <- data.frame("MajorRoadwayHighwayExposure" = c(as.numeric(i)),"RoadwayED" = c(RoadwayED))
  b <- data.frame("MaxDailyPM2.5Exposure_qcut" = c(as.numeric(i)), "PM2.5ED" = c(PM2.5ED))
  RoadwayWorking.df <- rbind(RoadwayWorking.df, a)
  PM2.5Working.df <- rbind(PM2.5Working.df, b)
}
RoadwayWorking.df <- RoadwayWorking.df[-c(1), ]
PM2.5Working.df <- PM2.5Working.df[-c(1), ]
#
PM2.5bounds <- c(6.768784523, 42.0219841, 46.209636688, 47.062618256, 51.719459534, 114.94243622)
Roadwaybounds <- c(0,50,100,200,300,500)
simulated.df <- data.frame("MajorRoadwayHighwayExposure" = c(0), "MaxDailyPM2.5Exposure_qcut" = c(0), "sMajorRoadwayHighwayExposure" = c(0), "sMaxDailyPM2.5Exposure" = c(0))
for (i in 1:5){
  simPM2.5 <- MajPM2.5.df %>%
    filter(MaxDailyPM2.5Exposure_qcut == i)
  for (n in 1:5){
    simRoadway <- simPM2.5 %>%
      filter(MajorRoadwayHighwayExposure == n)
    freq <- simRoadway$frequency
    sMajorRoadwayHighwayExposure <- runif(freq, min=Roadwaybounds[n], max=Roadwaybounds[n+1])
    sMajorRoadwayHighwayExposure <- as.data.frame(sMajorRoadwayHighwayExposure)
    sMaxDailyPM2.5Exposure <- runif(freq, min=PM2.5bounds[i], max=PM2.5bounds[i+1])
    sMaxDailyPM2.5Exposure <- as.data.frame(sMaxDailyPM2.5Exposure)
    features <- data.frame("MajorRoadwayHighwayExposure" = c(as.numeric(n)), "MaxDailyPM2.5Exposure_qcut" = c(as.numeric(i)))
    features <- rep(features, times = freq)
    filler <- bind_cols(features, sMajorRoadwayHighwayExposure, sMaxDailyPM2.5Exposure)
    simulated.df <- rbind(simulated.df, filler)
  }
}
simulated.df <- simulated.df[-c(1), ]
#
a <- merge(MajPM2.5.df, RoadwayWorking.df, by = "MajorRoadwayHighwayExposure", all = TRUE, sort = FALSE)
a <- mutate(a, RoadwayEDcount = column_percentage*RoadwayED)
b <- merge(MajPM2.5.df, PM2.5Working.df, by = "MaxDailyPM2.5Exposure_qcut", all = TRUE, sort = FALSE)
b <- mutate(b, PM2.5EDcount = row_percentage*PM2.5ED)
EDcount.df <- merge(a, b)
EDcount.df <- mutate(EDcount.df, TotalEDCount = (RoadwayEDcount+PM2.5EDcount)/frequency)
#
TotalEDCount.df <- data.frame("MajorRoadwayHighwayExposure" = c(0), "MaxDailyPM2.5Exposure_qcut" = c(0), "TotalEDCount" = c(0))
scatter.df <- data.frame("MajorRoadwayHighwayExposure" = c(0), "MaxDailyPM2.5Exposure_qcut" = c(0), "sMajorRoadwayHighwayExposure" = c(0), "sMaxDailyPM2.5Exposure" = c(0))
EDcount.df$frequency <- NULL
EDcount.df$column_percentage <- NULL
EDcount.df$row_percentage <- NULL
EDcount.df$total_percentage <- NULL
EDcount.df$RoadwayED <- NULL
EDcount.df$RoadwayEDcount <- NULL
EDcount.df$PM2.5ED <- NULL
EDcount.df$PM2.5EDcount <- NULL
simulated.df <- simulated.df %>%
  arrange(MaxDailyPM2.5Exposure_qcut)
for(i in 1:5){
  simPM2.5 <- MajPM2.5.df %>%
    filter(MaxDailyPM2.5Exposure_qcut == i)
  PM2.5filler <- filter(EDcount.df, MaxDailyPM2.5Exposure_qcut == i)
  for(n in 1:5){
    simRoadway <- simPM2.5 %>%
      filter(MajorRoadwayHighwayExposure == n)
    freq <- simRoadway$frequency
    Roadwayfiller <- PM2.5filler %>%
      filter(MajorRoadwayHighwayExposure == n) %>%
      rep(times = freq)
    TotalEDCount.df <- bind_rows(Roadwayfiller, TotalEDCount.df)
  }
}
#
TotalEDCount.df <- TotalEDCount.df %>%
  arrange(MaxDailyPM2.5Exposure_qcut) %>%
  arrange(MajorRoadwayHighwayExposure)
TotalEDCount.df <- TotalEDCount.df[-c(1), ]
TotalEDCount.df <- TotalEDCount.df[colSums(!is.na(TotalEDCount.df)) >0]
simulated.df <- simulated.df %>%
  arrange(MaxDailyPM2.5Exposure_qcut) %>%
  arrange(MajorRoadwayHighwayExposure)
scatter.df <- bind_cols(TotalEDCount.df, simulated.df)
scatter.df$MajorRoadwayHighwayExposure <- NULL
scatter.df$MaxDailyPM2.5Exposure_qcut <- NULL
scatter.df$MajorRoadwayHighwayExposure1 <- NULL
scatter.df$MaxDailyPM2.5Exposure_qcut1 <- NULL
#
for(i in 0:90) {
  graphPath <- file.path("/Users","/rosewang","/Downloads","/RCompSci","/Simulated Fig 3 Regression",paste("angle ",i,".jpg",sep=""))
  jpeg(file = graphPath)
  s3d <- scatterplot3d(x = scatter.df$sMajorRoadwayHighwayExposure, 
                       y = scatter.df$sMaxDailyPM2.5Exposure, 
                       z = scatter.df$TotalEDCount,
                       pch = 20,
                       cex.symbols = .5,
                       color = "#E69F00",
                       xlab = "Major Roadway Highway Exposure",
                       ylab = "Maximum Daily PM2.5 Exposure", 
                       zlab = "Number of Inpatient Visits",
                       angle = i, 
                       box = FALSE)
  regression <- lm(scatter.df$TotalEDCount~scatter.df$sMajorRoadwayHighwayExposure+scatter.df$sMaxDailyPM2.5Exposure)
  s3d$plane3d(regression)
  dev.off()
}





