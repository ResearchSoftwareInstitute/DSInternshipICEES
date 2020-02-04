# Rose Wang
# June 13, 2019
# ICEES Fig.3 - API calls
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
library(curlconverter)
#
data_ <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A22/associations_to_all_features",
              body = '{"feature":{"TotalEDInpatientVisits":{"operator":"<","value":2}},"maximum_p_value":0.1}',
              accept_json(),
              content_type("application/json"),
              encode = "json")
data_text <- content(data_, "text")
#
data_json <- fromJSON(data_text, flatten = TRUE)
hist.df <- data.frame("TotalEDInpatientVisits" = c(0), "MaxDailyPM2.5Exposure" = c(0), "frequency" = c(0), "column_percentage" = c(0), "row_percentage" = c(0), "total_percentage" = c(0))
for (i in 1:5) {
  data.df <- as.data.frame(data_json$"return value"$feature_matrix[[10]][[i]])
  features.df <- data.frame("TotalEDInpatientVisits" = c("<2",">=2"), "MaxDailyPM2.5Exposure" = c(i,i))
  data.df <- data.df %>%
    bind_cols(features.df, data.df) %>%
    subset(select = -c(frequency1, column_percentage1, row_percentage1, total_percentage1))
  hist.df <- rbind(hist.df, data.df)
}
hist.df <- hist.df[-c(1), ]
hist.df <- hist.df %>%
  filter(TotalEDInpatientVisits==">=2") %>%
  mutate(row_percent100 = 100*row_percentage)
#
ggplot(data = hist.df, aes(x = MaxDailyPM2.5Exposure, y = row_percent100)) +
  geom_histogram(stat = "identity") +
  labs( x = "Maximum Daily PM 2.5 Exposure (Bins)", y = "% Patients with 2+ ED or Inpatient Visits/Year for Respiratory Issues")






