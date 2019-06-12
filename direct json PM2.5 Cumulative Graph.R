# Rose Wang
# June 11, 2019
# PM 2.5 Cumulative Graph
# Pulling json file from API directly in R
# Pulling txt file from API and loading into R
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
data_ <- POST("https://icees.renci.org/1.0.0/patient/2010/cohort/COHORT%3A22/feature_association2",
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
          "AvgDailyPM2.5Exposure": [
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
data_text <- content(data_, "text")
#
data_json <- fromJSON(data_text, flatten = TRUE)
data_json$"terms and conditions" <- NULL
data_json$"version" <- NULL
data_json$"return value"$chi_squared <- NULL
data_json$"return value"$rows <- NULL
data_json$"return value"$feature_a <- NULL
data_json$"return value"$feature_b <- NULL
data_json$"return value"$p_value <- NULL
data_json$"return value"$total <- NULL
#
b <- c(1.5769518146, 9.2554972533, 9.6163242534, 9.6292716185, 10.32857036, 17.32892261)
MajRoadway.df <- data.frame("MajorRoadwayHighwayExposure" = c(1,2,3,4,5,6))
ICEES <- data.frame("frequency" = c(0), "MajorRoadwayHighwayExposure" = c(0), "Avg24hPM2.5Exposure" = c(0), "low" = c(0), "high" = c(0))
for(i in 1:5){
  feature.df <- as.data.frame(data_json$"return value"$feature_matrix[[i]])
  feature.df <- subset(feature.df, select = -c(column_percentage, row_percentage, total_percentage))
  t <- feature.df %>%
    bind_cols(feature.df, MajRoadway.df) %>%
    subset(select = -c(frequency1)) %>%
    mutate(Avg24hPM2.5Exposure = i) %>%
    mutate(low = frequency*b[i]) %>%
    mutate(high = frequency*b[i+1])
  t <- as.data.frame(t)
  ICEES <- rbind(ICEES, t)
}
ICEES <- ICEES[-c(1), ]
#
cumICEES <- data.frame("MajorRoadwayHighwayExposure" = c(0), "l" = c(0), "h" = c(0), "pop" = c(0))
for(i in 1:6) {
  lsum <- ICEES %>%
    filter(MajorRoadwayHighwayExposure==i) %>%
    summarise(sum(low))
  low <- pull(lsum, "sum(low)")
  hsum <- ICEES %>%
    filter(MajorRoadwayHighwayExposure==i) %>%
    summarize(sum(high))
  high <- pull(hsum, "sum(high)")
  pop <- ICEES %>%
    filter(MajorRoadwayHighwayExposure == i) %>%
    summarise(sum(frequency))
  pop <- pull(pop, "sum(frequency)")
  a <- data.frame("MajorRoadwayHighwayExposure" = c(as.numeric(i)), "l" = c(low), "h" = c(high), "pop" = c(pop))
  cumICEES <- rbind(cumICEES, a)
}
cumICEES <- cumICEES[-c(1), ]
# 
cumICEES <- cumICEES %>%
  mutate(newl = l/pop) %>%
  mutate(newh = h/pop) %>%
  mutate(avg = (newl+newh)/2)
#
ICEES1 <- read.delim(file="response_1559842883888.txt",sep="\t", header = FALSE)
colnames(ICEES1) <- c("MajorRoadwayHighwayExposure1","Avg24hPM2.5Exposure1","Frequency1","N")
ICEES1 <- subset(ICEES1, select = -c(N))
#
p1 <- data.frame("MajorRoadwayHighwayExposure1" = c(0), "Avg24hPM2.5Exposure1" = c(0), "Frequency1" = c(0), "low1" = (0), "high1" = (0))
for(i in 1:5){
  t1 <- ICEES1 %>%
    filter(Avg24hPM2.5Exposure1 == i) %>%
    mutate(low1 = Frequency1*b[i]) %>%
    mutate(high1 = Frequency1*b[i+1])
  as.data.frame(t1)
  p1 <- rbind(p1, t1)
}
#
cumICEES1 <- data.frame("MajorRoadwayHighwayExposure1" = c(0), "l1" = c(0), "h1" = c(0), "pop1" = c(0))
for(i in 1:6) {
  lsum1 <- p1 %>%
    filter(MajorRoadwayHighwayExposure1 == i) %>%
    summarise(sum(low1))
  low1 <- pull(lsum1, "sum(low1)")
  hsum1 <- p1 %>%
    filter(MajorRoadwayHighwayExposure1 == i) %>%
    summarize(sum(high1))
  high1 <- pull(hsum1, "sum(high1)")
  pop1 <- ICEES1 %>%
    filter(MajorRoadwayHighwayExposure1 == i) %>%
    summarise(sum(Frequency1))
  pop1 <- pull(pop1, "sum(Frequency1)")
  a1 <- data.frame("MajorRoadwayHighwayExposure1" = c(as.numeric(i)), "l1" = c(low1), "h1" = c(high1), "pop1" = c(pop1))
  cumICEES1 <- rbind(cumICEES1, a1)
}
cumICEES1 <- cumICEES1[-c(1), ]
# 
cumICEES1 <- cumICEES1 %>%
  mutate(newl1 = l1/pop1) %>%
  mutate(newh1 = h1/pop1) %>%
  mutate(avg1 = (newl1+newh1)/2)
#
ggplot() + 
  geom_line(data = cumICEES1, aes(x = MajorRoadwayHighwayExposure1, y = newl1), color = "red") + 
  geom_line(data = cumICEES1, aes(x = MajorRoadwayHighwayExposure1, y = newh1), color = "red") +
  geom_ribbon(data = cumICEES1, aes(x = MajorRoadwayHighwayExposure1, ymin = newl1, ymax = newh1), fill = "red", alpha = "0.5") +
  geom_line(data = cumICEES1, aes(x = MajorRoadwayHighwayExposure1, y = avg1), color = "red") +
  geom_line(data = cumICEES, aes(x = MajorRoadwayHighwayExposure, y = newl), color = "blue") + 
  geom_line(data = cumICEES, aes(x = MajorRoadwayHighwayExposure, y = newh), color = "blue") +
  geom_ribbon(data = cumICEES, aes(x = MajorRoadwayHighwayExposure, ymin = newl, ymax = newh), fill = "blue", alpha = "0.5") +
  geom_line(data = cumICEES, aes(x = MajorRoadwayHighwayExposure, y = avg), color = "blue") +
  ggtitle("Year-long Cumulative PM 2.5 Exposure Levels vs. Distance from Roadway") +
  xlab("Major Roadway Highway Exposure (Bins)") +
  ylab("Cumulative Average Daily PM 2.5 Exposure")

