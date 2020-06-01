## history from the tangerine repo some function examples
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(tidyverse)
library(MASS)
load("RData/tri_df.RData")
install.packages("rlist")
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(jsonlite)
library(httr)
cohort.id <- get_cohort_id("{}")
knitr::opts_chunk$set(echo = TRUE)
library(rlist)
library(httr)
httr::set_config(config(ssl_verifypeer = 0L))
get_cohort = function(cohort_def) {
  cohort_url <- "https://icees.renci.org:16339/patient/2010/cohort"
  cohort.call <- httr::POST(cohort_url,
                            body=cohort_def,
                            accept_json(),
                            content_type("application/json"),
                            encode = "json")
  cohort.text <- httr::content(cohort.call, "text")
  cohort <- jsonlite::fromJSON(cohort.text)$`return value`
  return(cohort)
}
get_cohort_id = function(cohort_def) get_cohort(cohort_def)$`cohort_id`
EDVisits = list(
  list(operator="=",value=0),
  list(operator="=",value=1),
  list(operator="=",value=2),
  list(operator="=",value=3),
  list(operator="=",value=4),
  list(operator="=",value=5),
  list(operator="=",value=6),
  list(operator="=",value=7),
  list(operator="=",value=8),
  list(operator="=",value=9),
  list(operator=">=",value=10)
)
AvgExposure = list(
  list(operator="=",value=1),
  list(operator="=",value=2),
  list(operator="=",value=3),
  list(operator="=",value=4),
  list(operator="=",value=5)
)
Race = list(
  list(operator="=", value="Native Hawaiian/Pacific Islander"),
  list(operator="=", value="Caucasian"),
  list(operator="=", value="African American"),
  list(operator="=", value="Asian"),
  list(operator="=", value="Unknown"),
  list(operator="=", value="American/Alaskan Native"),
  list(operator="=", value="Other")
)
Sex2 = list(
  list(operator="=", value="Male"),
  list(operator="=", value="Female")
)
ED=list(vals=EDVisits, name="TotalEDInpatientVisits", expr=expr(TotalEDInpatientVisits))
RACE=list(vals=Race, name="Race", expr=expr(Race))
PM25=list(vals=AvgExposure, name="AvgDailyPM2.5Exposure_StudyAvg_qcut", expr=expr(`AvgDailyPM2.5Exposure_StudyAvg_qcut`))
SEX2=list(vals=Sex2, name="Sex2", expr=expr(Sex2))
cell = function(x) if (x$operator == "=") x$value else paste0(x$operator,x$value)
univariate = function(year, cohort_id, a) {
  b = SEX2
  adata = list()
  adata[[a$name]] = a$vals
  bdata = list()
  bdata[[b$name]] = b$vals
  body_bivar = toJSON(list(feature_a = adata, feature_b = bdata), auto_unbox = TRUE)
  bivar_call <- httr::POST(paste0("https://icees.renci.org:16339/patient/", year, "/cohort/", cohort_id, "/feature_association2"),
                           body = body_bivar,
                           accept_json(),
                           content_type("application/json"),
                           encode = "json")
  bivar_text <- httr::content(bivar_call, "text")
  bivar_list <- jsonlite::fromJSON(bivar_text, flatten = TRUE)$`return value`
  fmatrix = bivar_list$columns
  univar_df = setNames(data.frame(unlist(map(a$vals, cell), use.names = FALSE),
                                  unlist(fmatrix$frequency, use.names = FALSE)), c(a$name, "Frequency"))
  return(univar_df)
}
bivariate = function(year, cohort_id, a, b) {
  adata = list()
  adata[[a$name]] = a$vals
  bdata = list()
  bdata[[b$name]] = b$vals
  body_bivar = toJSON(list(feature_a = adata, feature_b = bdata), auto_unbox = TRUE)
  bivar_call <- httr::POST(paste0("https://icees.renci.org:16339/patient/", year, "/cohort/", cohort_id, "/feature_association2"),
                           body = body_bivar,
                           accept_json(),
                           content_type("application/json"),
                           encode = "json")
  bivar_text <- httr::content(bivar_call, "text")
  bivar_list <- jsonlite::fromJSON(bivar_text, flatten = TRUE)$`return value`
  fmatrix = bivar_list$feature_matrix
  mlist = mapply(function(bval, fmat) {
    avals = unlist(map(a$vals, cell), use.names = FALSE)
    bvalue = cell(bval)
    freq = fmat$frequency
    temp_df <- setNames(data.frame(avals,
                                   bvalue,
                                   freq), c(a$name, b$name, "Frequency"))
    return(temp_df)
  }, b$vals, fmatrix, SIMPLIFY = FALSE)
  bivar_df = list.rbind(mlist)
  return(bivar_df)
}
trivariate = function(year, a, b, c) {
  tri_cohorts <- unlist(map(a$vals, function(val) {
    tri_body_tmp = list()
    tri_body_tmp[[a$name]] = val
    tri_cohort <- get_cohort_id(toJSON(tri_body_tmp, auto_unbox = TRUE))
    return(tri_cohort)
  }), use.names = FALSE)
  tri_df <- list.rbind(mapply(function(val, tri_cohort) cbind(setNames(data.frame(valueName(val)), a$name), bivariate(year, tri_cohort, b, c)), a$vals, tri_cohorts, SIMPLIFY = FALSE))
  return(tri_df)
}
tri_df
cohort.id <- get_cohort_id("{}")
cohort.id <- get_cohort_id("{}")
univar_PM25_df = univariate(2010, cohort.id, PM25)
univar_PM25_df
univar_RACE_df = univariate(2010, cohort.id, RACE)
univar_RACE_df
univar_ED_df = univariate(2010, cohort.id, ED)
univar_ED_df
save(univar_ED_df, file = "RData/univar_ED_df.RData")
save(univar_PM25_df, file = "RData/univar_PM25_df.RData")
save(univar_RACE_df, file = "RData/univar_RACE_df.RData")
## no need to see uninformative messages in the markdown document
url <- paste0("https://icees.renci.org:16339/patient/2010/cohort/", cohort.id, "/features")
## extract the data in json format
univariate.call <- httr::GET(url,
                             accept_json(),
                             content_type("application/json"),
                             encode = "json")
## the next line of code extracts just the content as a character string
univariate.text <- httr::content(univariate.call, "text")
## convert the output to JSON format
## note that this contains all of the variables in the dataset
univariate.list <- jsonlite::fromJSON(univariate.text, flatten = TRUE)$`return value`
names(univariate.list)
univariate.list$feature_matrix[[1]]
cat("Number of Features = ", length(univariate.list$feature.feature_name),"\n")
univariate.list$feature.feature_name[1:5]
univariate.list$feature.feature_qualifiers[[1]]
univariate.list$feature.feature_qualifiers[[2]]
## the following function combines the results for one feature
make_df <- function(i) {
  data.frame(feature=univariate.list$feature.feature_name[i],
             univariate.list$feature.feature_qualifiers[i],
             univariate.list$feature_matrix[[i]])
}
## this code loops through the entire list and returns a single data frame
## pdh: should remember how to do this more elegantly!
list.rbind(map(1:length(univariate.list[[1]]), make_df))
uni_all_df[1:8,]
uni_all_df[1:8,]
dim(uni_all_df)
