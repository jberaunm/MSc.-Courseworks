#' ---
#' title: "R Notebook"
#' output:
#'   html_document:
#'     df_print: paged
#'   pdf_document: default
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 
## ----include=FALSE---------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(comment = '>')

#' 
#' ### CS5706
#' ### Lab03 - Data Preparation and Cleaning in R
#' #### Exercise: Data Preparation and Cleaning
#' *Alessandro Pandini*
#' Revision: 1.01
#' 
#' ***
#' 
#' #### 1. load data
## --------------------------------------------------------------------------------------------------------------------
### 1.1 load the data from the airquality.csv file and inspect it
dfairquality <- read.csv("air_dataset.csv",na.strings = c("NA", ""))
str(dfairquality)

#' 
#' #### 2. handling missing values
## --------------------------------------------------------------------------------------------------------------------
### 2.1 identify features with missing values
###   by looking at the data frame summary
summary(dfairquality)
### 2.2 calculate and inspect a table of the percentage of NA per feature
dfairquality_NAs <- round(apply(is.na(dfairquality),2,sum) / dim(dfairquality)[1] * 100,1)
dfairquality_NAs
### 2.3 impute missing Ozone values by replacement with the median value
meanOzone <- mean(dfairquality$Ozone,na.rm = T)
dfairquality[is.na(dfairquality$Ozone),"Ozone"] <- meanOzone
### 2.4 remove instances with missing Solar.R values
dfairquality_noNA <- na.omit(dfairquality)

#' 
#' #### 3. handling duplicate instances
## --------------------------------------------------------------------------------------------------------------------
### 3.1 remove duplicate instances from the data set obtained from 2.4
dfairquality_nodup <- unique(dfairquality_noNA)

#' 
#' #### 4. handling outliers
## --------------------------------------------------------------------------------------------------------------------
### 4.1 remove instances with outlier values for Ozone
dfairquality_out <- boxplot(dfairquality_nodup$Ozone)
min(dfairquality_out$out)
dfairquality_clean <- dfairquality_nodup[dfairquality_nodup$Ozone<85,]

#' 
#' #### 5. preparing training/test set
## --------------------------------------------------------------------------------------------------------------------
### 5.1 split the cleaned data frame in training/test set (70% / 30%) x4
n_rows <- nrow(dfairquality_clean)
index0.7 <- sample(n_rows,n_rows*0.7)
dfairquality_data <- dfairquality_clean[index0.7,]
dfairquality_test <- dfairquality_clean[-index0.7,]

