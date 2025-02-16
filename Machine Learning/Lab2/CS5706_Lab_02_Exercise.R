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
#' ### Lab 02 - Introduction to R for ML
#' #### Exercise: linear regression
#' *Alessandro Pandini*  
#' Revision: 1.01
#' 
#' ***
#' 
## --------------------------------------------------------------------------------------------------------------------
# load external functions
source('CS5706_Lab_02_Functions.R')

#' 
#' #### 0. load required packages
## --------------------------------------------------------------------------------------------------------------------
### 0.1 load the psych library
library(psych)

#' 
#' #### 1. load data
## --------------------------------------------------------------------------------------------------------------------
### 1.1 load the data from the chickendeef.csv data set
chickenfeed <- read.csv("chickenfeed.csv")
### 1.2 create a cf_linseed dataset filtered for linseed feed
cf_linseed <- chickenfeed[chickenfeed$Feed=="linseed",]
### 1.3 create a cf_linseed_data dataset filtered for Chick < 36
cf_linseed_data <- cf_linseed[cf_linseed$Chick<36,]
### 1.4 create a cf_linseed_test dataset filtered for Chick >= 36
cf_linseed_test <- cf_linseed[cf_linseed$Chick>=36,]

#' 
#' #### 2. data exploration
## --------------------------------------------------------------------------------------------------------------------
### 2.1 save a scatterplots matrix for the cf_linseed_data
###     note: exclude the Feed variable
png("cf_linseed_data_scatterplot.png")
pairs.panels(cf_linseed_data[,-4])
dev.off()
### 2.2 calculate a correlation matrix for the cf_linseed_data
###     note: exclude the Feed variable
cf_linseed_data_cor <- cor(cf_linseed_data[,c("weight","Time")])
### 2.3 save the correlation matrix in a csv file
write.csv(cf_linseed_data_cor,"cf_linseed_data_cor.csv")

#' 
#' #### 3. model training
## --------------------------------------------------------------------------------------------------------------------
### 3.1 create a lm model cf_linseed_lm for weight over Time in cf_linseed_data
cf_linseed_lm <- lm(weight ~ Time, data=cf_linseed_data)
### 3.2 save the cf_linseed_lm object in a RData file
save(cf_linseed_lm,file = "cf_linseed_lm.RData")

#' 
#' #### 4. model evaluation
## --------------------------------------------------------------------------------------------------------------------
### 4.1 get summary statistics on the model
cf_linseed_lm_stats <- summary(cf_linseed_lm)
### 4.2 save the model summary object in a RData file
save(cf_linseed_lm_stats,file="cf_linseed_lm_stats.RData")
### 4.3 save the coefficients table in a csv file
write.csv(cf_linseed_lm$coefficients,file="cf_linseed_lm_coefficients.csv")
### 4.4 save the diagnostics plots for the lm model in a pdf file
pdf(file = "cf_linseed_lm_diagnostics.pdf")
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(cf_linseed_lm)
par(opar)
dev.off()

#' 
#' #### 5. prediction
## --------------------------------------------------------------------------------------------------------------------
### 5.1 run a prediction of weight for the cf_linseed_test using the cf_linseed_lm
cf_linseed_predict <- predict(cf_linseed_lm,cf_linseed_test["Time"])
### 5.2 add a column pred with the predicted values to the cf_linseed_test table
cf_linseed_test$pred <- cf_linseed_predict
### 5.3 save the updated cf_linseed_test table in a new csv file
write.csv(cf_linseed_test,file="cf_linseed_test.csv")
### 5.4 save a predicted VS actual plot in a pdf file
###     note: use the plotPredAct function
pdf(file="cf_linseed_predicted_vs_actual.pdf")
plotPredAct(cf_linseed_test$pred,cf_linseed_test$weight,"weight predicted",seq(0, 300, 20), seq(0, 300, 20))
dev.off()


