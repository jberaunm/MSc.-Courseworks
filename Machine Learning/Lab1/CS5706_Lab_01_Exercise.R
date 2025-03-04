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
#' ### Lab 01 - Introduction to R for ML  
#' #### Exercise: data exploration
#' *Alessandro Pandini*  
#' Revision: 1.01
#' 
#' ***
#' 
## --------------------------------------------------------------------------------------------------------------------
### 1. load the data from the chickenfeed.csv file
dfchikenfeed <- read.csv("chickenfeed.csv")

#' 
## --------------------------------------------------------------------------------------------------------------------
### 2. inspect the structure of the data
str(dfchikenfeed)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 3. get a summary table of each numerical variables
summary(dfchikenfeed$weight)
summary(dfchikenfeed$Time)
summary(dfchikenfeed$Chick)
# alternative solution:
apply(dfchikenfeed[,c("weight","Time","Chick")], 2, summary)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 4. create a new data frame cf21 containing only data for Time == 21
cf21 <- dfchikenfeed[dfchikenfeed$Time==21,]

#' 
## --------------------------------------------------------------------------------------------------------------------
### 5. calculate mean, median and sd of the weight variable in cf21
mean(cf21$weight)
median(cf21$weight)
sd(cf21$weight)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 6. plot the weight distribution in cf21 as a boxplot
boxplot(cf21$weight)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 7. plot the weight distribution in cf21 as a histogram
hist(cf21$weight)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 8. plot the weight distribution in cf21 by Feed as a boxplot
boxplot(weight ~ Feed,cf21)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 9. repeat steps 4-8 for a data frame cf8 filtered by Time == 8
cf8 <- dfchikenfeed[dfchikenfeed$Time==8,]
mean(cf8$weight)
median(cf8$weight)
sd(cf8$weight)
boxplot(cf8$weight)
hist(cf8$weight)
boxplot(weight ~ Feed,dfchikenfeed)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 10. get a frequency table for the Feed variable in cf21
table(cf21$Feed)

#' 
## --------------------------------------------------------------------------------------------------------------------
### 11. calculate the mean weight for each subgroups of Feed in cf21
mean(cf21[cf21$Feed=="casein",c("weight")])
mean(cf21[cf21$Feed=="horsebean",c("weight")])
mean(cf21[cf21$Feed=="linseed",c("weight")])
mean(cf21[cf21$Feed=="oatmeal",c("weight")])

#' 
## --------------------------------------------------------------------------------------------------------------------
### 12. plot the weight distribution as histogram for the most effective Feed
hist(cf21[cf21$Feed=="linseed",c("weight")])

#' 
## --------------------------------------------------------------------------------------------------------------------
### 13. count the number of individuals with weight over 250 at Time == 21
nrow(cf21[cf21$weight>250,])

#' 
## --------------------------------------------------------------------------------------------------------------------
### 14. write the cf21 and cf8 into two text files
write.table(cf21,file="cf21.dat")
write.table(cf8,file="cf8.dat")

