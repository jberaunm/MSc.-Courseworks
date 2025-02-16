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
#' ###  Lab 04 - Exploratory Data Analysis in R
#' #### Exercise: EDA and PCA
#' *Alessandro Pandini*
#' Revision: 1.01
#' 
#' ***
#' 
#' #### 1. load data and summary statistics
## --------------------------------------------------------------------------------------------------------------------
### 1.1 load the data from the heptatlon.csv file and inspect it
###   the dataset is available also in the HSAUR package
heptatlon <- read.csv(file="heptatlon.csv")
str(heptatlon)
### 1.2 get a summary report of the variables
summary(heptatlon)
### 1.3 remove the score variable from the dataset
heptatlon_no_score <- heptatlon[,-8]
### 1.4 calculate person correlation coefficient for the dataset
cor(heptatlon_no_score)

#' 
#' #### 2. graphical analysis
## --------------------------------------------------------------------------------------------------------------------
### 2.1 create a vector s_variables of labels for:
###   hurdles, run200m, run800m
s_variables <- c("hurdles","run200m","run800m")
### 2.2 create a vector m_variables of labels for:
###   highjump, longjump, javelin
m_variables <- c("highjump","longjump","javelin")
### 2.3 generate three simple boxplots for s_variables, m_variables and shot
###   display all three plots on one page
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,3))
boxplot(heptatlon[s_variables])
boxplot(heptatlon[m_variables])
boxplot(heptatlon$shot)
par(opar)

#' 
#' #### 3. PCA
## --------------------------------------------------------------------------------------------------------------------
### 3.1 perform PCA on the dataset (without the score variable)
pc_heptatlon <- prcomp(heptatlon_no_score, center = T, scale. = T)

#' 
#' #### 4. Visual analysis of PCA results
## --------------------------------------------------------------------------------------------------------------------
### 4.1 calculate the proportion of explained variance (PEV) from the std values
pc_heptatlon_var <- pc_heptatlon$sdev^2
pc_heptatlon_PEV <- pc_heptatlon_var / sum(pc_heptatlon_var)
### 4.2 plot the cumulative PEV
opar <- par(no.readonly = TRUE)
plot(
  cumsum(pc_heptatlon_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
par(opar)
### 4.3 get and inspect the loadings
pc_heptatlon_loadings <- pc_heptatlon$rotation
pc_heptatlon_loadings
### 4.4 generate a biplot for PC1 and PC2
opar <- par(no.readonly = TRUE)
biplot(
  pc_heptatlon,
  scale = 0,
  col = c('grey40','orange')
)
par(opar)

