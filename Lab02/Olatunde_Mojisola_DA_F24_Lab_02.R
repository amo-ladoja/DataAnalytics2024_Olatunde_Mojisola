library(ggplot2)
setwd("~/ITWS-1100/DataAnalytics2024_Olatunde_Mojisola/Lab02")
### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

##view data
View(epi.results)
View(epi.weights)


#### Exploratory Analysis ####
epi.results$EPI.new

epi.results[1,5]

attach(epi.results)

EPI.new
EPI.new[1]


## NA values
na.indices <- is.na(EPI.new)


## drop NAs
EPI.new.compl <- EPI.new [!na.indices]


## convert to data frame and add country
EPI.new.compl <- data.frame