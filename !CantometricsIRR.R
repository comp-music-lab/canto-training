#This code performs inter-rater reliability analyses for Cantometric training codings from CompMusic Lab students

#This code requires installing and loading the following packages:
#Install:
install.packages("irr")
install.packages("psych")
install.packages("googlesheets4")

#Load:
library(irr)
library(psych)
library(googlesheets4)

options(stringsAsFactors = FALSE)

#Read data from Google Sheet
(d <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1vCA6P92w71z1lAIz_EMDrp-qplUkqpC_aGdXLc0OfQM/edit#gid=0&output=csv")))

#Recode text as numbers used in Cantometrics input
d[ ][ d[ ] == "Very High" ] <- 1
d[ ][ d[ ] == "Very high" ] <- 1
d[ ][ d[ ] == "very high" ] <- 1
d[ ][ d[ ] == "High" ] <- 4
d[ ][ d[ ] == "high" ] <- 4
d[ ][ d[ ] == "Mid" ] <- 7
d[ ][ d[ ] == "mid" ] <- 7
d[ ][ d[ ] == "Low" ] <- 10
d[ ][ d[ ] == "low" ] <- 10
d[ ][ d[ ] == "very low" ] <- 13
d[ ][ d[ ] == "Very low" ] <- 13
d[ ][ d[ ] == "Very Low" ] <- 13

(d[ , 2:9] <- as.data.frame(apply(d[ , 2:9], 2, as.numeric))) # Recode characters as numeric

#normalize scale from 0-1
normalize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
d[,3:8] <- lapply(d[3:8], normalize,na.rm=TRUE)

#Calculate pairwise reliability vs "correct" answer (column number 3). Test case: Yuchen (column number 5)
d[,c(3,5)] #show columns 3 and 5
table(d[,c(3,5)]) #show contingency table 
agree(d[,c(3,5)]) #calculate percent agreement
agree(d[,c(3,5)],tolerance=0.25) #calculate percent agreement with tolerance of 1 scale point
psych::cohen.kappa(d[,c(3,5)]) #Calculate Cohen's Kappa (both unweighted and weighted [squared weighting by default])
psych::alpha(d[,c(3,5)]) #Calculate Cronbach's alpha

#Calculate average reliability across all raters (columns number 3-8).
agree(d[,3:8]) # percent agreement across all 6 raters = 33% (NB: this means ALL 6 raters agree for 3/9 songs)
agree(d[,3:8],tolerance=.25) # percent agreement across all 6 raters with tolerance of 1 scale point = 100% (NB: this means ALL 6 raters are within 1 point for all songs)
psych::cohen.kappa(d[,3:8]) #Calculate Light's Kappa (i.e., average of all pairwise Cohen's Kappas) (both unweighted and weighted [squared weighting by default])
psych::alpha(d[,3:8]) #Calculate Cronbach's alpha
