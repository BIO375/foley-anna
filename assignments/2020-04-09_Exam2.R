##Exam 2 Questions####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages(c("lubridate", "xml2"))
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

##Mother vaccine antibodies####
##load data 

baker <- read_csv("datasets/demos/baker.csv")

baker <- baker %>%
  mutate (difference = After - Before)

##Q17####

#1. Response = antibody concentration , Predictor = vaccination 

#2. statistical null: ud=0

#3. If all assumptions are met, alternative is that ud ≠ 0 

#4. 

# Do the data meet the assumption of normality?  Check with plots.

ggplot(baker) +
  geom_histogram(aes(difference), binwidth = 5)

ggplot(baker) +
  geom_boxplot(aes(x = '', y = difference))+
  stat_summary(aes(x = "", y = difference), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)


ggplot(baker)+
  geom_qq(aes(sample = difference))

##These data are terrible. Try mutating it with log but it did not change much 
baker <- baker %>%
  mutate (log1difference = log(difference+1))

ggplot(baker) +
  geom_histogram(aes(log1difference), binwidth = 0.7)

ggplot(baker) +
  geom_boxplot(aes(x = '', y = log1difference))

ggplot(baker)+
  geom_qq(aes(sample = log1difference))

##These data do not meet the assumptions for normality. The histogram is very left skewed and hard to find a binwidth that 
#even connects them. The box plot does not have the median in the center and it is very difference than the mean. There are
#3 outliers above the IQR and the whiskers are not even. The qq plot is not a straight line at all. Therefore, we need
#to perform the non-parametric sign test. 

# One-sample, Two-sided
SignTest(baker$difference, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

#There was a significant increase in the measured antibody concentration after the vaccination 
#(Sign Test, two-sided: S=18, number of difference =20, p-value =0.0004025)


##Q18####

install.packages("abd", repos="http://R-Forge.R-project.org")
library("abd")

algae <- AlgaeCO2

#1. Response = , Predictor = 
