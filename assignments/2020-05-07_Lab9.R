# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

library("ggfortify")

library("broom")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

##Chapter 16 Problems####
#  (5) 7, 10


##16-5,7####
godwits <- read_csv("datasets/abd/chapter16/chap16q05GodwitArrivalDates.csv")

# Check for the assumption of bivariate normality using a basic scatter 
# plot

ggplot(data = godwits) +
  geom_point(mapping = aes(x = femaleDate, y = maleDate))

##Check individual graphs with a boxplot 

ggplot(data = godwits)+
  geom_boxplot(aes("", femaleDate))
ggplot(data = godwits)+
  geom_qq(aes(sample = femaleDate))


ggplot(data = godwits)+
  geom_boxplot(aes("", maleDate))
ggplot(data = godwits)+
  geom_qq(aes(sample = maleDate))

##Data is normal, we are good. 

##Create correlation 

godwitsCor <- cor.test(~ femaleDate + maleDate, data = godwits,
                     method = "pearson")
godwitsCor

##correlation coeffiecient is 0.927

##95% confidence is 0.715-0.983

##7 Check if their mean arrival dates differ 

##use paired t test 

t.test(godwits$femaleDate, godwits$maleDate, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
## t =-0.180 df =9 p value = 0.8612 

##Therefore there is no significant differance between the arrival dates 




##16-10####

earwig <- read_csv("Data Sets Anna/ch16q10.csv")

# Check for the assumption of bivariate normality using a basic scatter 
# plot

ggplot(data = earwig) +
  geom_point(mapping = aes(x = prop_forceps, y = earwig_density))

##Check individual graphs with a boxplot 

ggplot(data = earwig)+
  geom_boxplot(aes("", prop_forceps))
ggplot(data = earwig)+
  geom_qq(aes(sample = prop_forceps))


ggplot(data = earwig)+
  geom_boxplot(aes("", earwig_density))
ggplot(data = earwig)+
  geom_qq(aes(sample = earwig_density))

##Data would probably be less normal if there were less data points like to begin with, so we will test spearman's rank 

##Create correlation 

earwigCor <- cor.test(~ prop_forceps + earwig_density, data = earwig,
                       method = "spearman")
earwigCor

##rho = 0.661, p value = 0.000802

##there is a relationship 

##assumptions include 
#1.random sample
#2.relationship between 2 ranks is linear 

##Chapter 17-6####

zoo <- read_csv("datasets/abd/chapter17/chap17q06ZooMortality.csv")
