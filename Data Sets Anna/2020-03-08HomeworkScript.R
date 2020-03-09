##Homework for March 9th 

rm(list = ls())

#install.packages("tidyverse")
library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
install.packages(c("dplyr", "ggplot2"))
tidyverse_update()

##Scenario 1####

#Load the births data 

CountryBirths <- read_csv("~/Bio375/2020-03-08_BirthDataCountryPaired.csv")

#Calcualte Summary Statistics

births_summary <- CountryBirths %>%
  summarise(n_Difference= n(),
            mean_birth_Birth_difference = mean(Difference),
            sd_Birth_difference = sd(Difference),
            IQR_Birth_difference = IQR(Difference),
            var_Birth_difference = var(Difference),
            se_Birth_difference = sd(Difference)/sqrt(n()))

##Scenario 2####

lizard <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
Lizards <- read_csv("~/Bio375/chap12e3HornedLizards.csv")

lizard_summary <- Lizards %>%
  group_by(Survival) %>%
  summarise(n_lizard = n(),
            mean_horn = mean(squamosalHornLength),
            sd_horn = sd(squamosalHornLength),
            IQR_horn = IQR(squamosalHornLength),
            var_horn = var(squamosalHornLength),
            se_horn = sd(squamosalHornLength)/sqrt(n()))

##Scenario 3#####

#no calc nescessary

##Scenario 4####

#no calculation yet, potentially after we learn about unequal variances 
