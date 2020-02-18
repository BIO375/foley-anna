#Anna Foley Lab 3
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

### Install and load packages ####
# The following commands will install these packages if they are not already installed, 
# and then load them!

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}
# install.packages("summarytools")
# library(summarytools)
# I give the general form below in comments.  A < > indicates you will type in
# your own value.  The main thing to watch for this command is the punctuation: ! ""

# if(!require(<package_name>)){install.packages("<package_name>")}

# Check for updates
tidyverse_update()

#Loading Lovett Data File from datasets folder 

lovett <- read_csv("datasets/quinn/chpt2/lovett.csv")


#Calculate summary statistics 

SO4 <- lovett %>%
summarise(mean_SO4 = mean(SO4),
            median_SO4 = median(SO4),
            IQR_SO4 = IQR(SO4),
            sd_SO4 = sd(SO4),
            var_SO4 = var(SO4))

SO4MOD <- lovett %>%
  summarise(mean_SO4MOD = mean(SO4MOD),
            median_SO4MOD = median(SO4MOD),
            IQR_SO4MOD = IQR(SO4MOD),
            sd_SO4MOD = sd(SO4MOD),
            var_SO4MOD = var(SO4MOD))

#Plotting Histograms of SO4 and Modified SO4

# Plot the level of SO4 and Modified SO4 as a histogram
#SO4 histogram
ggplot(lovett)+
  geom_histogram(aes(SO4), binwidth = 2)

#SO4 Modified Histogram
ggplot(lovett)+
  geom_histogram(aes(SO4MOD), binwidth = 3)

# Plot boxplots of SO4 and Modified SO4 using the code below.  
# You do not need to write any new code for this part!

# The code below modifies the dataset so it only contains SO4 and Modified SO4
# using select{dplyr}, and is oriented in long form using gather{tidyr}
lovett_tidy <- lovett %>%
  select(contains("SO4"))%>%
  gather(key = "type", value = "measurement", SO4, SO4MOD)

# The code below plots the two variables as boxplots, zooming in on the
# 40-75 range where most of the values are found (coord_cartesian).  The red 
# dots indicate the means (stat_summary).
ggplot(data = lovett_tidy)+
  geom_boxplot(aes(x = type, y = measurement))+
  coord_cartesian(ylim = c(40, 75))+
  stat_summary(aes(x = type, y = measurement), fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)

#Loading sanchez File from datasets folder

sanchez <- read_csv("Data Sets Anna/sanchez.csv")

#Calculate summary statistics for sanchez

bettle_density <- sanchez %>%
  group_by(bird_colony) %>%
  summarise(mean_bettle_density = mean(bettle_density),
            median_bettle_density = median(bettle_density),
            IQR_bettle_density = IQR(bettle_density),
            sd_bettle_density = sd(bettle_density),
            var_bettle_density = var(bettle_density),
            se_beetle=sd(bettle_density)/sqrt(n()))

#Histogram of original beetle density data

# Plot three histograms, one for each bird_colony type, using facet_wrap()
ggplot(sanchez) +
  geom_histogram(aes(bettle_density), binwidth = 20)+
  facet_wrap(~bird_colony)

#histogram, definitely questionable look. so let's modify the data 

sanchez<-mutate(sanchez, log1_bettle= log(bettle_density+1)) 

#histogram after data modification 

ggplot(sanchez) +
  geom_histogram(aes(log1_bettle), binwidth = 1.0)+
  facet_wrap(~bird_colony)

 #Box plot for both original and log data from sanchez
#original data box plot 
ggplot(sanchez)+
  geom_boxplot(aes(x = bird_colony , y = bettle_density), varwidth = TRUE)

#log y + 1 data box plot 

ggplot(sanchez)+
  geom_boxplot(aes(x = bird_colony , y = log1_bettle), varwidth = TRUE)

#done :)





