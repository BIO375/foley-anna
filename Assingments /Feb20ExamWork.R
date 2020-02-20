### Exam 1 Work questions 14-15 and Extra Credit

# Clean up the working environment
rm(list = ls())

### Install and load packages ####

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

##Load file polyploid 

polyploid <- read_csv("datasets/demos/polyploid.csv")

##Summary statistics 

polyploid_summary01 <- polyploid %>%
  group_by(ploidy) %>%
  summarise(n_length = n(),
            mean_length = mean(length),
            median_length = median(length),
            sd_length = sd(length),
            IQR_length = IQR(length),
            var_length = var(length),
            se_length = sd(length)/sqrt(n()))


##Box plot of the data

ggplot(data = polyploid)+
  geom_boxplot(aes(x = ploidy, y = length), notch = FALSE)+
  stat_summary(aes(x = ploidy, y = length), 
               fun.y=mean, 
               colour="blue", 
               geom="point", 
               shape=18, 
               size=3)

##Question 15 horned lizards 

lizards <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
lizards <- lizards %>% slice(-105)

##Extra credit 

##Load data for crickets 

crickets <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

##Create 4 histograms
#Raw data histograms, group between starved and fed crickets 

ggplot(crickets) +
  geom_histogram(aes(timeToMating), binwidth = 15)+
  facet_wrap(~feedingStatus)

##transform data to log

crickets <- crickets %>%
  mutate(log_timeToMating = log(timeToMating))

crickets<-mutate(crickets, log_timeToMating= log(timeToMating)) 

#histogram after data modification 

ggplot(crickets) +
  geom_histogram(aes(log_timeToMating), binwidth = 1.0)+
  facet_wrap(~feedingStatus)

####Done :) 




