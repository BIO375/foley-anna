#Practice for final Exam

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

###Question 27 a)####

cichlid <- read_csv("datasets/demos/Cichlid.csv")

cichlid <- rename(cichlid, mRNA_level = GnRHmRNALevel)

##i) Null hypothesis 

#uT = uNT

##ii) Predictor= territorial status (categorical, nominal)
##    response = GnRHmRNAlevel

##iii) two-sample t test 

# Look at the summary statistics
summ_cichlid <- cichlid %>%
  group_by(territorialStatus) %>% 
  summarise(mean_level = mean(mRNA_level),
            sd_level = sd(mRNA_level),
            n_level = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_cichlid$sd_level))/(min(summ_cichlid$sd_level))

# Look at histograms, box plots, q-q plots
ggplot(cichlid) +
  geom_histogram(aes(mRNA_level), binwidth = 1)+
  facet_wrap(~territorialStatus)

ggplot(cichlid) +
  geom_boxplot(aes(x = territorialStatus, y = mRNA_level))+
  stat_summary(aes(x = territorialStatus, y = mRNA_level), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(cichlid)+
  geom_qq(aes(sample = mRNA_level, color = territorialStatus))

##mutate data 

cichlid <- cichlid %>%
  mutate(log_mRNA = log10(mRNA_level))

# Look at the summary statistics
summ_cichlidlog <- cichlid %>%
  group_by(territorialStatus) %>% 
  summarise(mean_level = mean(log_mRNA),
            sd_level = sd(log_mRNA),
            n_level = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_cichlidlog$sd_level))/(min(summ_cichlidlog$sd_level))

# Look at histograms, box plots, q-q plots
ggplot(cichlid) +
  geom_histogram(aes(log_mRNA), binwidth = 0.5)+
  facet_wrap(~territorialStatus)

ggplot(cichlid) +
  geom_boxplot(aes(x = territorialStatus, y = log_mRNA))+
  stat_summary(aes(x = territorialStatus, y = log_mRNA), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(cichlid)+
  geom_qq(aes(sample = log_mRNA, color = territorialStatus))

##made ratio better and the data is now normal enough.

##Two sample t-test 

t.test(log_mRNA ~ territorialStatus, data = cichlid, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


###27-b####


smoking <- read_csv("datasets/demos/NoSmokingDay.csv")

##i) Null hypothesis

#injuries on NSD ≤ injuries before NSD

## alternate = 
#injuries on NSD > injuries before NSD


#ud (on NSD-before NSD) ≤ 0

##ii) variables 

# explanatory:    injury group = categorical nominal 
#response variable: injuries =numerical continuous 

##iii) Paired t test????


##mutate data 

smoking <- smoking %>%
  mutate(injury_diff = injuriesOnNSD-injuriesBeforeNSD)

# Look at the summary statistics
summ_smoke <- smoking %>%
  summarise(mean_diff = mean(injury_diff),
            sd_diff = sd(injury_diff),
            n_diff = n())

# Look at histograms, box plots, q-q plots
ggplot(smoking) +
  geom_histogram(aes(injury_diff), binwidth = 20)

ggplot(smoking) +
  geom_boxplot(aes(x = "", y = injury_diff))+
  stat_summary(aes(x = "", y = injury_diff), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(smoking)+
  geom_qq(aes(sample = injury_diff))

##data looks normal, can perform a  one sided paired t test 

t.test(smoking$injuriesOnNSD, smoking$injuriesBeforeNSD, 
       alternative = "greater", paired = TRUE, conf.level = 0.95)

##there was a significantly higher injury rate on NSD than before NSD 
##(one sided paired t test, t=2.447, df=9, p-value = 0.0185)


##27-c####

oxygen <- read_csv("datasets/demos/oxygen.csv")














