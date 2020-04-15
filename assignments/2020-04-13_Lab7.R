#### Lab 7: 1-way ANOVA #### 
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# The general workflow as you do analyses in R should be as follows:
#   Step 1.  Plot your data (boxplots, histograms, Q-Q plots)
#   Step 2.  Use the function lm() to fit a model, specifying equation & data
#     e.g., y ~ x, data = data
#   Step 3.  Check assumptions again, using residuals plot
#   Step 4.  If assumptions are met, use the functions anova() and summary() 
#     to interpret statistical results.  If assumptions are not met, try 
#     data transformation and/or a non-parametric or robust version of the test

##Load Data

jaffe <- read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))

##Step 1 Aldrin####
ggplot(jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)

ggplot(jaffe) +
  geom_boxplot(aes(x = Depth, y = Aldrin))+
  stat_summary(aes(x = Depth, y = Aldrin), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

jaffe <- jaffe %>%
  mutate(log_Aldrin = log(Aldrin))

##Do graphs for log transformed data 

ggplot(jaffe) +
  geom_histogram(aes(log_Aldrin), binwidth = 0.3)+
  facet_wrap(~Depth)

ggplot(jaffe) +
  geom_boxplot(aes(x = Depth, y = log_Aldrin))+
  stat_summary(aes(x = Depth, y = log_Aldrin), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(jaffe)+
  geom_qq(aes(sample = log_Aldrin, color = Depth))

##Check for equal variance 

summ_aldrin <- jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_aldrin = mean(Aldrin),
            sd_aldrin = sd(Aldrin),
            n_aldrin = n())


ratio <-(max(summ_aldrin$sd_aldrin))/(min(summ_aldrin$sd_aldrin))

##do equal variance on log data 

summ_logaldrin <- jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_logaldrin = mean(log_Aldrin),
            sd_logaldrin = sd(log_Aldrin),
            n_logaldrin = n())

ratio <-(max(summ_logaldrin$sd_logaldrin))/(min(summ_logaldrin$sd_logaldrin))

##Step 1 HCB####

ggplot(jaffe) +
  geom_histogram(aes(HCB), binwidth = 1)+
  facet_wrap(~Depth)

ggplot(jaffe) +
  geom_boxplot(aes(x = Depth, y = HCB))+
  stat_summary(aes(x = Depth, y = HCB), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

##Check variance 

summ_HCB <- jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())

ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

##Step 2 Aldrin ####

model01 <- lm(Aldrin~Depth, data = jaffe)

