#### Lab 8: 1-way ANOVA, continued #### 
# For this lab you will use the datasets described in Chapter 15 of your book 
# but you will answer the slightly modified questions that I provide below

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
install.packages("broom")
library("tidyverse")
# Check for updates
tidyverse_update()


###15-23####
##a and c#

pinecones <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

##a 
#this comparison is called a planned comparison, because it was decided before the experiment started

##c

##15-23 Step 1####
ggplot(pinecones) +
  geom_histogram(aes(conemass), binwidth = 0.5)+
  facet_wrap(~habitat)

ggplot(pinecones) +
  geom_boxplot(aes(x = habitat, y = conemass))+
  stat_summary(aes(x = habitat, y = conemass), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(pinecones)+
  geom_qq(aes(sample = conemass, color = habitat))

##Check for equal variance 

summ_cones <- pinecones %>%
  group_by(habitat) %>% 
  summarise(mean_cone = mean(conemass),
            sd_cone = sd(conemass),
            n_cone = n())


ratio <-(max(summ_cones$sd_cone))/(min(summ_cones$sd_cone))

##he histograms do not really give you much aboutt the normality of the data.The boxplots look pretty 
#symmetrical. Island absent and island present have median and mean centered in the IQR with equal whiskers.
#mainland present has a median a little higher than the center but with the mean pretty similar. 
#The qq plot, even with small n, looks linear. Therefore the data is normal enough to use the the parametric test
#The ratio of sd is 1.34, therefore there is no violation of equal variance 

#15-23 Step 2####

model23 <- lm(conemass~habitat, data = pinecones)

#15-23 Step 3####

autoplot(model23)

##I think the plot looks okay. It is very rectangular and not shaped liked a triangle, so the data
#is still normal enough for ANOVA.

### 15-23 Step 4 ####

anova(model23)

# Summary of the model results
summary(model23)

##We found that the conemass varied significantly with habitat (F=50.085, df=2,13, p=7.87e-7)

# Planned comparisons

planned <- glht(model23, linfct = 
                  mcp(habitat = c("island.absent-island.present = 0"
                      )))
confint(planned)
summary(planned)

##Using planned comparisons, the island absent showed significantly higher pinecone mass with island absent 
#compared to island present (t=8.596, p = 1.01e-6)

###############15-26####

malaria <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor()))


##15-26 Step 1####
ggplot(malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)

ggplot(malaria) +
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  stat_summary(aes(x = treatmentGroup, y = logSporozoiteNumbers), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))


##the histograms do not really give you much aboutt the normality of the data, but you do see an 
#outlier in the scorpine group.The boxplots look pretty symmetrical for control and WT.Control and WT 
#have median and mean centered in the IQR with almost equal whiskers. 
#But the scorpine group has outliers on both sides and a very different mean compared to median 
#and the median not centered in the IQR. The qq plot looks linear for control and WT, but the scorpine
#has the very large outlier. I believe that this makes the data non-parametric so we must perform a 
#Non-parametric Kruskal-Wallis test

#15-26 Step 2####

model26 <- lm(logSporozoiteNumbers~treatmentGroup, data = malaria)

#15-26 Step 3####

autoplot(model26)

##The plot looks a bit triangular, so I do believe the decision to do the non-parametric test was correct.

### 15-26 Step 4 ####

kruskal.test(logSporozoiteNumbers ~ treatmentGroup, data = malaria)

##The KW Rank sum test shows a statistical difference between the groups
#(chi-squared = 22.874, df=2, p=1.079e-5)


###15-30 & 31####

crabs <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor()))

crabs <- na.omit(crabs)

##15-30 Step 1####

ggplot(crabs) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.4)+
  facet_wrap(~crabType)

ggplot(crabs, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()+
  stat_summary(aes(x = crabType, y = bodyTemperature), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
  
ggplot(crabs)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

##Check for equal variance 

summ_crabs <- crabs %>%
  group_by(crabType) %>% 
  summarise(mean_temp = mean(bodyTemperature),
            sd_temp = sd(bodyTemperature),
            n_temp = n())


ratio <-(max(summ_crabs$sd_temp))/(min(summ_crabs$sd_temp))

#The histograms do not really demosntrate a large skew or any abnormalities. The boxplot for each group
#has pretty similar means and medians, with the medians being pretty centered in the IQR. The whiskers for 
#female are similar length. The bottom whisker is slightly longer for intact male and minor male removed.
#THe only problem with these plots is that there are outliers in 3 of the 4 groups. However, the n is pretty large
#so I feel it may still be appropriate to perform the parametric test. The ratio of sd is 1.18, so there are 
#equal variances.


#15-30 Step 2####

model30 <- lm(bodyTemperature~crabType, data = crabs)

#15-30 Step 3####

autoplot(model30)

##The plot looks pretty rectangular and not so much like a square, so I believe although there are a 
#couple of outliers, it is still okay to do the ANOVA test 

### 15-30 Step 4 ####

anova(model30)

# Summary of the model results
summary(model30)

##We found that the conemass varied significantly with habitat (F=50.085, df=2,13, p=7.87e-7)

# Planned comparisons

planned <- glht(model23, linfct = 
                  mcp(habitat = c("island.absent-island.present = 0"
                  )))
confint(planned)
summary(planned)

##Using planned comparisons, the island absent showed significantly higher pinecone mass with island absent 
#compared to island present (t=8.596, p = 1.01e-6)


