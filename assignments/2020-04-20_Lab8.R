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


### Multiple Comparisons ####

# Earlier you installed and loaded the package multcomp.  To do planned comparisons
# we will use a function in multcomp called glht, short for general linear
# hypotheses.
# linfct specifies the linear hypotheses to be tested, I find the easiest way
# is to specify by name

# Planned comparisons

planned <- glht(model01, linfct = 
                  mcp(parasite = c("Metschnikowia - control = 0",
                                   "Pansporella - control = 0",
                                   "Pasteuria - control = 0")))
confint(planned)
summary(planned)

###15-26####

###15-30 & 31####