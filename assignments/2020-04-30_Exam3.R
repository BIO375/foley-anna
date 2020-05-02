##Exam 3 ####
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")

# Check for updates
tidyverse_update()

##Load Data#### 

caffeine <- read_csv("datasets/demos/caffeine.csv", col_types = cols(group = col_factor()))

# The general workflow as you do analyses in R should be as follows:
#   Step 1.  Plot your data (boxplots, histograms, Q-Q plots)
#   Step 2.  Use the function lm() to fit a model, specifying equation & data
#     e.g., y ~ x, data = data
#   Step 3.  Check assumptions again, using residuals plot
#   Step 4.  If assumptions are met, use the functions anova() and summary() 
#     to interpret statistical results.  If assumptions are not met, try 
#     data transformation and/or a non-parametric or robust version of the test

##Null Hypotheses & Variables####

#i) There is no difference in the mean caffeine metabolism rate between the male and normal progesterone female groups 
## u m = u npf

##ii) There is no difference in the mean caffeine metabolism rate between normal progesterone female group and high 
#progesterone female group
## u hpf = u npf

##predictor variable: group (categorical)
##Response: caffeine concentration in the blood  (continuous)

##Step 1####
##Check assumptions 
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 3)+
  facet_wrap(~group)

ggplot(caffeine) +
  geom_boxplot(aes(x = group, y = half_life))+
  stat_summary(aes(x = group, y = half_life), 
               fun.y=mean, 
               colour="purple", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))

##Check for equal variance 

summ_caffeine <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_caffeine = mean(half_life),
            sd_caffeine = sd(half_life),
            n_caffeine = n())

ratio <-(max(summ_caffeine$sd_caffeine))/(min(summ_caffeine$sd_caffeine))

##Looking at the histograms, the norm_prog and high_prog look pretty normal, with the male group potentially being
#a bit right skewed. The boxplot for the normal prog has similar whiskers and a mean similar to the median, 
#relatively centered in the IQR. The high prog group has very similar mean and median that is a bit lower in the
#IQR, but has one outlier way lower than the IQR. However, the top whisker in this group is a bit longer than the 
#lower whisker so that would hopefully help balance it out. The male group is the one with the most problems, with
#the mean slightly deviating from the median with the high outlier as well as a longer tail on top of the IQR.
#However, the median itself is pretty centered in the IQR, and the male group is the one with the highest n at 13
#so the outlier would have less of an influence on the data. Lastly, the qq plot shows pretty linear for both
#the male and the normal prog group, and a linear relationship for the high prog group other than the one outlier. 
#I tried log transforming the data and although it made some parts of the data better, it made other parts worse.
#Overall, due to what I mentioned above,I believe the data is still normal enough to perform a parametric test. 
#Also, the ratio of variance is 1.697, which means that the data meet the assumption of homogeneous variance as well.

##The test I am using is Fixed effects ANOVA with planned comparisons 

##Step 2####

model01 <- lm(half_life~group, data = caffeine)

##Step 3####
##Check assumptions again

autoplot(model01)

##Looking at the Residuals vs fitted graph, it does not look like a cone shape, even with the outliers,
#so there is no indication of the violation of assumptions, therefore  it does meet the assumptions of ANOVA

##Step 4 ANOVA####

##Anova model 

anova(model01)

##There is a significant difference between the three groups 
#(One-way ANOVA: F=13.866;df = 2, 27; p=6.528x10^-5)

##Planned comparisons#### 

planned <- glht(model01, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "norm_prog - high_prog = 0")))
confint(planned)
summary(planned)

##Planned comparison revealed that there was no significant difference in the half life (caffeine blood concentration) between male 
#and female with normal progesterone level
#(t=-0.649, p=0.73758)

##Planned comparisons revealed the high progesterone female group had a significantly higher half life compared 
#to the normal progesterone level female group, meaning the normal progesterone group had a faster caffeine metabolism
#(t=-4.034, p=0.00074)




