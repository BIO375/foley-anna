##Exam 3 R script####

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


##Step 1####

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
#relatively centred in the IQR. The high prog group has very similar mean and median that is a bit lower in the
#IQR, but has one outlier way lower than the IQR. However, the top whisker in this group is a bit longer than the 
#lower whisker so that would hopefully help balance it out. The male group is the one with the most problems, with
#the mean slightly deviating from the median with the high outlier as well as a longer tail on top of the IQR.
#However, the median itself is pretty centered in the IQR, and the male group is the one with the highest n at 13
#so the outlier would have less of an influence on the data.Lastly, the qq plot shows pretty linear for both
#the male and the normal prog group, and a linear relationship for the high prog group other than the outlier. 
#Overall, I believe the data is still linear enough to perform a parametric test. Also, the ratio of variance is 
#1.697, which means that the data meet the assumption of homogenous variance as well.

##The test I am using is Fixed effects ANOVA with planned comparisons 

##Step 2####

model01 <- lm(half_life~group, data = caffeine)

##Step 3####
##Check assumptions again

autoplot(model01)

##Looking at the REsiduals vs fitted graph, it does not look like a cone shape, even with the outliers,
#so there is no indicaition of the violation of assumtpions, therefore  it does meet the assumptions of ANOVA

##Step 4 ANOVA####

##Null Hypotheses & Variables####

#i) There is no difference in caffeine half_life between the male and normal progesterone female groups 
## u m = u npf

##ii) There is no difference in caffeine half_life between normal progesterone female group and high 
#progesterone female group
## u hpf = u npf

##predictor variable: Treatment group (categorical)
##Response: caffeine concentration in the blood  (continuous)

##Anova model 

anova(model01)

##There is a significant difference between the group 
#df=2, 28
#F value = 13.866
#P= 6.528x10^-5

##Planned comparisons#### 

planned <- glht(model01, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "norm_prog - high_prog = 0")))
confint(planned)
summary(planned)

##No significant difference between male and women (t=-0.649, p=0.73758)
##Significantly higher half life in the high prog group compared to 
#normal prog group, meaning less rapid metabolism with higher progesterone levels 
#(t=-4.034, p=0.0074)


