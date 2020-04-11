##Exam 2 Questions####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

##Mother vaccine antibodies####
##load data 
##looking for INCREASE in antibody after vaccination

baker <- read_csv("datasets/demos/baker.csv")

baker <- baker %>%
  mutate (difference = After - Before)

##Therefore if sees increase, then difference >0

##Q17####

#1. Response = streptococcus antibody concentration , Predictor = vaccination 

#2. statistical null :difference = after - before  
#ud < or = 0

# alternative is that ud > 0 

#3. For these data, the default appropriate test would be a paired t-test. 
#because it is looking at the differences before and after the vaccine.

#4
#Assumptions for paired t test: 
#i)The people are randomly sampled from population (technically there is volunteer 
#bias but hopefully other than we will assume it is a random sample). But groups of before 
#and after are not independent of each other because it is the same person before and after.
#ii) the differences are normally distributed 

# Do the data meet the assumption of normality?  Check with plots.

ggplot(baker) +
  geom_histogram(aes(difference), binwidth = 5)

ggplot(baker) +
  geom_boxplot(aes(x = '', y = difference))+
  stat_summary(aes(x = "", y = difference), 
               fun.y=mean, 
               colour="green", 
               fill = "purple",
               geom="point", 
               shape=21, 
               size=3)


ggplot(baker)+
  geom_qq(aes(sample = difference))

##These data are terrible. Try mutating it with log 

baker <- baker %>%
  mutate(logBefore = log(Before))

baker <- baker %>%
  mutate(logAfter = log(After))

baker <- baker %>%
  mutate(diff_logs = logAfter-logBefore)

##now try graphs 
ggplot(baker) +
  geom_histogram(aes(diff_logs), binwidth = 1)

ggplot(baker) +
  geom_boxplot(aes(x = '', y = diff_logs))+
  stat_summary(aes(x = "", y = diff_logs), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(baker)+
  geom_qq(aes(sample = diff_logs))

##These data do not meet the assumptions for normality (raw and log transformed data). The histogram is very right skewed and hard to find a bin width that 
#even connects them. The box plot does not have the median in the center of the IQR and it is very different than the mean. There are
#3 outliers above the IQR and the whiskers are not even. The qq plot is not a straight line at all. The log transformed 
#data helped with some of this squewing (don't know if that's a word), the qq plot got a bit better but not linear. The box plot ismore symmetrical but
#with outliers and differences between mean and median. But I still do not think it made it normal enough. Therefore, we need
#to perform the non-parametric sign test. 

# One-sample, One-sided, HA that sample mean is greater than null mean
SignTest(baker$difference, 
         alternative = "greater", mu = 0, conf.level = 0.95)

#There was a significant increase in the measured antibody concentration after the vaccination 
#(Sign Test, one-sided: S=18, number of differences =20, p-value =0.000201)


##Q18####

install.packages("abd", repos="http://R-Forge.R-project.org")
library("abd")

algae <- AlgaeCO2

#1. Response = growth rate, Predictor = Treatment group (level of CO2)
#2. Statistical null: µ normal CO2 = µ high CO2 or µh-µn = 0
##Alternative: µ normal CO2 ≠ µ high CO2 or µh-µn ≠ 0
#3. If all assumptions were met, the appropriate test would be a two-sample t test 
#4. Assumptions of a Two-Sample T test
##i)Obs are a random sample from the population and groups are independent of each other
#We know the groups are independent grown in different conditions, and if the observations are 
#not a random sample, then it is an experimental design issue, so we are assuming Collins and Bell did that
#properly 
##ii)Obs are from normally distributed population & iii)Two groups have homogeneous variance 
#shown below 

##Normality assumptions 

ggplot(algae) +
  geom_histogram(aes(growthrate), binwidth = 0.5)+
  facet_wrap(~treatment)

ggplot(algae) +
  geom_boxplot(aes(x = treatment, y = growthrate))+
  stat_summary(aes(x = treatment, y = growthrate), 
               fun.y=mean, 
               colour="pink", 
               fill = "pink",
               geom="point", 
               shape=19, 
               size=3)

ggplot(algae)+
  geom_qq(aes(sample = growthrate, color = treatment))

# Examine the ratio of the variances
summ_algae <- algae %>%
  group_by(treatment) %>% 
  summarise(mean_algae = mean(growthrate),
            sd_algae = sd(growthrate),
            n_algae = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_algae$sd_algae))/(min(summ_algae$sd_algae))

##ratio is < 3, therefore the variances are considered equal. 

##4. The histograms for both groups look relatively normal, but hard with small n. The high CO2 boxplot has a median centered in the middle of IQR
#and a mean that is only slightly higher. The upper whisker of this group is slightly longer than the bottom
#but with no outliers. For the normal CO2 boxplot, the median is less centered in the middle of the IQR, but the mean
#isn't that different than the median. The whiskers are even. For the qq plot, although
#there is not many data points, it is still linear for both groups. Therefore we can perform a parametric test
#Lastly, the ratio of s is 1.13, which means the variances are similar enough to perform a two sample over welch's t test

# Two-sided
t.test(growthrate ~ treatment, data = algae, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#5. The growth rate was found not to be significantly different between the high CO2 group and the normal CO2 group.
# (two-sample, two sided t test: t =-0.536, df=12, p value = 0.602)




