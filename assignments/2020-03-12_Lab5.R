##Lab5####
# Clean up the working environment
rm(list = ls())
##Load proper packages##

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

#Problem 1####

earthangle <- read_csv("Data Sets Anna/EarthAngleData.csv")

###1.1 

#The hypothesis is two sided, because we are seeing if the angle is different than the angle given by the paris observatory in 
#1738. So it can either be greater or smaller for the alternative hypothesis 

#therefore: a) angle is different from angles measures (u0 does not equal u previous)

###1.2 

#The null hypothesis is that the 1738 measurement is the same as the angles from the past 
#(u0 = uprevious)

##1.3 One sample T test 

#two sided

# Identify your response variable using the form dataset$variable_name
y<-earthangle$Obliquity

#Calculate Summary Statistics 
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1
alpha=0.05

#Null mean 
null_mean <- 23.4722

#calculate t sample 
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Other way to calculate Two-sided
t.test(earthangle$Obliquity, 
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)

##We found the obliquity measured in Paris in 1738 was statistically different when compared with 
#earlier measurements (one-sample t-test: t = 3.03; df = 4; P=0.04)

##Problem 2####

heartattack <- read_csv("datasets/demos/HeartAttack_short.csv")


##2.1 

#two-sided because we are looking for just a difference (not greater or less than)
#Group 1 is Heart attack 
#Group 2 Control 

#Statistical alternative hypothesis 
#u1 - u2 ≠ 0 

#Statistical Null hypothesis 
#u1 - u2 = 0

##2.2 

#df = n1 + n2 -2 
# = 28 + 30 -2
# degrees of freedom is 56


##Calculate summary statistics for each group 

 heart_summary <- heartattack %>%
  group_by(`group`) %>%
  summarise(n_heart = n(),
            mean_heart = mean(cholest ),
            median_heart = median(cholest ),
            sd_heart = sd(cholest ),
            IQR_heart = IQR(cholest ),
            var_heart = var(cholest ),
            se_heart = sd(cholest )/sqrt(n()))

##2.3 (2.4)

##Check normality assumption 
 # Look at histograms, box plots, q-q plots
 
 ggplot(heartattack) +
   geom_histogram(aes(cholest), binwidth = 25)+
   facet_wrap(~group)
 
 ggplot(data = heartattack, mapping = aes(group = group, x = group, y = cholest)) + 
  geom_boxplot()+
   stat_summary(aes(x = group, y = cholest), 
                fun.y=mean, 
                colour="green", 
                fill = "green",
                geom="point", 
                shape=21, 
                size=3)
 
 ggplot(heartattack)+
   geom_qq(aes(sample = cholest, color = group))
 
##Try mutating the data 
heartattack <- heartattack %>%
   mutate(log_cholest = log(cholest))

##Check normality again
ggplot(heartattack) +
  geom_histogram(aes(log_cholest), binwidth = 0.1)+
  facet_wrap(~group)

ggplot(data = heartattack, mapping = aes(group = group, x = group, y = log_cholest)) + 
  geom_boxplot()+
  stat_summary(aes(x = group, y = log_cholest), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(heartattack)+
  geom_qq(aes(sample = log_cholest, color = group))

# In both histograms, it seems to be a generally normal distribution, with a larger range for group 1 compared
#to group 2. There also shows a tail in the lower end for group 1. As for the boxplots, it shows an outlier&longer whisker and right skew for 
#group 2, with the mean being larger than the median. For group1, the whiskers are relatively the same length but the mean is more centered
#in the middle of the IQR than the median. But, similar to the ward snail data, the samples sizes of 28 and 30 are large so even with the slight
#deviations from normality, a parametric test is still okay 


# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
 ratio <-(max(heart_summary$sd_heart))/(min(heart_summary$sd_heart))

##The ratio is 2.20, which means it is less than or equal to 3, so the variances are homegenous enough for the parametric test, not the
 #welch's seperate variance test 
 
# With both of these assumptions that are not violated, then we can assume the two sample t test is reliable 
 
##2.5 perform 2 sample pooled variance t test 
 
# Two-sided
 t.test(cholest ~ group, data = heartattack, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

##Conclusions: We found that blood cholesterol in heart-attack patients 2 days post heart attack was
##statistically different than the blood cholesterol in individuals who did not have a heart attack
## (two-sample t test: t=6.2852; df = 56; P = 5.202 x10^-8)
 
 
##Problem 3####
 
furness <- read_csv("datasets/quinn/chpt3/furness.csv")

##3.1 
##When there are departures from normality in the data, you can use a non-parametric test, 
 #it does not assume normally distributed observations 

##Assumptions of a Non-parametric test 
 #1. Still a random sample of observations (for the 1 &2 sample designs they also have to be independent observations)
 #2. If it is a two-sample design, it must have homogenous variance 
 #3.If a two sample design, then the shape that is non-normal needs to still have similar distribution shape 
 
##The options for tests are
 #A) Sign Test. This option is used for non-normally distributed data in which you would have used a one sample or paired t test.
 #B) Mann-Whitney U test or Wilcoxan rank sum test. This would be used for non-normally distributed data in which we would have used 
 #a two-sample test 
 
 ##In this case, we are looking at males versus females so we would go with option B the Mann-Whitney U test or Wilcoxan rank sum test
 
##3.2
 
#In the two-sample t test, the null hypothesis tested is the difference between the two population means is equal to a certain 
#to zero. (u1 -u2 = 0). For the MWU test, the H0 tested is that if the non-normal distributions are the same shape, that 
 #median 1-median2 =0. They are different in what statistic they are using to describe the population, mean versus median. If not the 
 #same shape, then the H0 tested is that the two groups have the same distribution 
 
##3.3
 
 #THe underlying assumptions for MWU test include 
 #a)the dependent variable is measured on a continuous scale and the independent variable is 2 categories 
 #b) the observations still need to be a random sample and independent from each other (no relationship between the two groups ), 
 #similar to two sample 
 #c) For the MWU, the non-normal distributions have to be the same shape and if not the same shape, then the H0 is that they have 
 #the same distribution

##Why might this not be the best test?
ggplot(furness) +
   geom_histogram(aes(METRATE), binwidth = 500)+
   facet_wrap(~SEX)
 
 ggplot(furness) +
   geom_boxplot(aes(x = SEX, y = METRATE))+
   stat_summary(aes(x = SEX, y = METRATE), 
                fun.y=mean, 
                colour="blue", 
                fill = "blue",
                geom="point", 
                shape=21, 
                size=3)
 
 ggplot(furness)+
   geom_qq(aes(sample = METRATE, color = SEX))
 
#Unlike the cricket data, which showed that they were both right skewed, the furness data does not obviously show the same shape
 #with the female data looking a bit ore right skewed than the male data. Overall, it is also hard to tell with the small sample size in
 #each group

#3.4 Perform Wilcoxon test 
 
# Two-sided
wilcox.test(METRATE ~ SEX, data = furness, alternative = "two.sided", conf.level = 0.95)

#We found that the metabolic rate of male fulmars was not statistically different than the metabolic rate of female 
#fulmars (Wilcoxon rank sums test: W=21, p-value = 0.75)
 

##Problem 4####

elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")

##Mutate data to do difference in horizontal diameter, check if normally distributed 

elgar <- elgar %>%
  mutate(diff_hori = HORIZLIG-HORIZDIM)

ggplot(elgar) +
  geom_histogram(aes(diff_hori), binwidth = 60)

ggplot(elgar) +
  geom_boxplot(aes(x = "", y = diff_hori))+
  stat_summary(aes(x = "", y = diff_hori), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(elgar)+
  geom_qq(aes(sample = diff_hori))


##4.1 

#After looking at the histogram, boxplot and the qq plot for the differences in the horizontal diameter of webs. The histogram 
#is not very informative, but when looking at the boxplot, the median and the mean are very similar, and the median is centered in the 
#middle of the IQR. In addition, the whiskers are similar in length with no outliers. Lastly, the qq plot shows pretty linear relationship
#Looking at all this, I assume normality in the elgar data, meaning that we can use a parametric test. As mentioned in the introduction 
#they used paired comparisons because it was the same spider spinning the web at different conditions. So since the experimental units are
#sampled twice and not independent of each other, then a paired t test is the appropriate test. 

##4.2 

# The null hypothesis is that the difference in horizontal diameter spun in light and dark conditions is zero 
# ud= 0 (ulig - udim = 0)


##4.3

#load tidy data 

elgar_tidy <- read_csv("Data Sets Anna/elgartidydata.csv")

##summary 

elgar_summary <- elgar_tidy %>%
  group_by(`Lighting`) %>%
  summarise(n_heart = n(),
            mean_lighting = mean(hori_length ),
            median_lighting = median(hori_length ),
            sd_lighting = sd(hori_length ),
            IQR_lighting = IQR(hori_length ),
            var_lighting = var(hori_length ),
            se_lighting = sd(hori_length )/sqrt(n()))

##Check assumption of homogenous variance 

ratio <-(max(elgar_summary$sd_lighting))/(min(elgar_summary$sd_lighting))

#ratio is 1.09, so it meets assumption of homogenous variance

#Although these data meet the assumption of a random sample and homogenous variance, These data do not meet one assumption 
#for the two sample t test because as mentioned before, the measurement in the light and dim 
#conditions are not independent of each other. If they had used different randomly selected spiders and only measured each experimental 
#unit once, then it would meet the assumptions for two sample. But the experimental units are not independent. 


##4.4 perform appropriate statistical test, paired t test

t.test(elgar$HORIZLIG, elgar$HORIZDIM, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)
 
 
 
 
 
 
 
 
 
 
