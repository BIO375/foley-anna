### Lab 6. More T tests####

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()
install.packages(c("tibble", "xml2"))
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

##Problem 13-20####

##Load tidy salmon data 

salmon <- read_csv("Data Sets Anna/salmontidydata.csv")

##13-20 a)

##Two methods you could use for this data would be ANOVA and two-sample t test. Both of these can specifically test a difference 
#between the two means if they meet the assumptions of normal distribution and homogenous variance. If the data does not 
#meet the assumption of homogenous variance, than a Welch's unequal variance test can be used. If the data does not meet
#the assumption of a normal distribution, then you could use the MWU test. But if the distributions were not the same shape, 
#then you could not test the difference in means. 

##13-20 b)


##Calculate Summary stats for salmon data 
salmon_summary <- salmon %>%
  group_by(species) %>%
  summarise(n_salmon = n(),
            mean_salmon = mean(abs),
            median_salmon = median(abs),
            sd_salmon = sd(abs),
            IQR_salmon = IQR(abs),
            var_salmon = var(abs),
            se_salmon = sd(abs)/sqrt(n()))

##Check normality assumption 
# Look at histograms, box plots, q-q plots
ggplot(salmon) +
  geom_histogram(aes(abs), binwidth = 0.1)+
  facet_wrap(~species)

ggplot(data = salmon, mapping = aes(x = species, y = abs)) + 
  geom_boxplot()+
  stat_summary(aes(x = species, y = abs), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(salmon)+
  geom_qq(aes(sample = abs, color = species))

##Looking at the plots, the data seem pretty normal. The histograms are always so difficult to tell, with the kokanee group have much more variability than
#the sockeye group, but it is hard to tell any departures from normality. The boxplots look pretty even, with the mean being very similar
#to the median in both groups. The median is also pretty centered in the IQR, with the whiskers being even in both groups and only one 
#outlier in the sockeye group.The qq plot shoes for both of the groups the data is pretty linear.
#If the ratio of s is less than 3, I would assume normality and homoscedasticity and perform a 2 sample t test.

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(salmon_summary$sd_salmon))/(min(salmon_summary$sd_salmon))

##The ratio IS NOT below 3, which means that the assumption was violated for the 2 sample t test. 

####Try mutating the data 
salmon <- salmon %>%
  mutate(log_abs = log(abs))

##Check normality again

salmon_summary2 <- salmon %>%
  group_by(species) %>%
  summarise(n_salmon = n(),
            mean_salmon = mean(log_abs),
            median_salmon = median(log_abs),
            sd_salmon = sd(log_abs),
            IQR_salmon = IQR(log_abs),
            var_salmon = var(log_abs),
            se_salmon = sd(log_abs)/sqrt(n()))

ggplot(salmon) +
  geom_histogram(aes(log_abs), binwidth = 0.1)+
  facet_wrap(~species)

ggplot(data = salmon, mapping = aes(x = species, y = log_abs)) + 
  geom_boxplot()+
  stat_summary(aes(x = species, y = log_abs), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(salmon)+
  geom_qq(aes(sample = log_abs, color = species))

##Calculate log ratio of s 

ratio <-(max(salmon_summary2$sd_salmon))/(min(salmon_summary2$sd_salmon))

##Overall, I would perform the welch's t test because the data is pretty normal (even before the transformation) so the only problem
#was the s ratio being above 3

##Looking for DIFFERENCE in mean skin color between the sockeye and the kokanee 

#The null hypothesis is that 
# uk = us

#the alternate hypothesis is 
# uk ≠ us

# Two-sided
t.test(abs ~ species, data = salmon, alternative = "two.sided", conf.level = 0.95)

#Kokanee salmon had a significantly higher mean skin absorbance than the sockeye salmon 
#(Welch's t test, two-sided, t=11.15, df = 20.29, p value = 4.20 x10^-10)

##13.25####

trees <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

##Check normality assumption 
# Look at histograms, box plots, q-q plots
ggplot(trees) +
  geom_histogram(aes(biomassChange), binwidth = 2)
  

ggplot(data = trees, mapping = aes(x = '', y = biomassChange)) + 
  geom_boxplot()+
  stat_summary(aes(x = '', y = biomassChange), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(trees)+
  geom_qq(aes(sample = biomassChange))

##When looking at the histogram, it looks left skewed in the book as well as in the histogram in R studio. When looking at the boxplot,
# the mean is a different than the median in the direction of left skew like we saw in the histogram, and with that we can see two low outliers
# below the box. The whiskers are a bit uneven, even with the median is centered within the IQR.The qq plot shows a line
#that is not perfectly straight with the outliers.

####Try mutating the data 
trees <- trees %>%
  mutate(log_biomass15 = log(biomassChange + 15))
##add 15 because log 0 is undefined 


##Check normality assumption 
# Look at histograms, box plots, q-q plots
ggplot(trees) +
  geom_histogram(aes(log_biomass15), binwidth = 0.1)


ggplot(data = trees, mapping = aes(x = '', y = log_biomass15)) + 
  geom_boxplot()+
  stat_summary(aes(x = '', y = log_biomass15), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(trees)+
  geom_qq(aes(sample = log_biomass15))

##Honestly, log transforming +15 to make the numbers not negative made the data worse not better. So my analysis is of the unmutated data

#Transforming the data did not really do anything. Even with n being relatively high at 36, that the data is still not
#normal enough. The non-parametric version of the paired t test and one sample is the sign test. 

# One-sample, Two-sided
SignTest(trees$biomassChange, alternative = "two.sided", mu = 0, conf.level = 0.95)

#The change in biomass of the rainforest after clear cutting nearby was not significantly different
#(Sign test, two sided, S = 21, number of difference =36, p=0.405)
#(median of differences 0.2, 97.1% CI: -1.4----1.2)w


##13.26####

finches <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

##Check normality assumption 
# Look at histograms, box plots, q-q plots
ggplot(finches) +
  geom_histogram(aes(preference), binwidth = 10)

#So hard to tell I do not enjoy histograms at this small sample size

ggplot(data = finches, mapping = aes(x = '', y = preference)) + 
  geom_boxplot()+
  stat_summary(aes(x = '', y = preference), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(finches)+
  geom_qq(aes(sample = preference))

##The boxplot does not show that much departure from normality. The mean is a little bit higher than the median with a whisker 
#longer, indicating a slight right skew. However, the median is still pretty centered in the IQR. The qq plot is not perfectly
#linear but hard to evaluate with n=10.

####Try mutating the data to see if it helps
finches <- finches %>%
  mutate(log_pref = log(preference))

##Check normality assumption 
# Look at histograms, box plots, q-q plots
ggplot(finches) +
  geom_histogram(aes(log_pref), binwidth = 0.3)

ggplot(data = finches, mapping = aes(x = '', y = log_pref)) + 
  geom_boxplot()+
  stat_summary(aes(x = '', y = log_pref), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(finches)+
  geom_qq(aes(sample = log_pref))

##Looking specifically at the boxplot, after the log mutation the mean and the median are closer together and they are both centered in the IQR.
#The whiskers are also similar in length. Overall, I think the log transformation helped make the data a bit more normal, so we will analyse it
#with the proper parametric test.
##It was easier to determine that it was a one sample instead of a paired or two-sample t test because they gave you the data as
#preference instead of two groups measurements for each brother finch. I find this data similar to the koala bellow example. Each experimental
#group is not measured twice, like for a paired t test, but the finches are not independent of each other either which is an 
#assumption for a two-sample, so we can test it in the form of one-sample and the parameter of interest is no preference

##Null hypothesis is 
#u = 0

#Alternate hypothesis iS 
#u ≠ 0

##measured in terms of percentage spent next to carotenoid supplemented male

# Two-sided
t.test(finches$preference, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#The data showed significant preference for the caretenoid-supplemented finches over the low-caretenoid finches.
#(One-sample, two-sided, t = 5.62, df=9, p=0.000326)


##Review 2-16####

zebrafish <- read_csv("datasets/abd/review2/rev2q16ZebrafishAggression.csv")

##Check normality assumption 
# Look at histograms, box plots, q-q plots

ggplot(zebrafish) +
  geom_histogram(aes(timeInAggression), binwidth = 32)+
  facet_wrap(~genotype)

ggplot(data = zebrafish, mapping = aes(x = genotype, y = timeInAggression)) + 
  geom_boxplot()+
  stat_summary(aes(x = genotype, y = timeInAggression), 
               fun.y=mean, 
               colour="green", 
               fill = "green",
               geom="point", 
               shape=21, 
               size=3)

ggplot(zebrafish)+
  geom_qq(aes(sample = timeInAggression, color = genotype))

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(zebrafish_summary$sd_zebra))/(min(zebrafish_summary$sd_zebra))

##Summary Statistics 
zebrafish_summary <- zebrafish %>%
  group_by(genotype) %>%
  summarise(n_zebra = n(),
            mean_zebra = mean(timeInAggression),
            median_zebra = median(timeInAggression),
            sd_zebra = sd(timeInAggression),
            IQR_zebra = IQR(timeInAggression),
            var_zebra = var(timeInAggression),
            se_zebra = sd(timeInAggression)/sqrt(n()))

##The histogram is hard to look at for normality. WT looks relatively normal but hard to get a binwidth for the spd mutant.
#The boxplot shows, for the mutant group, the mean and the median are almost identical and positioned in the middle of the IQR. The 
#whiskers are a little bit uneven. The WT is a bit less symmetrical, with the mean being smaller than the median but the top whisker
#being longer. Still, no outliers. What is interesting is the qq plot actually looks more linear for the WT than the Spd mutant, even 
#with the boxplots being the way they are. I would say that the departures from normality are not that severe and that I would
#test the data with a parametric test. I would choose two-sample because each experimental unit is only observed once, and the experimental
#units are independent of each other. They are from a normally distributed population and the ratio of sd is 1.39, so there is homoscedasticity.

##Null hypothesis is 
#umut-uWT = 0

#Alternate hypothesis iS 
#umut-uWT ≠ 0

# Two-sided t test
t.test(timeInAggression ~ genotype, data = zebrafish, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

##2.16 a&b####

#t sample value is 3.38
#df=19
#p-value = 0.003142
##95% confidence interval of the difference between the two means = 25.93-110.27
#Mean Mutant group = 142.1
#Mean Wild Type = 74.0

#Difference 
mean_zebraMut <-142.1
mean_zebraWT <- 74.0
diff_meanfish <- mean_zebraMut-mean_zebraWT
sd_zebraMut <-37.74
sd_zebraWT <- 52.5
dfMut <-9
dfWT <- 10
n_Mut <-10
n_WT <-11

pooled_variance <- ((dfMut*(sd_zebraMut)^2) + (dfWT*(sd_zebraWT)^2))/(dfMut+dfWT)
#pooled variance = 2125.33

##SE umut-uWT

SE <- (sqrt(pooled_variance*((1/n_Mut)+(1/n_WT))))

##tsample 

t_sample <- (mean_zebraMut-mean_zebraWT)/SE


##a)magnitude is shown by the 95% confidence interval difference between the means
## 25.93-110.27

##b)Two-sample t test was performed above 
#null hypothesis is umut = uwt

#alternate hypothesis is umut ≠ uwt

#The data show that there was a significant increase in aggression time for the Spd mutant group compared to 
#the wild type group (two-sided, two sample t test, t=3.38, df=19, p-value = 0.0031)