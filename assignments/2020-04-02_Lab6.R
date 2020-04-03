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

##Looking at the plots, the data seem pretty normal. THe histograms are always so difficult to tell,with the kokanee group have much more variability than
#the sockeye group, but it is hard to tell any departures from normality. The boxplots look pretty even, with the mean being very similar
#to the median in both groups. The median is also pretty centered in the IQR, with the whiskers being even in both groups and only one 
#outlier in the sockeye group. If the ratio of s is less than 3, I would assume normality and perform a 2 sample t test.

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
#13-20 a####

#Two methods you could use for this data are a parametric two-sample t test (or welch's test) or the Mann Whitney U test/wilcoxan rank sum test for non-parametric data

##Overall, I would perform the welch's t test because the data is pretty normal (even before the transformation) so the only problem
#was the sd ratio. 

#The null hypothesis is that 
# uk = us

#the alternate hypothesis is 
# uk ≠ us

# Two-sided
t.test(abs ~ species, data = salmon, alternative = "two.sided", conf.level = 0.95)

#The data shows that the kokanee salmon had a significantly higher mean absorbance than the sockeye salmon (Welch's t test, two-sided, t=11.146, df = 20.289
#p value = 4.198 x10^-10)

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

##When looking at the histogram, it looks a bit left skewed in the book as well as in the histogram in R studio. When looking at the boxplot,
# the mean is a bit different than the median in the direction of left skew like we saw in the histogram, and with that we can see two low outliers
# below the box. However, other than them the whiskers are relatively even and the median is centered within the IQR.The qq plot shows a line
#that is not perfectly straight with the outliers, but the bulk of the data is relatively close. I would say because the n is relatively 
# high at 36, that the outliers that make the left skew are not as important as in a data set with smaller n, so we can still assume 
#normality and perform a paired t test. 

# The code below ASSUMES that you have read in the data file
# Now you have to specify which dataset the values are coming from using the form dataset$variable_name.

# Two-sided
t.test(trees$biomassChange, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

#The change in biomass of the rainforest after clear cutting was not significantly different (paired t test, two sided, t = -0.853, df=35, p=0.3996)

####Try mutating the data 
trees <- trees %>%
  mutate(log_biomass15 = log(biomassChange + 15))

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


