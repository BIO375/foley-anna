#Chapter 11 Hwk####

library("tidyverse")
tidyverse_update()

#Q1####

rangeshift <- read_csv("datasets/abd/chapter11/chap11q01RangeShiftsWithClimateChange.csv")


#Summary Statistics 

geo_summary <- rangeshift %>%
  summarise(n_geo = n(),
            mean_geo = mean(elevationalRangeShift),
            median_geo = median(elevationalRangeShift),
            sd_geo = sd(elevationalRangeShift),
            IQR_geo = IQR(elevationalRangeShift),
            var_geo = var(elevationalRangeShift),
            se_geo = sd(elevationalRangeShift)/sqrt(n()))

#Q6####

WolfTeeth <- read_csv("datasets/abd/chapter11/chap11q06WolfTeeth.csv")

wolf_summary <- WolfTeeth %>%
  summarise(n_wolf = n(),
            mean_wolf = mean(wolfTeethLength),
            median_wolf = median(wolfTeethLength),
            sd_wolf = sd(wolfTeethLength),
            IQR_wolf = IQR(wolfTeethLength),
            var_wolf = var(wolfTeethLength),
            se_wolf = sd(wolfTeethLength)/sqrt(n()))

ggplot (data=WolfTeeth) +
  geom_histogram(mapping = aes (wolfTeethLength), binwidth = 0.2)

ggplot (data=WolfTeeth)+
  geom_boxplot(mapping = aes(wolfTeethLength) )


#Q8####

mating <- read_csv("Data Sets Anna/chapter11q8datamatingsystem.csv")

ggplot (data = mating)+
  geom_histogram(aes(TestesArea), binwidth = 0.05)+
  facet_wrap(~Group)

ggplot(data = mating)+
  geom_boxplot(aes(x = Group, y = TestesArea))+
  stat_summary(aes(x = Group, y = TestesArea), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

mating_summary <- mating %>%
  group_by(Group) %>%
  summarise(n_mating = n(),
            mean_mating = mean(TestesArea),
            median_mating = median(TestesArea),
            sd_mating = sd(TestesArea),
            IQR_mating = IQR(TestesArea),
            var_mating = var(TestesArea),
            se_mating = sd(TestesArea)/sqrt(n()))

#Q9####

chap11q9 <- read_csv("Data Sets Anna/chap11q9.csv")

foodweb_summary <- chap11q9 %>%
  summarise(n_mating = n(),
            mean_foodweb = mean(MeanScore),
            median_foodweb = median(MeanScore),
            sd_foodweb = sd(MeanScore),
            IQR_foodweb = IQR(MeanScore),
            var_foodweb = var(MeanScore),
            se_foodweb = sd(MeanScore)/sqrt(n()))

##Chapter 12####

Taxes <- read_csv("datasets/abd/chapter12/chap12q01DeathAndTaxes.csv")

Taxes <- Taxes %>%
  mutate (difference = HigherTaxDeaths - lowerTaxDeaths)

Taxdiff_summary <- Taxes %>%
  summarise(n_difference = n(),
            mean_Taxdiff = mean(difference),
            median_Taxdiff = median(difference),
            sd_Taxdiff = sd(difference),
            IQR_Taxdiff = IQR(difference),
            var_Taxdiff = var(difference),
            se_Taxdiff = sd(difference)/sqrt(n()))

##12-Q9####

cichlids <- read_csv("datasets/abd/chapter12/chap12q09Cichlids.csv")

##Summary stats 
cichlids_summary <- cichlids %>%
  group_by(genotype) %>%
  summarise(n_fish = n(),
            mean_fish = mean(preference),
            median_fish = median(preference),
            sd_fish = sd(preference),
            IQR_fish = IQR(preference),
            var_fish = var(preference),
            se_fish = sd(preference)/sqrt(n()))


ggplot (data = cichlids)+
  geom_histogram(aes(preference), binwidth = 0.1)+
  facet_wrap(~genotype)

ggplot(data = cichlids)+
  geom_boxplot(aes(x = genotype, y = preference))+
  stat_summary(aes(x = genotype, y = preference), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)
##a)The means and the medians look very similar. There is just a larger range/variance for F2 compared to F1

##b) it does agree 

##c) test if varainces are equal 
#h0: varF1 = varF2
#halt: varF1≠varF2

varF1 <- 0.004127147
varF2 <- 0.025028695	
ftest<- varF2/varF1


##12-Q10####

president <- read_csv("datasets/abd/chapter12/chap12q10WillsPresidents.csv")

##Mutate data to make it difference in promises made (winner-loser)

president <- president %>%
  mutate (difference = willsWinner-willsLoser)

ggplot(data = president)+
  geom_boxplot(aes(x = '', y = difference))+
  stat_summary(aes(x = '', y = difference), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

# Two-sided paired t test
t.test(president$willsWinner, president$willsLoser, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

##The winner was significantly more likely to make promises (two-sided paired t test, t=4.50, df=7, p<0.0028)

##12-Q16####

mosquitos <- read_csv("datasets/abd/chapter12/chap12q16BeerAndMosquitoes.csv")


##three graphs are histograms, boxplot and apparently strip chart 

ggplot(data = mosquitos, mapping = aes(x = drink, y = change))+
  geom_boxplot(aes(color=drink))

summ_mosquito <- mosquitos %>%
  group_by(drink) %>% 
  summarise(mean_mosquito = mean(change),
            sd_mosquito = sd(change),
            n_mosquito = n())

##Two-sample t test 

# Two-sided
t.test(change ~drink, data = mosquitos, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

##t sample = 3.1913, df=41, p-value = 0.002717
#tcrit is 2.02

##12-Q17####

HIV <- read_csv("datasets/abd/chapter12/chap12q17HIVAntibody.csv")

ggplot(data =HIV)+
  geom_boxplot(aes(x = 'treatment', y = percentHealthyCD4))+
  stat_summary(aes(x = 'treatment', y = percentHealthyCD4), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

##Summary stats 

##Summary stats 
HIV_summary <- HIV %>%
  group_by(treatment) %>%
  summarise(n_fish = n(),
            mean_CD4 = mean(percentHealthyCD4),
            median_CD4 = median(percentHealthyCD4),
            sd_CD4 = sd(percentHealthyCD4),
            IQR_CD4 = IQR(percentHealthyCD4),
            var_CD4 = var(percentHealthyCD4),
            se_CD4 = sd(percentHealthyCD4)/sqrt(n()))

##Confidence INterval Step by step 

alpha <- 0.05
mean_control <- 9.33
mean_antibody <- 83.13
se_ant <- 4.80
se_control <-3.06
df_ant <- 7
df_control <- 5


# If you used summarise to calculate the descriptive statistics, then the code is
mean_control + c(-1,1)*qt(1-alpha, df_control )*se_control

##95% CI is 3.16-15.50 for control 
mean_antibody + c(-1,1)*qt(1-alpha, df_ant )*se_ant

##95% CI for antibody group = 74.04 - 92.22



