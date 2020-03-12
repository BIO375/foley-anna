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
