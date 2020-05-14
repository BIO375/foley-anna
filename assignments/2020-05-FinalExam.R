### Final Exam  ####
rm(list = ls())

library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

insulation <- read_csv("datasets/demos/insulation.csv")

##a####

#Identify Variables

#Response variable: "heat_loss": change in body temperature/min (continuous)
#Predictor Variable: "leanness": adjusted skin fold thickness index (continuous)

#b####
##Statistical null

#slope of linear model = 0, b1 = 0

##c & d ####
##Default test for two continuous variables and testing the causal relationship between them is linear regression
##Assumption of linear regression: 

#i) Relationship is linear? 

ggplot(data = insulation) +
  geom_point(mapping = aes(x = leanness, y = heat_loss ))

##generally as points increase for x, they increase for y in a linear fashion, so that assumption is met

##ii) Use residual plot to check assumption of normally distributed residuals and equal variance of residuals 

model01 <- lm(heat_loss ~ leanness, data = insulation)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

# Use the function resid() right in the plotting command
ggplot(data = insulation)+
  geom_point(aes(x = leanness, y = resid(model01)))

##Although it does not look perfect because of the pretty small sample size, the residual plot does not look fan shaped
#even with the outliers and the normal Q-Q plot does not look terribly not linear which means that the data meet the assumptions for 
#a linear regression. Doing transformations such as square root or log did not improve the data significantly, 
#and the residual plot for log might even look a bit fan-shaped, so I do not believe a transformation is warranted. 

##sqrt
insulation <- insulation %>%
  mutate(sqrt_heat = sqrt(heat_loss))

model02<-lm(sqrt_heat ~ leanness, data = insulation)

autoplot(model02, smooth.colour = NA)
ggplot(data = insulation)+
  geom_point(aes(x = leanness, y= resid(model02)))

##log
insulation <- insulation %>%
  mutate(log10_heat = log10(heat_loss))

model03<-lm(log10_heat ~ leanness, data = insulation)

autoplot(model03, smooth.colour = NA)

ggplot(data = insulation)+
  geom_point(aes(x = leanness, y= resid(model03)))


#The appropriate statistical test is linear regression, because we are looking to test the
#causal relationship between leanness and heat loss and how much variation in heat loss is 
#caused but leanness

##e####

summary(model01)

##Boys with higher leanness scores have significantly more heat loss 
#(Linear Regression: heat_loss = -0.02691 + 0.01897(leanness);
#df = 1,10, F=68.73, p=8.61e-06) and leanness explained more than 85% of the variability in
#heat loss (R2 = 0.873)


##add regression line to plot 

ggplot(data = insulation, aes(x = leanness, y = heat_loss)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Leanness", y = "Heat Loss (Change in C)")






