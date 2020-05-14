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

#Response variable: change in body temperature/min (continuous)
#Predictor Variable: "leaness": adjusted skin fold tickness (coontinous)

#b####
##Statistical null

#slope of the fit line =0, B = 0

##c####
##Default test for two continous variables and testing the causal relationship between them is linear regression
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

##Although it does not look perfect because of the pretty small sample size, it does not look fan shaped, 
#which means that the data meet the assumptions for a linear regression

#d####

#The appropriate staistical test is linear regression, because we are not just looking for an association between
#the two varibales, but a causal relationship between leanness and heat loss 

##e####

summary(model01)

##Boys with higher leanness scores have significantly more heat loss 
#(Linear Regression: heat_loss = -0.0269 + 0.0190(leanness);
#df = 1,10, F=68.73, p=8.61e-06) and leanness explained more than 85% of the variability in
#heat loss (R2 = 0.873)

ggplot(data = insulation, aes(x = leanness, y = heat_loss)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Leanness", y = "Heat Loss (Change in C)")






