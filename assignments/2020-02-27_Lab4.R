##Lab 4 :) 

# Clean up the working environment
rm(list = ls())

### Install and load packages ####

install.packages("modelr")
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}


# Check for updates
tidyverse_update()

#Data Visualisation 

#Load mpg data frame about car gas usage and size 

ggplot2::mpg

#Creating graph of this data, a car's engine size in litres
#on the x-axis (displ) and the fuel efficiency on the highway (hwy) on y axis

ggplot(data = mpg) +
  geom_point(mapping = aes (x=displ, y = hwy))

#first line shows which data you want to use (data=mpg)
#adding the line geom_point creates a scatter plot
#ggplot 2 is a mapping argument, where you write aes in code and define
#x and y variables 

#Exercises 

#1. run just the ggplot without specifying variables 

ggplot(data=mpg)

#like mentioned before in the instructions, just writing that line of code will specifiy
#what data set to use, but will not give the program enough info, so an empty graph
#is created instead. 

#2. How many rows/columns in mpg?

#It shows 234 rows for the data set, and if you include the manufacturer as the first column, then 
#there are 11 columns of variables for this data set

#3. what does the drv variable describe? 

?mpg

#drv variable describes what kind of "wheel-drive" the car has. The options are front wheel, rear wheel and 4 wheel drive
#these describe how the power from the engine can be distributed to the wheels of the car

#4. Make a scatter plot of hwy vs cyl 

ggplot(data = mpg) +
  geom_point(mapping = aes (x=cyl, y = hwy))

