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

#5. Make a scatter plot of class vs drv (or at least try)

ggplot(data = mpg) +
  geom_point(mapping = aes (x=class, y = drv))

#this graph is not useful because this is not a relationship with continuous numbers,
#so it just looks very funny and isn't telling me about a trend because it is relating these categorical variables that do not 
#have a lot of data points at all. If you wanted to look at this data, there is definitely another graph that would better display the relationship between the type of car
#and how what the "wheel-drive" is 

#Aesthetic Mapping 

#add another layer to the graph that allows you to look at the car type 

ggplot(data=mpg) +
  geom_point(mapping = aes(x = displ, y= hwy, color = class))

#Try graphing it using size instead of color (but this is cautioned because it is an
#unordered variable we are mapping with size)

ggplot(data=mpg) +
  geom_point(mapping = aes(x = displ, y= hwy, size = class))

#Other options are using "alpha" which is transparency of the points
#or shape, which controls the shapes of the points 

ggplot(data=mpg) +
  geom_point(mapping = aes(x = displ, y= hwy, alpha = class))


ggplot(data=mpg) +
  geom_point(mapping = aes(x = displ, y= hwy, shape = class))

#sadly can only use 6 shapes at one time 

#Exercises 

#1. In this code, to manually make the dots blue you have to put the color outside the aes()

#2. Which variables are categorical, which are continuous? How can you see this info
#When you first load the data in the table format in the console, it shows the type of varible below
# the column name. You can also use ?mpg to get more info
?mpg

#Model name, cylinders, type of transmission, drv, fuel type and class are all 
#categorical variables
#Engine displacement, disply, city mpg and highway mpg are all continuous. The manufacturer
#year is discrete, but as we discusses in class has more than 6 values, so can be 
#treated as continuous in data analysis 

#3. 

ggplot(data=mpg) +
  geom_point(mapping = aes(x = drv, y = hwy, shape = displ))

#You cannot map a continuous variable to shape, because how would is decide what values 
# get what shape if it is continuous 

#4. Mapping same variable for multiple aesthetics 

ggplot(data=mpg) +
  geom_point(mapping = aes(x = displ, y= class, size = class))

# It is kind of pointless because you already have that information on an axis, putting the\
#legend with shapes is not giving me any more info

#5. what is stroke aesthetic 
?geom_point

ggplot(data=mpg) +
  geom_point(mapping = aes(x = drv, y= hwy, stroke = cty))

#stoke controls the width of the shapes to a degree. But it said it has to be used with a numeric operator 

#6. 

ggplot(data=mpg) +
  geom_point(mapping = aes(x =drv, y= hwy, color = displ<5))

#It shows the individual measurements for that variable that are less than 5 and more than
#5 using a color legend with TRUE and FALSE

# 3.5 Facets 
#Plot data and group based on type of car 
ggplot(data = mpg) + 
  geom_point (mapping = aes(x = displ, y = hwy)) + facet_wrap ( ~ class, nrow =2)

#Combine to variables, add a facet_grid 

ggplot(data = mpg) + 
  geom_point (mapping = aes(x = displ, y = hwy)) + facet_grid ( drv ~ cyl)

#Exercises 

#1. Using facet wrap with a continuous variable 

ggplot(data = mpg) + 
  geom_point (mapping = aes(x = displ, y = hwy)) + facet_wrap ( ~ cty, nrow =2)
#it will group it based off of the cty variable, so each value for city mpg is seperated then
#graphed based off of the displ and hwy variables 
