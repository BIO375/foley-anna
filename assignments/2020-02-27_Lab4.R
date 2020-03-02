##Lab 4 :) 

# Clean up the working environment
rm(list = ls())

### Install and load packages ####
install.packages(c("broom", "forcats", "rlang"))
install.packages("modelr")
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}


# Check for updates
tidyverse_update()

#Data Visualization 

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

#Exercises 3.5.1

#1. Using facet wrap with a continuous variable 

ggplot(data = mpg) + 
  geom_point (mapping = aes(x = displ, y = hwy)) + facet_wrap ( ~ cty, nrow=2)

#it will group it based off of the cty variable, so each value for city mpg is separated then
#graphed based off of the displ and hwy variables. It allowed me to do it, but i'm not sure how 
#much info it actually gave me 

#2. 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y= hwy)) + facet_grid(drv ~ cyl)

#the empty cells mean that there are no observations in that particular combination of 
#groups/variables displayed in the plot. 
#compare to 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

#this is showing something similar. For example, there is no 5 cylinder 4 wheel drive cars to be displayed
#This is shown in the previous plot. this is why there are no data points in the 2nd group from the left
# on the top row. Because that is the 5 cylinder and 4wd grouping

#3. 
ggplot(data = mpg) + 
geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv~.)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)

#When you put the dot after the ~ symbol, then the data is facetted by the type of drive of the car
# but then that is the only way it is divided. There isnt another attribute dividing them vertically 
# like in the plot in #2. The y axis is repeated on each of the plots
#For the dot before the ~ symbol, it does a similar thing, but will facet the groups without a horizontal attribute. 
#The data will be grouped by the cyl attribute like above, then the x axis is repeated on each individual plot 

#4. 

ggplot(data = mpg) + 
geom_point(mapping = aes(x = displ, y = hwy, color = class))

#When using the colors, it is hard to distinguish the pattern of each individual class compared to each other.
#But, it is easier to see how the classes compared to the other classes overall. For the faceting, it makes being 
#able to see the individual classes better, but then you can't see the data all together on the plot
#For the larger data sets, I feel like it would be better to see them all compared to each other on the same 
#plot, than individual plots of many points that kind of cluster together but don't really portray any meaning
#unless you are comparing them to the other classes 

#5. 

?facet_wrap

#Nrow & ncol allows you to choose the numbers of rows and columns. Other options include scales for the axes
#scales can either be free or fixed, or fixed for either x or y dimension. Shrink can be used to change the scales
#so that it will either fit the output of statistics, or be the range of the raw data before the statistical summary
#Facet-grid does not have nrow and ncol because those are already determined by the specific attributes 

#6. You want to be able to see the data as spread out as possible when you are making a plot, so putting the variable 
#with the most unique levels in the columns prevents the data from being displayed on an axis that is smaller and therefore 
#less interpretable 

#3.6 Geometric Objects 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#Note: not every aesthetic works with every geom 
#Set the linetype with Geom_smooth

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

#Different versions of the graph 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE)

#How to display multiple geoms on the same plot 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

#To avoid problems with duplicated code 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

#If you put mappings in the geom function, it will override the global mappings
#for that layer only 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

#You can get the smooth line to display just a subset, such as the compact cars
#The local argument under the geom will override the global data argument 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

#3.6.1 Exercises 

#1. Like mentioned in the chapter, I would use line geom for line charts, boxplot geom for
#box plot, histogram geom for the histogram and the area geom for the area plot. ggplot2
# has over 40 geoms for the program.

#2. This graph will have the display on the x, with the hwy mpg on the y, and the dots will
# be different colors based on the type of wheel drive the car is. There will be a line for 
#each group but with false for SE, so no confidence interval displayed 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

#3. If you put false, it will not show the legend box for the colors of the drv. So if you remove the command 
# then there will show a legend for the graph.
#example from earlier in the chapter: 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),
          )
# In that part of the question, the importance wasn't which groups each line was, but just showing that you could change the
#color of the group and distinguish them based off of color, that is probably why a legend wasn't included

#4. se argument is for displaying confidence intervals around the line, the grey area around the line. 

#5. no they will not be different. It was talked about before in the chapter, because both of the geom 
#plots will get that information from the ggplot. In the second one, ggsmooth and gg point 
# both have the same data and mappings 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

#6. Recreate R code to generate the following graphs 

#top left
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

#top right 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(mapping = aes (group = drv), se = FALSE)

#middle left 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes (group = drv, color  = drv)) + 
  geom_smooth(mapping = aes (group = drv, color = drv), se = FALSE)

#middle right

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes (group = drv, color  = drv)) + 
  geom_smooth (se = FALSE)

#bottom left 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes (group = drv, color  = drv)) + 
  geom_smooth(mapping = aes (group = drv, linetype = drv), se = FALSE)

#bottom right 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color=drv)) +
  geom_point(shape = 21, color = "white", stroke = 1)

#3.7 Statistical Transformations 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

#generally use geoms and stats interchangeably 

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))
#3 reasons why you want to state stat explicitly 

#1. override default stat 

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

#2. Override default mapping. display proportion rather than count 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

#3. draw greater attention to stat 

ggplot(data = diamonds) + 
  stat_summary(mapping = aes(x = cut, y = depth),fun.ymin = min, fun.ymax = max,fun.y = median)

#3.7 excersizes 

#1.default geom for stat summary is geom point range  
#but the default stat for geom point range is identity, so you would have to add the calculations in 
#to the previous plot 

ggplot(data = diamonds) + 
  geom_pointrange(mapping = aes(x = cut, y = depth), stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = median)

#2. 
?geom_col
##they are both bar charts. geom bar makes the height of the bar proportional to the frequencies that 
#R calculated doing the statistical transformation
#geom col allows you to make the heights of the bars representative of the raw data. geom call uses the stat identity 
#and leaves the data the way it was given to the program 

#3. 
#link to documentation---> http://ggplot2.tidyverse.org/reference/
#The link shows similarities in the names. here is a list of some pairs
  
#geom bar/geom col--stat count 
#geom bin2d--stat bin2d
#geom boxplot--stat boxplot 
#geom contour--stat contour
#geom count -- stat sum 
#geom density -- stat density 
#geom density 2d--stat density 2d
#geom hex -- stat bin hex 
#geom freqpoly/geom histogram -- stat bin 
#geom qq line -- stat qq line 
#geom qq -- stat qq
#geom quantile -- stat quantile 
#geom smooth -- stat smooth 
#geom violin -- stat ydensity 
#geom sf -- stat sf

#Therefore most of the geom and stat pairs have similar names 
#This is not the case for every single one, for example geom count & stat sum 

#4. 
?stat_smooth
#the computed variables for stat smooth are y:predicted value, ymin: lower pointwise confidence interval
#around the mean, ymax:upper pointwise confidence interval around the mean
#and se: standard error. I believe the parameters are method, formula, n, span, na.rm, se, span, fullrange, level

#5. 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))

#all of the bars are equal to 1, so the graph does not display the correct information.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))

#the problem is that the proportions that are calculated are all equal to 1 because they are 
#counting it within a group, so treating each variable separately when doing this calculation
# need to override the behavior and change proportion relative to the whole group

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

#3.8 Position Adjustments 
#coloring charts 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

#fill aesthetic with another variable 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
#position = identity will place each object where it falls in the graph 

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

#position = fill works like stacking, but makes each bar same height
#useful for comparing proportions across groups 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
#position = dodge places overlapping objects beside one another to compare individual 
#values 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

#position = jitter for scatterplots. makes your graph more revealing on large scale 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

#3.8.1 exercises

#1. 
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
geom_point() 

#this looks like thte overplotting issue we saw in the chapter. Fails to show where he mass
#of the data is. To fix it, I would use geom_jitter 

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

#2. The parameters that control the amount of jitter would be height and width 
#the default introduces both horizontal and lateral movement on the plot 

#3. 
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()
#geom count allows you to see how often a certain data point was found, put like an emphasis, using size, 
#on the chart to display that data. THis is how geom count achieves this. FOr geom jitter, it uses vertical and horizontal
#displacement to help show where the mass of data is. Geom jitter sacrifices the individual accuracy of the points 
#to do this, while geom count does not

#4. 
?geom_boxplot
#the default position is dodge 2
?position_dodge2
#it preserves the vertical position in a geom but adjusts the horizontal position to prevent overplotting

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(y = displ, x = drv, color = factor(class)))

#3.9 Coordinate systems 
#coord flip switches x and y, useful for long labels 


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

#coord quickmap sets the aspect ratio correctly for maps

#coordpolar uses polar coordinates 

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

#3.9.1 exercises 

#1. 
ggplot(data = diamonds) + 
geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
 #this is the stacked bar chart

bar <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity),position = "fill", show.legend = FALSE,width = 1) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

