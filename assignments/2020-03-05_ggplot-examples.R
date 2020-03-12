##ggplot examples##

#updated lab 4#

#install.packages("tidyverse")
library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
tidyverse_update()

#Introduction####

# ggplot template
# ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

###Dot plot & Aesthetics####
#Simple
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

##Adding Aesthetics 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
##for the size graph, it does not advised doing size for a discrete variable

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
##alpha changes the transparency of the groups 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
##max 6 shapes 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
##color needs to be after the parentheses 
##mapping a continuous variable to color gets you a legend on the side with a gradient 

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class, stroke = cyl), shape = 21)
##stroke is the thickness of the line outlining the shape, can only do for shapes 21-24

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))
##adding a statement at the end that changes the color of the dots based off of TRUE and FALSE

##Facets####

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow=2)
##Groups it based on class, nrow specifies how many rows of graphs it splits it to

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(cyl ~ drv)
##This groups them based on the two things in the facet parentheses. The first one is the vertical grouping
##the second one is the horizontal grouping 
#other vertical/horizontal alternatives for facetting 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+
  facet_wrap(vars(cyl,drv))
#vars is another alternative for coding, does it with two different variables 

##Geometric Objects####

#Geom smooth plots the data in a line 
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

##Adding linetype then puts a line for each of the groups in the variable you wrote, and gives you a legend 
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

##With group, it groups it based off the variable, but does not change the linetype and does not give you a legend
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

##Complicated one :)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

#This has 2 layers, the point and the smooth geom
#The smooth line was for just the subcompact group, se false takes the greying around the line away 
#se=FALSE has to be out of the asthetic brackets 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(mapping = aes(group = class), se = FALSE)

##Special Line Creating Case 

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)
# But this is easier to do with geom_smooth:
p + geom_smooth(method = "lm", se = FALSE)

##Geom Line and Geom Path####

# geom_line() is suitable for time series
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line()
# geom_path lets you explore how two variables are related over time,
# e.g. unemployment and personal savings rate
m <- ggplot(economics, aes(unemploy/pop, psavert))
m + geom_path()
m + geom_path(aes(colour = as.numeric(date)))

##Legend 
#putting legend=false takes the legend away 
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE)

##Lots of settings for the graphs, Examples####

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5)+
  geom_smooth(se = FALSE, size = 2)
##size in point changes the size, size of smooth changes line size 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5)+
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)
#same as previous graph, but grouped lines by their drv

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)
##Changed the points and grouped them by drv with color, smooth line grouped same as previous graph

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(se = FALSE, size = 2)
##taking out the mapping and grouping under the geom smooth places the line on all the data instead of the groups

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(mapping = aes(group = drv, linetype = drv,), se = FALSE, size = 2, color ="green")
##adding linetype added the different pattern lines instead of 3 smooth blue ones
##also adding the color changes the color of the line 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point( size = 6, stroke = 2, color = "white")+
  geom_point( mapping = aes(color = drv), size = 3)
#this is the stroke setting, where you can change the size of the outline of the shape

##Statistical Transformations####

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))
##This is when you specify the stat that you would like to be graphed on the y axis. 
##You have to put group = 1 so that all the proportions will add up to 1 in total 

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )


##Creates a table & plot 

diamonds_summary <- diamonds %>%
  group_by(cut) %>%
  summarise(min_depth = min(depth),
            max_depth = max(depth),
            med_depth = median(depth))
ggplot(data = diamonds_summary, mapping = aes(x = cut, y = med_depth)) +
  geom_pointrange(mapping = aes(ymin = min_depth,
                                ymax = max_depth) )

#calculates numbers then uses them to create the plot 

ggplot(data = mpg, aes(x = fl))+
  geom_bar()
ggplot(data = mpg, aes(x = fl, y = hwy))+
  geom_col()

#geom bar does not need a y variable, but geom col does

#Plots & functions info####

#     geom_histogram, stat = "bin"
#     geom_bar, stat = "bar"
#     geom_col stat = "count"
#     geom_point stat = "identity"
#     geom_pointrange stat = "identity"
#     In terms of what they have in common, it is what R is calculating behind your back and then displaying

##Linear Regression Example####

#   INSTEAD, for a linear regression, we use method = "lm" which stands for linear model. 
#     the equation of the best fit line allows us to plug in each value of x and then get the predicted y.
#     So for example, in the graph below, I'm plotting a simple dataset looking at the relationship
#     between rainfall and biomass.  I write the equation in gthe form y ~ x, or biomass ~ rainfall.
environment <- read_csv("datasets/r4all/environment.csv")
plot01 <-ggplot(data = environment, mapping = aes(x = rainfall.m, y = biomass.g.per.m2))+
  geom_point()
plot01
# When I fit the linear model, biomass.g.per.m2 ~ rainfall.m, part of the output is the slope and intercept
model01 <- lm(biomass.g.per.m2 ~ rainfall.m, environment)
model01
# I can create a new column in the datatable called predicted OR, lazy way, just plot the fitted values
plot02 <- plot01+
  geom_point(mapping = aes(x = environment$rainfall.m, y=model01$fitted.values), color = "red")
plot02

# Each of those fitted values has its own 95% confidence interval!  Then R connects the C.I.s and makes
# a nice smooth curve, so long as I specify the smoothing method as lm.

plot03 <- plot02+
  geom_smooth(method = "lm")
plot03

# Of course, the default method is this messy looking loess fit instead of a pretty linear fit.  
plot04 <- plot02 +
  geom_smooth()
plot04

##Bar graphs with aethetics####

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))
##color groups within each x axis group 

##Position Adjustments Bar Graphs ####

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
##colour outlines the bars with a colour

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
##fill will fill the bars with color instead

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
##This will fill each individual cut based off of the clarity variable 


##Plots with Position####

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
##alpha is the transparency, identity overlaps the individual parts 

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")
##this is similar as before, but now with color instead of transparency 

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
##position fill stacks the bars & makes them have a consistent height 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
##dodge puts the clarity into different groups horizontally to preserve their height


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
#position jitter is used by slightly changing the vertical and horizontal position of the points 
#to avoid overplotting. geom_jitter makes overlapping points more visible by jittering them slightly
?position_jitter

##width and height control jitter in the geom_jitter()##
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width=0.2, height = 0)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width = 0, height=0.2)
?geom_jitter
?geom_count

##geom_count  makes the point size scale with the number of points that overlap.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_count()

ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=class))
##This is a boxplot with each class in each driving group. dodge is a default for boxplot

##Coordinate Systems####

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
##This flipped it from vertical box plot to horizontal box plots

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
##this made the bars horizontal instead of vertical
bar + coord_polar()
##Does this weight proportional polar thing with the data, makes it circular 
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = cut))+
  coord_polar()


##Maps#### 
install.packages("maps")
library("maps")
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
##quickmap is for people who care less about the accuracy 

##Final Thoughts####
?labs

#lets you modify axis, legend and plot labels 

##Coord fixed lets you keep the axis ratios of x and y the same 

### 3.10.  The layered grammar of graphics, Template ####

# Template
# ggplot(data = <DATA>) + 
# <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>




