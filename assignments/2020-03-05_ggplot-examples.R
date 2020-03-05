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







