

# Data Visualization on packages quakes and mpg

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}

if(!("ggplot2" %in% rownames(installed.packages()))){
  install.packages("ggplot2")
}

if(!("ggpubr" %in% rownames(installed.packages()))){
  install.packages("ggpubr")
}

library(tidyverse)
library(ggplot2)
library(ggpubr)

# ********************************* Question A *********************************

if(!("caret" %in% rownames(installed.packages()))){
  install.packages("caret")
}

library(caret)

?quakes

# The quakes data set(1000 X 5) contains information on 1000 seismic 
# events of MB > 4.0 occurred near Fiji since 1964.
# Variables include Latitude, Longitude, Depth (km), Richter Magnitude,
# and number of stations reporting.  


# A1----------------------------------------------------------------------------

# Scatter plot of monitoring Quake magnitude against number of stations. 

req_label <- labs(x = "Number of Stations",
                  y = "Quake magnitude",
                  title = "Relation b/w Magnitude & Stations") # required label

req_theme <- theme(
                plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                axis.title.x = element_text(size = 10, hjust = 0.5),
                axis.title.y = element_text(size = 10, hjust = 0.5)
              )     # required theme

quakes %>%
  ggplot(aes(x = stations, y = mag)) +
  geom_point() +
  geom_smooth() +
  req_label +
  req_theme

# As, Richter magnitude is calculated using the distance between the 
# epicenter->station and with the amplitude of the s-wave, it is independent 
# of number of stations. Rather, number of stations reporting depends on 
# Richter magnitude (intensity of quake). So, mag is an explanatory variable
# and station is a response variable. For this reason, we can consider 
# coord_flip() as shown below. 

quakes %>%
  ggplot(aes(x = stations, y = mag)) +
  geom_point() +
  geom_smooth() +
  coord_flip() +
  req_label +
  req_theme 

# A2----------------------------------------------------------------------------

# Yes, the interpretability of the plot can be improved by:- 

# 1. Assigning Size, Alpha - 

quakes %>%
  ggplot(aes(x = stations, y = mag)) +
  geom_point( size = 1, alpha = 0.5) +
  geom_smooth() +
  coord_flip() +
  req_label +
  req_theme 

# Reducing the size of points and lowering the alpha (transparency)
# of points helps to reduce over plotting.

# 2. Using jitter -  

quakes %>%
  ggplot(aes(x = stations, y = mag)) +
  geom_jitter() +
  geom_smooth() +
  coord_flip() +
  req_label +
  req_theme 

# geom_jitter() adds a small amount of random variation to the location of each
# point which helps to handle over plotting.

# 3. 2D density maps- 

quakes %>%
  ggplot(aes(x = stations, y = mag)) +
  geom_bin2d(bins = 15) +
  geom_smooth() +
  coord_flip() +
  req_label +
  req_theme 

# Applying geom_bin2d() creates a density plot, colored on the gradient scale,
# depending on the number of observations in it's occupied space. 
# It helps to deal with over plotting as the color gradient gives a view 
# of number of occurrences at all different locations. 

# A3----------------------------------------------------------------------------

# Relation b/w stations and mag -

# Direction - Positive direction, mag and station are related.
# Pattern - The graph depicts a near linear trend, increase in magnitude 
#           leading to increase in stations.
# Strength - There is a moderate->high strength between mag and stations. 
# Outliers - Possibility of outliers as there exists few extreme values for 
#            both earthquake's magnitude and number of stations. 

# Cause of trend -

help("quakes")

# mag variable is used to display the Richter Magnitude(strength of earthquake) 
# for every quake in the data set.

# stations variable describe the number of stations that reported 
# the earthquake. 

# Whenever an earthquakes takes place, it sends seismic waves around the globe.
# When the earthquake is highly intense, the impact produced is huge and more 
# number of stations are able to observe/catch & report it in time. 

# Logically, as the magnitude(strength) increases, the stations variable should
# also increase. 

# Correlation (Using Pearson method) -

cor.test(quakes$mag, quakes$stations, method = 'pearson')

# Here value of Pearson correlation coefficient is 0.8511824(close to 1) 
# which tells that magnitude and stations hold a strong correlation.

# A4----------------------------------------------------------------------------

# Why check regardless of presumption - 

# 1. Exploratory data analysis is not a traditional visualization approach.
# In EDA, No prior assumptions are made about the data - allowing 
# the data itself to reveal the underlying structure and model.

# 2. Logical presumption only tells that stations is directly proportional 
# to mag, but it doesn't talk about the gradient(steepness) of relation.

# 3. Logical presumptions miss out outliers, which are important in 
# analysis purposes. 

# 4. Logical presumption lacks in precision of the strength between the 
# variables. 

# 5. Plotting verifies certain assumptions we make sometimes.

# A5----------------------------------------------------------------------------

# Add a color set to depth -

quakes %>%
  ggplot(aes(x = stations, y = mag, color = depth)) +
  geom_jitter() +
  geom_smooth() +
  scale_color_gradient(low = "navy", high = "lightpink") +
  req_label +
  req_theme 

# Our goal is to predict magnitude, but here depth doesn't seem to play 
# any major role as high/low depth values are evenly distributed throughout
# the plot and not following any trend/pattern.

# Verify using a correlation plot -

if(!("GGally" %in% rownames(installed.packages()))){
  install.packages("GGally")
}

library(GGally)

df1 <- quakes[, c("mag","stations","depth")] # select required variables in df1

# correlation plot

ggcorr(df1) + 
  ggtitle("Relation between mag, stations, & depth") +
  req_theme 

# This plot clearly depicts that both mag and stations do not hold
# any strong relation with the depth variable whereas station and mag hold a 
# strong relation within themselves as seen in the earlier plots as well. 

# Correlation Value (Using Pearson method) -

cor.test(quakes$mag, quakes$depth, method = 'pearson')

# Here value of Pearson correlation coefficient is -0.2306377
# which tells that magnitude and depth does not hold a strong correlation, 
# but may hold a weak correlation. 

# A6----------------------------------------------------------------------------

# Scatter plot using lat and long -

quakes %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Latitude vs Longitude") + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5)
  )

# Half plot first follows a downward trend and then the other half sees an
# upward trend. 
# It is a non-linear trend. 
# There is a weak relation between lat and long. 
# Possibility of outliers as there exists few quakes for extreme values of
# lat and long. 

# A7----------------------------------------------------------------------------

# To increase the value and interpretability of this plot, adding geom_jitter()
# and setting color aesthetic to different numeric variables for e.g. -
# depth or mag. 

# Adding color aesthetic to mag variable-

quakes %>%
  ggplot(aes(x = long, y = lat, color = mag)) +
  geom_jitter(width = 5) +
  scale_color_gradient(low = "navy", high = "lightpink") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Latitude vs Longitude") + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5)
  )

# As the mag variable does not seem to follow any pattern/trend in color 
# display, it shows that mag does not hold a strong relation with  
# latitude/longitude. This can be also checked by plotting a correlation 
# plot/matrix as shown below.

df2 <- quakes[,c("lat","long","mag")] # select required variables in df2

# correlation plot
ggcorr(df2) +
  scale_fill_gradient2(name = "Correlation",
                      limits = c(-1,1),
                      low = "navy",
                      mid = "ivory2",
                      high = "lightpink")

# correlation matrix
cor(df2)  

# This correlation plot and matrix depicts that there is a minor relation 
# between latitude and longitude but both fail to have any major connection
# with the quake magnitude(mag). 


# Similarly, adding color aesthetic to depth variable-

quakes %>%
  ggplot(aes(x = long, y = lat)) +
  geom_jitter(width = 5,aes(color = depth)) +
  scale_color_gradient(low = "navy", high = "lightpink") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Latitude vs Longitude") + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5)
  )

# Looking at the plot, depth seems to have a bi-variate dependency on
# the geographical location (long AND lat) i.e. depth holds large value 
# in a certain range of coordinates.

# Coordinates ranging from (175,-27) to (185,-17) where x coordinate
# represents Longitude and y coordinate represents Latitude. 

# ********************************* Question B *********************************

help(mpg)

# Description - mpg data set(234 X 11) contains a subset of fuel economy data.
# Variables of interest are - cty(city miles per gallon), hwy(highway miles 
# per gallon)

# My theme for the plots stored in my_theme (appended wherever required). 

my_theme <- theme(
  panel.background = element_rect( fill = "ivory2" , color ="gray"),
  plot.background = element_rect( color = "black"),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = 12, hjust = 0.5),
  axis.title.y = element_text(size = 12, hjust = 0.5),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank()) 


# B1----------------------------------------------------------------------------

# No. of unique manufacturers in the data set- 

length(unique(mpg$manufacturer))

# There are total 15 unique manufacturers in the data set. 

# Print names of these unique manufacturers- 

print(unique(mpg$manufacturer))

# B2----------------------------------------------------------------------------

# Histogram of hwy variable- 

# Storing plot in hwy_hist variable- 

hwy_hist <- mpg %>%
  ggplot(aes(hwy, fill = ..count..)) +
  geom_histogram(binwidth = 2) +
  scale_fill_gradient(name = "Freq.(hwy)",
                      low = "navy",
                      high = "lightpink") +
  labs(x = "hwy (Highway miles per gallon)",
       y = "Count",
       title = "Frequency for range of hwy") +
  my_theme


# display the plot (hwy_hist)

hwy_hist 

# The median is around 25 miles. 
# The maximum values of hwy falls within 25-27 miles( binwidth = 2 ). 
# This is a non symmetric distribution, skewed to the right.
# Also, this is a bimodal distribution, when binwidth = 2. 
# There is a steep decline in frequency, once the maximum bar of around 
# 27 miles is crossed.
# The data values(hwy) stretch in a range of around 11-44 miles.
# There exists probably two outliers accounting for that minor 
# stretch in x axis.  

# B3----------------------------------------------------------------------------

# Adding fill aesthetic set to *cyl*

stacked_hwy_hist <- mpg %>% 
  ggplot(aes(hwy)) +
  geom_histogram(aes(fill = as.factor(cyl)), 
                 position = position_stack(),
                 binwidth = 2) +
  labs(x = "hwy (Highway miles per gallon)",
       y = "Count",
       title = "Frequency for range of hwy") +
  my_theme +
  scale_fill_manual(name = "No. of cyl",
                    values = c("red3","orangered","orange","yellow"))

# Display the stacked graph- 

stacked_hwy_hist

# The graph depicts that number of cylinders have an impact on hwy 
# (highway miles per gallon).
# hwy has a *roughly* inverse relation with no. of cylinders. 
# Vehicles with less number of cylinders have high values of hwy and vice
# versa, with some overlaps in between the values. 
# Also, there are least number of vehicles with 5 cylinders as most vehicles
# contain 4, 6 or 8 cylinders. 
# The outliers having high values for hwy falls under the 4 cylinders category.


# The relation between hwy and cyl can be tested using Pearson method- 

cor.test(mpg$hwy, mpg$cyl, method = 'pearson')

# Here value of Pearson correlation coefficient is -0.7619124 (close to -1) 
# which tells that hwy and cyl hold a moderate->strong correlation.

# B4----------------------------------------------------------------------------

# Histogram of cty variable- 

# Storing plot in cty_hist variable- 

cty_hist <- mpg %>%
  ggplot(aes(cty, fill = ..count..)) +
  geom_histogram(binwidth = 4) +
  scale_fill_gradient(name = "Freq.(cty)",
                      low = "navy", 
                      high = "lightpink") +
  labs(x = "cty (City miles per gallon)",
       y = "Count",
       title = "Frequency for range of cty") +
  my_theme

# display the plot (cty_hist)

cty_hist 

# The median is somewhere around 16 miles. 
# The maximum values of cty falls within 14-18 miles( binwidth = 4 ).  
# This is a non symmetric distribution, skewed to the right.
# Also, this is a uni modal distribution, when binwidth = 4. 
# The data values(cty) stretch in a range of around 6-35 miles. 
# There exists 2-4 outliers accounting for that minor 
# stretch in x axis. 


# B5----------------------------------------------------------------------------

# Comparing cty with hwy

ggarrange(hwy_hist,cty_hist, nrow = 2) 

# On comparing both charts,

# 1. hwy max out at around 44 whereas cty max out at 35. 
# 2. center for hwy and cty is around 25 and 16 respectively.
# 3. Min value for hwy around 11 miles/gallon, for cty around 6 miles/gallon.
# 
# This justifies the expected that highway miles/gallon would have 
# larger values than city miles/gallon.

# Both the Plots are non-symmetric and right skewed.

# Both the plots contain around 2-4 outliers.

# There is a difference in frequency range for both the plots because of the
# difference in binwidth. As, the binwidth (range for every block) increases,
# y axis stretches alongside too, as then there are more values falling in 
# that bigger binwidth. 

# Adding fill aesthetic set to *cyl*

stacked_cty_hist <- mpg %>%
  ggplot(aes(cty)) +
  geom_histogram(aes(fill = as.factor(cyl)), position = position_stack(),
                 binwidth = 4) +
  labs(x = "cty (City miles per gallon)",
       y = "Count",
       title = "Frequency for range of cty") +
  my_theme +
  scale_fill_manual(name = "No. of cyl",
                    values = c("red3","orangered","orange","yellow"))

# Display the stacked graph- 

stacked_cty_hist

# Comparison and interpretation- 

ggarrange(stacked_hwy_hist,stacked_cty_hist, nrow = 2) 

# The graph depicts that number of cylinders have an impact on cty similar to
# what is observed in hwy.

# cty also has a *roughly* inverse relation with no. of cylinders. 

# Vehicles with less number of cylinders have high values of cty and vice
# versa, with some overlaps in between the values. 

# Also, there are least number of vehicles with 5 cylinders as most vehicles
# contain 4, 6 or 8 cylinders. 

# The outliers having high values for cty falls under the 4 cylinders category.

# B6----------------------------------------------------------------------------

# Bar plot for distribution of values in class variable

mpg %>%
  mutate(cyl = as.factor(cyl)) %>%
  ggplot(aes(x = class, fill = cyl)) +
  geom_bar(position = "stack") + 
  scale_fill_manual(name = "cyl",
                    values = c("tomato2", "yellowgreen", "cornflowerblue", 
                               "slateblue4")) +
  labs(x = "Class (type of car)",
       y = "Count",
       title = "Freqency(class) with stacked cyl") +
  my_theme

# Description and interpretation of the plot in next question.

# B7----------------------------------------------------------------------------

# Rearranging axis in ascending order- 

mpg %>% 
  mutate(cyl = as.factor(cyl)) %>%
  group_by(class,cyl) %>%
  summarize(freq = n()) %>%
  ggplot( aes(fct_reorder(class, freq, sum), freq, fill = cyl)) +
  geom_col() + 
  scale_fill_manual(name = "cyl",
                    values = c("tomato2", "yellowgreen", "cornflowerblue", 
                               "slateblue4")) +
  labs(x = "Class (type of car)",
       y = "Count",
       title = "Freqency(class) with stacked cyl") +
  my_theme

# There is a huge variation in frequency of different class of vehicles. 
# Suv's are most popular whereas 2seater's are least popular.

# Cylinders 4,6, and 8 are almost equally common as a whole but the variation 
# in no. of cylinders is different within every subgroup of class. 

# SUV's and pickup have maximum vehicles with 8 cylinders, 
# compact and subcompact have maximum vehicles with 4 cylinders,
# and midsize, minivan have maximum with 6 cylinders. 

# 2 seaters only have the data set for 8 cylinders. 
# 5 cylinders can be only be seen in compact and subcompact vehicles. 

# B8----------------------------------------------------------------------------

# The above color scale is of type qualitative.

# Qualitative is used mostly when there is no ordering of data, and color 
# simply allows to differentiate groups.

# Here we have taken cylinder as factor, so as there are individual levels,
# qualitative type of color works in this scenario. Had we taken cyl in 
# it's original form i.e. int, then we would have to apply scale_fill_gradient
#  as cyl will become continuous.

# The purpose of qualitative scale is to separate different groups. So, it gives
# an effective visualization, where we are not aware of the actual values
# but just with this color difference we are able to notice some details. 

# Also, it is better to select varied set of colors, so that even when 
# the levels are more in number, the difference is seen clearly. Here,
# two shades of *Blue* are used, one of them can be changed to yellow or brown
# for better visibility. 

# B9----------------------------------------------------------------------------

# Median cty and hwy against each type of fuel- 

median_cty_hwy <- mpg %>%
                    group_by(fl) %>%
                    summarize(median_cty = median(cty, na.rm = TRUE), 
                              median_hwy = median(hwy, na.rm = TRUE))

# Plot for median cty against fuel.

plot_median_cty <- median_cty_hwy %>%
                    ggplot(aes(x = reorder(fl, median_cty), y = median_cty)) +
                    geom_bar(aes(fill = fl), stat = "identity") +
                    labs(x = "Type of Fuel",
                         y = "Median(cty)",
                         title = "Median(cty) against fuel type") +
                    my_theme

# Display plot median_cty -

plot_median_cty


# Plot for median hwy against fuel.

plot_median_hwy <- median_cty_hwy %>%
                    ggplot(aes(x = reorder(fl, median_hwy), y = median_hwy)) +
                    geom_bar(aes(fill = fl), stat = "identity") +
                    labs(x = "Type of Fuel",
                         y = "Median(hwy)",
                         title = "Median(hwy) against fuel type") +
                    my_theme

# Display plot median_hwy -

plot_median_hwy

# Description for both the plots in next question.

# B10---------------------------------------------------------------------------

# Comparing above plots- 

ggarrange(plot_median_cty, plot_median_hwy, nrow = 2) 

# Both the plots have a similar trend across fuel type. 
# Ascending order for median values for both the plots follow same order on
# fuel type i.e. 'e' < 'r' < 'p' < 'c' < 'd'.
# Fuel type 'd' provides best efficiency(hwy and cty) whereas fuel type 'e'
# gives the lowest efficiency in terms of hwy and cty. 
# Median values for cty plotted across fuel type range from 
# around 9 to 29 miles/gallon. 
# Median values for hwy plotted across fuel type range from 
# around 12 to 41 miles/gallon.
# hwy has higher values than cty for every type of fuel which justifies the
# expected that highway miles/gallon have bigger values than city miles/gallon. 

# B11---------------------------------------------------------------------------

mpg %>%
  ggplot(aes(x = manufacturer, y = cty, fill = manufacturer)) +
  geom_boxplot() +
  coord_flip() +  # to avoid overlaps between names of manufacturers. 
  labs(x = "Manufacturer",
       y = "cty (city miles per gallon)",
       title = "cty for every manufacturer") +
  my_theme


# Clearly, manufacturer has a significant impact on the cty.

# The lowest average(median) for cty can be seen in 'lincoln' and maximum 
# average(median) for cty can be seen in 'Honda' indicating that Honda
# vehicles maybe more efficient(cty) as compared to other manufacturers.

# For some plots, median is overlapping with interquartile range on either Q1 or
# Q3, indicating that there are considerably less number of observations
# for these categories. 

# 'toyota' has maximum spread whereas 'mercury' has the least spread. 
# 'chevrolet', 'audi', 'jeep', 'ford' seems to have almost equal spread on plot.

# Outliers can be seen in four categories, where mercury and chevrolet both
# have one outlier and 'volkswagen' has maximum number of outliers i.e. 3, 
# accounting for that stretch in the plot.
# 'Honda' has outliers in both directions.
 

# Caveats and cautions
# There are many cases which have very thin spread because of very low number
# of observations.
# Lower number of observations is a caveat as they may allow for higher medians
# or provide wrong information.
# So, adding geom_jitter() will help to see an overview of all the 
# observations. Either geom_jitter() can be attached to the box plot
# OR make a violin plot which gives a view of the distribution
# and occurrences of individual observations. 


# B12---------------------------------------------------------------------------

# To improve interpretation - 

# 1. Adding stat_summary() and geom_jitter()

mpg %>%
  ggplot(aes(x = manufacturer, y = cty, fill = manufacturer)) +
  geom_boxplot() +
  geom_jitter(width = 0.33, size = 1, color= "brown") +
  coord_flip() + # to avoid overlaps between names of manufacturers. 
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 3,
               color = "white",
               fill ="white") +
  labs(x = "Manufacturer",
       y = "cty (city miles per gallon)",
       title = "cty for every manufacturer") +
  my_theme


# It can be seen that there are categories with very few number of observations
# for e.g. 'mercury', 'lincoln', 'land rover', 'pontiac' etc and 
# such things then lead to providing wrong information. 

# Categories like 'dodge', 'chevrolet', 'volkswagen', 'ford' etc have a 
# significant no. of observations indicating that maybe these manufacturers are 
# more common among people than others. 

# For most of the manufacturers, mean seems to overlap median with a few 
# exceptions such as 'Nissan' where mean is significantly below median.

# Mean values deviate either due to the presence of outliers as outliers hold an 
# impact on pulling means toward themselves or because there are not enough 
# number of observations. 

# 2. Violin plot- 

mpg %>%
  ggplot(aes(x = manufacturer, y = cty, fill = manufacturer)) +
  geom_violin() +
  coord_flip() + # to avoid overlaps between names of manufacturers. 
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 3,
               color = "white",
               fill ="white") +
  labs(x = "Manufacturer",
       y = "cty (city miles per gallon)",
       title = "cty for every manufacturer") +
  my_theme

# Violin plots have one drawback of not being able to display outliers,
# otherwise violin plots can be used to replace the working of (boxplot+jitter)



# ****************************** Thank you ********************************




