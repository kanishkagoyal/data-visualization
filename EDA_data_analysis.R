
#--------------------------------------------------------------------------

# Question number 1 

library(tidyverse)
library(ggplot2)

# 1a ( Description - Chickweight )---------------------

?ChickWeight

# The Chickweight data set(578 X 4) consists of 578 observations from an 
# experiment conducted to test the effects of diet on early growth of chicks. 
# It contains 4 variables:- Weight(gm), Time(no. of days since birth), 
# Chick(a unique identifier), Diet( diet received). 

# 1b ( Weight vs Diet )---------------------

ggplot(ChickWeight, aes(x = Diet,y = weight)) + 
  geom_boxplot() +
  xlab("Type of Diet") +
  ylab("Weight (gm)") + 
  ggtitle("Effect of diet on weight") +
  theme_classic()

# Clearly, type of diet has an impact on the weight of chick.
# With type of diet, there is a linear increase   
# in average weight(Median) following type(1 -> 4) where Diet-1 has
# the least average and Diet-4 has the maximum average. 
 

#1c ( Extended box-plot )---------------------

ggplot(ChickWeight, aes(x = Diet, y = weight, fill = Diet)) + 
  geom_boxplot() +
  geom_jitter(width = 0.33, size = 1, color= "burlywood1") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "blue",
               fill ="blue") +
  xlab("Type of Diet") +
  ylab("Weight (gm)") + 
  ggtitle("Effect of diet on weight") +
  theme_light()
 

# 1d ( Comparison )---------------------

# Type of diet does have an impact on the weight of chick.
# Diet 3 has maximum spread over the weight variable, whereas
# Diet-2 and Diet-4 seems to max out near the same point. Diet-1 has
# the least spread.
# Maximum weight also falls under Diet-3. 
# Mean being greater than median indicates the presence of outliers and 
# Diet 1,2, and 3 all have their mean values largely above the median.  
# Diet 1 seems to have least effect on weight of chick indicated by the least 
# mean and median value.

ggplot(ChickWeight, aes(x = Diet, y = weight, fill = Diet)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "blue",
               fill ="blue") +
  xlab("Type of Diet") +
  ylab("Weight (gm)") + 
  ggtitle("Effect of diet on weight") +
  theme(
    panel.background = element_rect( fill = "ivory2" , color ="gray"),
    legend.key = element_rect(fill = "gray", color = "gray"),
    legend.background = element_rect(fill = "white", color = "white", size = 2),
    plot.background = element_rect(fill = "white", color = "black")
  )

# There was a lot of over plotting while using Boxplot with geom_jitter(),
# so geom_violin() helps to overcome overlaps like these. 
 
# 1e ( Median weight vs Time )---------------------

ChickWeight %>%
  group_by(Time = as.factor(Time)) %>%
  summarize( Median = median(weight)) %>%
  ggplot(aes(x = Time , y = Median)) +
  geom_bar(aes(fill = Median),stat= "identity") + 
  xlab("Time") +
  ylab("Median weights(gm)") + 
  ggtitle("Median weights vs time") +
  theme_minimal()

# Bi-variate bar plots are usually used for the visualizations of wrangled 
# data sets, so they should be utilized when comparing the values of 
# individual levels of a variable.
# In this distribution, clearly the average weight is increasing 
# with the increase in time.
# Time has a significant impact on weight of chick.
# They hold a positive , near linear relationship. 

#------------------------------------------------------------------

# Standard theme for all the graphs stored in my_theme 

my_theme <- theme(
  panel.background = element_rect( fill = "ivory2" , color ="gray"),
  legend.key = element_rect(fill = "ivory2", color = "gray"),
  legend.background = element_rect(fill = "white", color = "white", size = 2),
  plot.background = element_rect(fill = "white", color = "black"),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = 12, hjust = 0.5),
  axis.title.y = element_text(size = 12, hjust = 0.5),
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(face = "bold"),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank()) 


# 2a( Description - Toothgrowth )---------------------

?ToothGrowth

# The Toothgrowth data set (60 X 3) consists of 60 observations, response of 
# giving vitamin C to guinea pigs on the length of their odontoblasts (cells 
# responsible for tooth growth). Animals received three different levels of 
# doses(0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or 
# ascorbic acid. The three variables are len(length of tooth), supp(delivery 
# method), dose(mg/day).

# 2b ( Basic histogram of len )---------------------

ggplot(ToothGrowth,aes(x = len, fill = ..count..)) +
  geom_histogram(bins = 17) +
  xlab("Length") + 
  ylab("Frequency") +
  ggtitle(" Frequency of Length figures") +
  scale_fill_gradient(name = "Length",
                      low = "navy",
                      high = "lightpink") +
  my_theme
  


# 2c ( Description for histogram )---------------------

# This is a non symmetric distribution, skewed to the left.
# The median seems to be around 19. 
# The maximum values of length falls within 26-28.  
# There is a steep decline in frequency, once the maximum bar of around 
# 27(length) is crossed.
# There doesn't seem to be the presence of outliers as x-axis do not have 
# a large stretch. 

# 2d ( Extended Histogram, bins = 8 )---------------------

ggplot(ToothGrowth,aes(x = len, fill = ..count..)) +
  geom_histogram(bins = 8) +
  xlab("Length") + 
  ylab("Frequency") +
  ggtitle(" Frequency of a range of Length figures") +
  scale_fill_gradient(name = "Length",
                      low = "navy",
                      high = "lightpink") +
  my_theme

# 2e ( Suitable Bins/ Bin width )---------------------

# A bin value of {9-17} and binwidth of {2.5 - 4} is suitable as it is 
# neither too wide, nor too granular. 

# Choosing a good binwidth because-

# 1. A large bin width will probably hide some important details about distribution 
# while a small binwidth creates a lot of noise and hinders in analyzing the 
# trend in data.

# 2. A suitable binwidth makes the distribution more presentable and easier
# to study.  

# 2f ( Extended Bar plot )---------------------

ggplot(ToothGrowth, aes(x = as.factor(dose), fill = as.factor(dose))) + 
  geom_bar() +
  xlab("Levels of dose(mg/day)") +
  ylab("Frequency") +
  ggtitle("Count for levels of dose") +
  scale_fill_manual(name = "Level of dose",
                     labels = c("0.5","1","2"),
                     values = c("yellow","orange","darkorange1")) +
  theme_classic()

# Uniform spread
# All the levels have equal number of observations.

# 2g ( Median length by grouping dose )---------------------

ToothGrowth %>%
  group_by(dose) %>%
  summarize(Median_len = median(len))

# 2h ( Median length by grouping dose and supp)---------------------

ToothGrowth %>%
  group_by(dose,supp) %>%
  summarize(Median_len = median(len))

# 2i ( Mean length by grouping dose )---------------------

ToothGrowth %>% 
  group_by(dose) %>%
  summarize(Mean_len = mean(len))

# 2j ( Comparing g and i )---------------------

# With the increase in level of dose, the mean and median length of
# tooth also increase. 
# For every level of dose, mean values are greater than median values. 
# This shows that the data is probably skewed and indicates the possibility of 
# outliers. 

# 2k ( len vs dose, scatter plot)---------------------

ggplot(ToothGrowth,aes(x = dose, y = len)) +
  geom_point(aes(color = len)) + 
  geom_smooth(method = "lm") +
  xlab("Level of Dose(mg/day)") +
  ylab(" Length of tooth ") + 
  ggtitle("Length vs Dose") +
  my_theme

# Positive direction, dose and len are related. 
# Linear relation.
# len is dependent on dose with moderate strength.
# There might be a slight possibility of outliers. 

# 2l ( Adding geom_jitter() )---------------------

ggplot(ToothGrowth,aes(x = dose, y = len)) +
  geom_point(aes(color = len)) + 
  geom_smooth(method = "lm"  ) +
  geom_jitter(width = 0.129) +
  xlab("Level of Dose(mg/day)") +
  ylab(" Length of tooth ") + 
  ggtitle("Length vs Dose") +
  my_theme
  
#--------------------------------------------------------------------

# 3a ( Description - Storms )---------------------

?storms

# The storms data set (10,010 X 13) contains 10,010 observations that includes
# position and attributes of 198 tropical storms, measured every six hours
# during the lifetime of a storm. It consists of 13 variables including name,
# status(storm classification), wind(knots), pressure(millibars) etc.

# 3b ( Pressure vs Wind )-------------------------

ggplot(storms,aes(x = wind, y = pressure, color = category)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm") +
  xlab("Wind (knots)") +
  ylab("Pressure (millibars)") + 
  ggtitle("Pressure vs Wind") +
  my_theme

# Wind and pressure are related. Negative direction.
# Near Linear distribution.
# Moderate strength between wind and pressure.
# There might be possibility of bi-dimensional outliers. 

# To increase the interpretability- a. geom_jitter , b. Density map

# a. Adding geom_jitter()

ggplot(storms,aes(x = wind, y = pressure, color = category)) +
  geom_point() +
  geom_jitter(width = 3, size = 1,alpha = 0.5) + 
  xlab("Wind (knots)") +
  ylab("Pressure (millibars)") + 
  ggtitle("Wind vs pressure") +
  my_theme

# b. 2d density map

ggplot(storms,aes(x = wind, y = pressure)) +
  geom_bin2d(bins  = 30) + 
  xlab("Wind (knots)") +
  ylab("Pressure (millibars)") + 
  ggtitle("Wind vs pressure") +
  my_theme


# 3c (Adding linear trend line)------------------------

ggplot(storms,aes(x = wind, y = pressure)) +
  geom_point() +
  geom_jitter(width = 4,size = 1, alpha = 0.4) + 
  geom_smooth(method = "lm") +
  xlab("Wind (knots)") +
  ylab("Pressure (millibars)") + 
  ggtitle("Wind vs pressure") +
  theme_light()

# 3d (Density map between longitude and latitude)----------------------

ggplot(storms,aes(x = long, y = lat)) +
  geom_bin2d(bins = 36) + 
  xlab("Longitude") +
  ylab("Latitude") + 
  ggtitle("Location of storm center") +
  my_theme

# Correlation of wind with latitude and longitude (Graphical)

ggplot(storms,aes(x = long, y = lat, col = wind)) +
  geom_point() + 
  xlab("Longitude") +
  ylab("Latitude") + 
  ggtitle("Location in relation with wind") +
  scale_color_gradient( low = "lightpink",
                        high = "black") +
  my_theme

# Correlation (Using Pearson method)

cor.test(storms$long, storms$wind, method = 'pearson')

# Here value of Pearson correlation coefficient is 0.0047 which tells
# that longitude and wind does not hold a strong correlation.

cor.test(storms$lat, storms$wind, method = 'pearson')

# Here value of Pearson correlation coefficient is 0.076 which tells 
# that latitude and wind does not hold a strong correlation. 

# 3e ( Box plot ( Wind vs Category ))-----------------------------------

ggplot(storms,aes(x = category, y = wind, fill = category)) + 
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 3,
               color = "white",
               fill ="white") +
  xlab("Category") +
  ylab("Wind speed(knots)") + 
  ggtitle("Wind vs Category") +
  my_theme

# Average wind for each category shows a linear increase 
# in speed with each category(following -1 -> 5). 
# Presence of outliers in category -1.
# For all the categories mean lies close to median.
# Category 0 seems to have the largest spread and 
# category -1 is with least spread. 

# 3f ( Bar plot for median wind speed for the top 10 storms)----------

top_10_storms <- storms %>% 
  group_by(name = as.factor(name)) %>% 
  summarize(Median_wind = median(wind)) %>%
  arrange(desc(Median_wind)) %>%
  slice(1:10) 

top_10_storms$name  <- with(top_10_storms,reorder(name, -(Median_wind)))

top_10_storms %>%
  ggplot(aes(x = name, y = Median_wind)) +
  geom_bar(aes(fill = name), stat = "identity") +
  labs(x = "Storm name",
       y = "Median speed of Wind(knots)",
       title = "Average wind speed for top 10 storms") +
  my_theme

# 3g ( Grouping data in other ways)---------------------

# 1. Bi-variate bar plot of wind vs status(storm classification).

status_wind <- storms %>% 
  group_by(status) %>%
  summarize(Median_wind = median(wind))

status_wind$status  <- with(status_wind,reorder(status, -(Median_wind)))

status_wind %>%
  ggplot(aes(x = status, y = Median_wind)) +
  geom_bar(aes(fill = status), stat = "identity") +
  xlab("Storm Classification") +
  ylab("Median speed of Wind(knots)") +
  ggtitle("Wind speed vs Status") +
  my_theme
  
#2. Ordered Bi-variate bar plot of Frequency of storms(count) vs month.

month_count <- storms %>% 
  group_by(month) %>%
  summarize(count = n())

month_count$month  <- with(month_count,reorder(month, -(count)))

month_count %>%
  ggplot(aes(x = month, y = count)) +
  geom_bar(aes(fill = month), stat = "identity") +
  xlab("Month") +
  ylab("Count of storms") +
  ggtitle("Frequency of storms vs month") +
  my_theme

# From this bar plot we can observe that maximum storms occurred in the 
# 9th month (September) 
# There is no information about month of February & March, which means that
# either there are no occurrences or the data is missing.


# ****************** Thank you ******************
