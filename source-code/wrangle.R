# Mendez, Christopher

rm(list = ls())
library(tidyverse)

## Question 1 ##
###Part A ###
# Read in the Dataset
fastfoodData <- read.csv("datasets/fastfood.csv" , stringsAsFactors = TRUE)

# Overview of all variables from fastfoodData
summary(fastfoodData)

###Part B ###
# Filter to McDonald's only
mcdonaldsData <- filter(fastfoodData, restaurant == "Mcdonalds")

# Summary focused on cal_fat
summary(mcdonaldsData)
# 25th Q1 percentile of cal_fat = 160 calories from fat.
# 75th Q3 percentile of cal_fat = 320 calories from fat.
# This means that the middle 50% of McDonald's items are between 160 and 320 calories from fat.

###Part C ###
# Filter to Dairy Queen only
dairyqueenData <- filter(fastfoodData, restaurant == "Dairy Queen")

# Summary focused on cal_fat
summary(dairyqueenData)
# 25th Q1 percentile of cat_fat = 160 calories from fat.
# 75th Q3 percentile of cal_fat = 310 calories from fat.
# Both restaurants share the same Q1 of 160, Dairy Queen's Q3 is lower at 310 vs McDonald's 320.

###Part D ###
# Combine Dairy Queen AND McDonald's using the OR operators which is symbol:|
# A single item can't belong to both restaurants, so using AND symbol: & will return zero rows.
# Using the OR symbol will keep any row that belong to either restaurant.
dqMcdData <- filter(fastfoodData, restaurant == "Dairy Queen" | restaurant == "Mcdonalds")

###Part E ###
# Side-by-side boxplots for cal_fat - Dairy Queen vs McDonald's
ggplot(dqMcdData, aes(x = restaurant, y = cal_fat)) + geom_boxplot() +
  labs(title = "Calories from Fat by Restaurant", x = "Restaurant",
    y = "Calories from Fat"
  )

###Part F ###
# Single pipeline - summary statistics for cal_fat, grouped by restaurant
# Starting with full Datasets then filter to two restaurants group by restaurant.
fastfoodData %>%
  filter(restaurant == "Dairy Queen" | restaurant == "Mcdonalds") %>% group_by(restaurant) %>%
  summarise(n = n(), min = min(cal_fat, na.rm = TRUE),
    Q1 = quantile(cal_fat, 0.25, na.rm = TRUE), median = median(cal_fat, na.rm = TRUE),
    mean = mean(cal_fat, na.rm = TRUE), Q3 = quantile(cal_fat, 0.75, na.rm = TRUE), max = max(cal_fat, na.rm = TRUE),
    sd = sd(cal_fat, na.rm = TRUE)
  )

###Part G ###
# McDonald's has the more skewed meaning right-skewed distribution based on cal_fat.
# Because it has three elements that are quantitative:
#
# Max value. McDonald's has the highest cat_fat item has 1270 cals from fat.
# There is only 670 cals on Dairy Queens highest. Which is about half of McDonald''s max.
# Those extreme high values pull McDonald's distribution far to the right. This shows up as
# outliers dots from above the box to the boxplot.
# Standard deviation. McDonald's SD is 220.90 which is much more larger than Dairy Queen's 156.49.
# A larger SD means that data is much more spread out. In this case, that spread is driven by high fat
# items that are stretching the upper tail of the distribution, which is right skew.
# Mean and Media. McDonald's mean is 286.61 is which about 45.6 unit above its media of 240. Dairy Queen's
# mean is 260.48. which is 40.5 units above media of 220. When the mean is pulled above the median, that means
# that a few very high values are stretching the upper tail, meaning right skew.



