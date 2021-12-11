---
  title: "STA 235H - Homework 1" # You can put the title of your document here
author: 
  - Name 1  # You can include names here
- Name 2
- Name 3
- Name 4
date: "September 16th, 2021"
output: 
  pdf_document: 
  latex_engine: xelatex  # More modern PDF typesetting engine
fig_width: 5
fig_height: 4
fig_caption: true # If you want to include captions for your figures (usually you do)
toc: no #Do you want a Table of Contents?
header-includes:
  - \usepackage{float} #This is just a useful LaTeX package for positioning tables and figures
urlcolor: blue #This is so links are highlighted (and more visible)
---
  
#2.1
food_health <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Homework/data/hw1/food_health_politics.csv") 
mortality_rate <- food_health$mortality_rate
hist(mortality_rate)

low_access_to_food <- food_health$pct_low_access_pop
hist(low_access_to_food)

#2.2
food_health_clean <- na.omit(food_health)

#2.3*
ggplot(data = food_health_clean, aes(x = pct_low_access_pop, y = mortality_rate)) + geom_point() 
lm1 = lm(mortality_rate ~ pct_low_access_pop, data = food_health_clean)
summary(lm1)

#2.4
ggplot(data = food_health_clean, aes(x = fastfood_per_1000, y = mortality_rate)) + geom_point() 
ggplot(data = food_health_clean, aes(x = snap_stores_per_1000, y = mortality_rate)) + geom_point() 
ggplot(data = food_health_clean, aes(x = per_dem_2012, y = mortality_rate)) + geom_point() 











