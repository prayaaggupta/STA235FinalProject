################################################################################
### Title: "Prediction Project"
### Course: STA 235H
### Semester: Fall 2021
### Names: Prayaag Gupta, Benjamin Mathew, Shaan Parekh, Benjamin Wang
################################################################################

# Clears memory
rm(list = ls())
# Clears console
cat("\014")

library(tidyverse)

# Before doing anything, load your data and select the sample we will use
setwd("/Users/benmathew/Desktop/Personal/UT_CSB/S3/STA235H/STA235FinalProject")
d_total <- read.csv("TravisCountyData.csv")  %>% 
  select(-c(activity_year, lei, derived_msa.md, state_code, county_code,
            census_tract, prepayment_penalty_term, intro_rate_period,
            total_points_and_fees, multifamily_affordable_units, 
            applicant_ethnicity.2, applicant_ethnicity.3,
            applicant_ethnicity.4, applicant_ethnicity.5,
            co.applicant_ethnicity.2, co.applicant_ethnicity.3,
            co.applicant_ethnicity.4, co.applicant_ethnicity.5, 
            applicant_race.2, applicant_race.3, applicant_race.4,
            applicant_race.5))

# These are the row numbers you will need (everyone will use the same observations)
rows <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Project/data/row_sample.csv") %>%
  pull() # Load it as a vector and not a dataframe.

d <- d_total %>% slice(rows)

# Now clean your data and conduct your analysis with the d dataset.
for(i in 1:ncol(d_total)){
  d_total[is.na(d_total[,i]), i] <- mean(d_total[,i], na.rm = TRUE)
}
warnings()
