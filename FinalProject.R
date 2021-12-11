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
library(naniar)

# Before doing anything, load your data and select the sample we will use
d_total <- read.csv("TravisCountyData.csv")
# These are the row numbers you will need (everyone will use the same observations)
rows <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Project/data/row_sample.csv") %>%
  pull() # Load it as a vector and not a dataframe.

d <- d_total %>% slice(rows)
na_count <-sapply(d, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

d_total <- read.csv("TravisCountyData.csv")  %>% 
  select(-c(activity_year, lei, derived_msa.md, state_code, county_code,
            census_tract, prepayment_penalty_term, intro_rate_period,
            total_points_and_fees, multifamily_affordable_units, 
            applicant_ethnicity.2, applicant_ethnicity.3,
            applicant_ethnicity.4, applicant_ethnicity.5,
            co.applicant_ethnicity.2, co.applicant_ethnicity.3,
            co.applicant_ethnicity.4, co.applicant_ethnicity.5, 
            applicant_race.2, applicant_race.3, applicant_race.4,
            applicant_race.5, co.applicant_race.2, co.applicant_race.3,
            co.applicant_race.4, co.applicant_race.5))

# These are the row numbers you will need (everyone will use the same observations)
rows <- read.csv("https://raw.githubusercontent.com/maibennett/sta235/main/exampleSite/content/Assignments/Project/data/row_sample.csv") %>%
  pull() # Load it as a vector and not a dataframe.

d <- d_total %>% slice(rows)

# Now clean your data and conduct your analysis with the d dataset.
d <- d %>% replace_with_na_all(condition = ~.x == "Exempt")
d$rate_spread <- as.double(d$rate_spread)

# below this line is not working as expected
# df <- as.data.frame(t(d))
# df$rate_spread[is.na(df$rate_spread)]<-mean(df$rate_spread,na.rm=TRUE)
d[rate_spread[is.na(d[rate_spread])]<-mean(d[rate_spread],na.rm=TRUE)
d$rate_spread

d$rate_spread <- as.double(d$rate_spread)
d$rate_spread <- as.double(d$total_loan_costs)
d$rate_spread <- as.double(d$origination_charges)
d$rate_spread <- as.double(d$discount_points)
d$rate_spread <- as.double(d$lender_credits)
d$rate_spread <- as.double(d$income)