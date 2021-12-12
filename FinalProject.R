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

#remove columns that have more than 30% NA values
d = d[,!sapply(d, function(x) mean(is.na(x)))>0.3]

d <- d %>% 
  select(-c(activity_year, lei, derived_msa.md, state_code, county_code,
            census_tract))

# Now clean your data and conduct your analysis with the d dataset.

# Get rid of Exempt and Missing values, replace with NA
d <- d %>% replace_with_na_all(condition = ~.x == "Exempt")
d <- d %>% replace_with_na_all(condition = ~.x == "")

# to doubles
d$loan_amount <- as.double(d$loan_amount)
d$loan_to_value_ratio <- as.double(d$loan_to_value_ratio)
d$interest_rate <- as.double(d$interest_rate)
d$rate_spread <- as.double(d$rate_spread)
d$total_loan_costs <- as.double(d$total_loan_costs)
d$origination_charges <- as.double(d$origination_charges)
d$discount_points <- as.double(d$discount_points)
d$lender_credits <- as.double(d$lender_credits)
d$property_value <- as.double(d$property_value)
d$income <- as.double(d$income)
d$tract_population <- as.double(d$tract_population)
d$tract_minority_population_percent <- as.double(d$tract_minority_population_percent)
d$ffiec_msa_md_median_family_income <- as.double(d$ffiec_msa_md_median_family_income)
d$tract_to_msa_income_percentage <- as.double(d$tract_to_msa_income_percentage)
d$tract_owner_occupied_units <- as.double(d$tract_owner_occupied_units)
d$tract_one_to_four_family_homes <- as.double(d$tract_one_to_four_family_homes)
d$tract_median_age_of_housing_units <- as.double(d$tract_median_age_of_housing_units)

# replace with means
d$loan_amount[is.na(d$loan_amount)]<-mean(d$loan_amount,na.rm=TRUE)
d$loan_to_value_ratio[is.na(d$loan_to_value_ratio)]<-mean(d$loan_to_value_ratio,na.rm=TRUE)
d$interest_rate[is.na(d$interest_rate)]<-mean(d$interest_rate,na.rm=TRUE)
d$rate_spread[is.na(d$rate_spread)]<-mean(d$rate_spread,na.rm=TRUE)
d$total_loan_costs[is.na(d$total_loan_costs)]<-mean(d$total_loan_costs,na.rm=TRUE)
d$origination_charges[is.na(d$origination_charges)]<-mean(d$origination_charges,na.rm=TRUE)
d$discount_points[is.na(d$discount_points)]<-mean(d$discount_points,na.rm=TRUE)
d$lender_credits[is.na(d$lender_credits)]<-mean(d$lender_credits,na.rm=TRUE)
d$property_value[is.na(d$property_value)]<-mean(d$property_value,na.rm=TRUE)
d$income[is.na(d$income)]<-mean(d$income,na.rm=TRUE)
d$tract_population[is.na(d$tract_population)]<-mean(d$tract_population,na.rm=TRUE)
d$tract_minority_population_percent[is.na(d$tract_minority_population_percent)]<-mean(d$tract_minority_population_percent,na.rm=TRUE)
d$ffiec_msa_md_median_family_income[is.na(d$ffiec_msa_md_median_family_income)]<-mean(d$ffiec_msa_md_median_family_income,na.rm=TRUE)
d$tract_to_msa_income_percentage[is.na(d$tract_to_msa_income_percentage)]<-mean(d$tract_to_msa_income_percentage,na.rm=TRUE)
d$tract_owner_occupied_units[is.na(d$tract_owner_occupied_units)]<-mean(d$tract_owner_occupied_units,na.rm=TRUE)
d$tract_one_to_four_family_homes[is.na(d$tract_one_to_four_family_homes)]<-mean(d$tract_one_to_four_family_homes,na.rm=TRUE)
d$tract_median_age_of_housing_units[is.na(d$tract_median_age_of_housing_units)]<-mean(d$tract_median_age_of_housing_units,na.rm=TRUE)

# Citation: https://www.codingprof.com/how-to-replace-nas-with-the-mode-most-frequent-value-in-r/
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

d <- d %>% mutate(conforming_loan_limit = if_else(is.na(conforming_loan_limit), 
                         calc_mode(conforming_loan_limit), 
                         conforming_loan_limit))
d <- d %>% mutate(derived_loan_product_type = if_else(is.na(derived_loan_product_type), 
                                                  calc_mode(derived_loan_product_type), 
                                                  derived_loan_product_type))
d <- d %>% mutate(derived_dwelling_category = if_else(is.na(derived_dwelling_category), 
                                                      calc_mode(derived_dwelling_category), 
                                                      derived_dwelling_category))
d <- d %>% mutate(derived_ethnicity = if_else(is.na(derived_ethnicity), 
                                                      calc_mode(derived_ethnicity), 
                                                      derived_ethnicity))
d <- d %>% mutate(derived_race = if_else(is.na(derived_race), 
                                              calc_mode(derived_race), 
                                              derived_race))
d <- d %>% mutate(derived_sex = if_else(is.na(derived_sex), 
                                         calc_mode(derived_sex), 
                                          derived_sex))
d <- d %>% mutate(purchaser_type = if_else(is.na(purchaser_type), 
                                        calc_mode(purchaser_type), 
                                        purchaser_type))
d <- d %>% mutate(preapproval = if_else(is.na(preapproval), 
                                           calc_mode(preapproval), 
                                          preapproval))
d <- d %>% mutate(preapproval = if_else(is.na(preapproval), 
                                        calc_mode(preapproval), 
                                        preapproval))
d <- d %>% mutate(loan_type = if_else(is.na(loan_type), 
                                        calc_mode(loan_type), 
                                      loan_type))
d <- d %>% mutate(loan_purpose = if_else(is.na(loan_purpose), 
                                      calc_mode(loan_purpose), 
                                      loan_purpose))
d <- d %>% mutate(lien_status = if_else(is.na(lien_status), 
                                         calc_mode(lien_status), 
                                        lien_status))
d <- d %>% mutate(reverse_mortgage = if_else(is.na(reverse_mortgage), 
                                        calc_mode(reverse_mortgage), 
                                        reverse_mortgage))
d <- d %>% mutate(open.end_line_of_credit = if_else(is.na(open.end_line_of_credit), 
                                             calc_mode(open.end_line_of_credit), 
                                             open.end_line_of_credit))
d <- d %>% mutate(business_or_commercial_purpose = if_else(is.na(business_or_commercial_purpose), 
                                                    calc_mode(business_or_commercial_purpose), 
                                                    business_or_commercial_purpose))
d <- d %>% mutate(hoepa_status = if_else(is.na(hoepa_status), 
                                        calc_mode(hoepa_status), 
                                         hoepa_status))
d <- d %>% mutate(loan_term = if_else(is.na(loan_term), 
                                         calc_mode(loan_term), 
                                      loan_term))
d <- d %>% mutate(negative_amortization = if_else(is.na(negative_amortization), 
                                      calc_mode(negative_amortization), 
                                      negative_amortization))
d <- d %>% mutate(interest_only_payment = if_else(is.na(interest_only_payment), 
                                                  calc_mode(interest_only_payment), 
                                                  interest_only_payment))
d <- d %>% mutate(balloon_payment = if_else(is.na(balloon_payment), 
                                                  calc_mode(balloon_payment), 
                                                  balloon_payment))
d <- d %>% mutate(other_nonamortizing_features = if_else(is.na(other_nonamortizing_features), 
                                            calc_mode(other_nonamortizing_features), 
                                            other_nonamortizing_features))
d <- d %>% mutate(construction_method = if_else(is.na(construction_method), 
                                                         calc_mode(construction_method), 
                                                construction_method))
d <- d %>% mutate(occupancy_type = if_else(is.na(occupancy_type), 
                                                calc_mode(occupancy_type), 
                                           occupancy_type))
d <- d %>% mutate(manufactured_home_secured_property_type = if_else(is.na(manufactured_home_secured_property_type), 
                                           calc_mode(manufactured_home_secured_property_type), 
                                           manufactured_home_secured_property_type))
d <- d %>% mutate(manufactured_home_land_property_interest = if_else(is.na(manufactured_home_land_property_interest), 
                                                                     calc_mode(manufactured_home_land_property_interest), 
                                                                     manufactured_home_land_property_interest))
d <- d %>% mutate(total_units = if_else(is.na(total_units), 
                                        calc_mode(total_units), 
                                        total_units))
d <- d %>% mutate(manufactured_home_land_property_interest = if_else(is.na(manufactured_home_land_property_interest), 
                                                                     calc_mode(manufactured_home_land_property_interest), 
                                                                     manufactured_home_land_property_interest))
d <- d %>% mutate(debt_to_income_ratio = if_else(is.na(debt_to_income_ratio), 
                                        calc_mode(debt_to_income_ratio), 
                                        debt_to_income_ratio))
d <- d %>% mutate(applicant_credit_score_type = if_else(is.na(applicant_credit_score_type), 
                                        calc_mode(applicant_credit_score_type), 
                                        applicant_credit_score_type))
d <- d %>% mutate(co.applicant_credit_score_type = if_else(is.na(co.applicant_credit_score_type), 
                                                        calc_mode(co.applicant_credit_score_type), 
                                                        co.applicant_credit_score_type))
d <- d %>% mutate(applicant_ethnicity.1 = if_else(is.na(applicant_ethnicity.1), 
                                                           calc_mode(applicant_ethnicity.1), 
                                                            applicant_ethnicity.1))
d <- d %>% mutate(co.applicant_ethnicity.1 = if_else(is.na(co.applicant_ethnicity.1), 
                                                  calc_mode(co.applicant_ethnicity.1), 
                                                  co.applicant_ethnicity.1))
d <- d %>% mutate(co.applicant_ethnicity.1 = if_else(is.na(co.applicant_ethnicity.1), 
                                                     calc_mode(co.applicant_ethnicity.1), 
                                                     co.applicant_ethnicity.1))
d <- d %>% mutate(applicant_ethnicity_observed = if_else(is.na(applicant_ethnicity_observed), 
                                                     calc_mode(applicant_ethnicity_observed), 
                                                     applicant_ethnicity_observed))
d <- d %>% mutate(co.applicant_ethnicity_observed = if_else(is.na(co.applicant_ethnicity_observed), 
                                                         calc_mode(co.applicant_ethnicity_observed), 
                                                         co.applicant_ethnicity_observed))
d <- d %>% mutate(applicant_race.1 = if_else(is.na(applicant_race.1), 
                                              calc_mode(applicant_race.1), 
                                             applicant_race.1))
d <- d %>% mutate(co.applicant_race.1 = if_else(is.na(co.applicant_race.1), 
                                             calc_mode(co.applicant_race.1), 
                                             co.applicant_race.1))
d <- d %>% mutate(applicant_race_observed = if_else(is.na(applicant_race_observed), 
                                                calc_mode(applicant_race_observed), 
                                                applicant_race_observed))
d <- d %>% mutate(co.applicant_race_observed = if_else(is.na(co.applicant_race_observed), 
                                                    calc_mode(co.applicant_race_observed), 
                                                    co.applicant_race_observed))
d <- d %>% mutate(applicant_sex = if_else(is.na(applicant_sex), 
                                          calc_mode(applicant_sex), 
                                          applicant_sex))
d <- d %>% mutate(co.applicant_sex = if_else(is.na(co.applicant_sex), 
                                          calc_mode(co.applicant_sex), 
                                          co.applicant_sex))
d <- d %>% mutate(applicant_sex_observed = if_else(is.na(applicant_sex_observed), 
                                             calc_mode(applicant_sex_observed), 
                                             applicant_sex_observed))
d <- d %>% mutate(co.applicant_sex_observed = if_else(is.na(co.applicant_sex_observed), 
                                                   calc_mode(co.applicant_sex_observed), 
                                                   co.applicant_sex_observed))
d <- d %>% mutate(applicant_age = if_else(is.na(applicant_age), 
                                          calc_mode(applicant_age), 
                                          applicant_age))
d <- d %>% mutate(co.applicant_age = if_else(is.na(co.applicant_age), 
                                          calc_mode(co.applicant_age), 
                                          co.applicant_age))
d <- d %>% mutate(applicant_age_above_62 = if_else(is.na(applicant_age_above_62), 
                                             calc_mode(applicant_age_above_62), 
                                             applicant_age_above_62))
d <- d %>% mutate(submission_of_application = if_else(is.na(submission_of_application), 
                                                   calc_mode(submission_of_application), 
                                                   submission_of_application))
d <- d %>% mutate(initially_payable_to_institution = if_else(is.na(initially_payable_to_institution), 
                                                      calc_mode(initially_payable_to_institution), 
                                                      initially_payable_to_institution))
d <- d %>% mutate(aus.1 = if_else(is.na(aus.1), 
                                  calc_mode(aus.1), 
                                  aus.1))
d <- d %>% mutate(denial_reason.1 = if_else(is.na(denial_reason.1), 
                                  calc_mode(denial_reason.1), 
                                  denial_reason.1))

# confirm no more NA values
colSums(is.na(d))