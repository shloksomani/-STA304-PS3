#### Preamble ####
# Purpose: Prepare and clean the census data to estimate the results of presidential elections
# Author: Shlok Somani and James Bai
# Data: November 1, 2020
# Contact: shlok.somani@mail.utoronto.ca, james.bai@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/shreeji/Desktop/P3")
# Read in the raw data
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)

# Cleaning the income column as it had uneven range 
reduced_data <- reduced_data %>%
  mutate(household_income = case_when(
    household_income == "Less than $14,999" ~ "Less than $49,999",
    household_income == "$15,000 to $19,999" ~ "Less than $49,999" ,
    household_income == "$20,000 to $24,999" ~ "Less than $49,999",
    household_income == "$25,000 to $29,999" ~ "Less than $49,999", 
    household_income == "$30,000 to $34,999" ~ "Less than $49,999",
    household_income == "$35,000 to $39,999" ~ "Less than $49,999",
    household_income == "$40,000 to $44,999" ~ "Less than $49,999",
    household_income == "$45,000 to $49,999"~ "Less than $49,999",
    household_income == "$50,000 to $54,999" ~ "$50,000 to $99,999",
    household_income == "$55,000 to $59,999" ~ "$50,000 to $99,999",
    household_income == "$60,000 to $64,999" ~ "$50,000 to $99,999",
    household_income == "$65,000 to $69,999" ~ "$50,000 to $99,999",
    household_income == "$70,000 to $74,999" ~ "$50,000 to $99,999", 
    household_income == "$75,000 to $79,999" ~ "$50,000 to $99,999",
    household_income == "$80,000 to $84,999" ~ "$50,000 to $99,999",
    household_income == "$85,000 to $89,999" ~ "$50,000 to $99,999", 
    household_income == "$90,000 to $94,999" ~ "$50,000 to $99,999",
    household_income == "$95,000 to $99,999"~ "$50,000 to $99,999",
    household_income == "$100,000 to $124,999" ~ "$100,000 to $149,999",
    household_income == "$125,000 to $149,999" ~ "$100,000 to 149,999",
    household_income == "$150,000 to $174,999" ~ "$150,000 to $199,999",
    household_income == "$175,000 to $199,999" ~ "$150,000 to $199,999",
    household_income == "$200,000 to $249,999" ~ "$200,000 to $249,999",
    household_income == "$250,000 and above" ~ "$250,000 and above",
    TRUE ~ "Respondent Skipped"
  ))


## Cleaning the race_ethnicity column of the dataset
reduced_data <- reduced_data %>%
mutate(race_ethnicity = case_when(
  race_ethnicity == "Asian (Chinese)" ~ "Chinease",
  race_ethnicity == "Asian (Japanese)" ~ "Japanese",
  race_ethnicity == "Asian (Asian Indian)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Asian (Korean)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Asian (Vietnamese)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Asian (Other)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "White" ~ "White",
  race_ethnicity == "Black, or African American" ~ "Black, or African American",
  race_ethnicity == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
  race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Pacific Islander (Guamanian)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Pacific Islander (Samoan)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Pacific Islander (Other)" ~ "Other Asian or Pacific Islander",
  race_ethnicity == "Some other race" ~ "Some other race",
  TRUE ~ "Respondent Skipped"))


# Generating Age range and making a new column 
reduced_data <- reduced_data %>%
  mutate(ageCategory = case_when(
    age <= 17 ~ "Not eligible to vote",
    18 <= age & age <= 24 ~ "18 to 24",
    25 <= age & age <= 34 ~ "25 to 34",
    35 <= age & age <= 44 ~ "35 to 44",
    45 <= age & age <= 54 ~ "45 to 54",
    55 <= age & age <= 64 ~ "55 to 64",
    65 <= age & age <= 74 ~ "65 to 74",
    age >= 75  ~ "75 or older",
    TRUE ~ "Respondent Skipped"))



## Assigning binary if someone votes for either trump or biden 
reduced_data<-
  reduced_data %>%
  mutate(voteTrump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
  mutate(voteBiden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))




# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

