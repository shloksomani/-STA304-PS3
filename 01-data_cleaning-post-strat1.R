#### Preamble ####
# Purpose: Prepare and clean the census data to estimate the results of presidential elections 
# Author: Shlok Somani and James Bai
# Data: 2 November 2020
# Contact: shlok.somani@mail.utoronto.ca, james.bai@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. Path will be diffrent according to your workspace
setwd("C:/Users/shreeji/Desktop/P3")
raw_data <- read_dta("usa_00001.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(
    sex, 
    age, 
    race,
    inctot
  )

reduced_data <- reduced_data %>%
  mutate(race_ethnicity = case_when(
    race == "chinese" ~ "Chinease",
    race == "japanese" ~ "Japanese",
    race == "other asian or pacific islander" ~ "Other Asian or Pacific Islander",
    race == "white" ~ "White",
    race == "black/african american/negro" ~ "Black, or African American",
    race == "american indian or alaska native" ~ "American Indian or Alaska Native",
    race == "two major races" ~ "Some other race",
    race == "three or more major races" ~ "Some other race",
    race == "other race, nec"~ "Some other race"
  ))

reduced_data <- reduced_data %>%
  mutate(household_income = case_when(
    inctot <= 49999 ~ "Less than $49,999",
    inctot >= 50000 & inctot <= 99999 ~"$50,000 to $99,999",
    inctot >=100000 & inctot <= 149999 ~ "$100,000 to $149,999",
    inctot >=150000 & inctot <= 199999 ~ "$150,000 to $199,999",
    inctot >=200000 & inctot <= 249999 ~ "$200,000 to $249,999",
    inctot >=250000 ~ "$250,000 and above",
    TRUE ~ "Respondent Skipped"))%>%
  filter(household_income != "Respondent Skipped")


reduced_data$age <- as.integer(reduced_data$age)

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



reduced_data <- reduced_data %>%
  mutate(gender = case_when(
    sex == "female"~"Female",
    sex == "male"~"Male"
  ))

reduced_data <- reduced_data %>%
  count(gender,
        race_ethnicity,
        household_income,
        ageCategory) %>%
  group_by(gender,
           race_ethnicity,
           household_income,
           ageCategory) 


# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



