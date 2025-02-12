#Data Analysis

#load packages
library(tidyverse)

#grab the data for our analysis
sample_data <-read_csv("data/sample_data.csv")
glimpse(sample_data)

#summarize
summarize(sample_data, avg_cells=mean(cells_per_ml))

#syntax/style
sample_data %>%
  #group data by environmental group
  group_by(env_group)%>%
  #calculate mean
  summarize(avg_cells=mean(cells_per_ml))

#filter: subset data by rows based on some value
sample_data %>%
  #subset samples only from the deep
  #subset based on a logical, TRUE==1
  filter(env_group =="Deep") %>%
  #calculate mean cell abundances
  summarize(avg_cells = mean(cells_per_ml))


sample_data %>%
  #subset samples only from the deep
  #subset based on a logical, TRUE==1
  filter(temperature<5) %>%
  #calculate mean cell abundances
  summarize(avg_cells = mean(cells_per_ml))

#mutate: create new column 
sample_data %>%
  #calculate new column with the TN:TP ratio
  mutate(tn_tp_ratio=total_nitrogen/total_phosphorus) %>%
  #visualize it
  view()

#select():subset by entire columns
sample_data %>%
  #pick specific columns
  select(sample_id, depth)

#select():subset by entire columns
sample_data %>%
  #pick specific columns in between sample id and temperature
  select(sample_id:depth)

#select():subset by entire columns
sample_data %>%
  #pick all columns except for
  select(-diss_org_carbon)

#select():subset by entire columns
sample_data %>%
  #pick all columns except for
  select(-c(diss_org_carbon,chlorophyll))

#clean up data
