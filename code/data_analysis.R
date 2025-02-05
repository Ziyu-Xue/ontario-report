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
