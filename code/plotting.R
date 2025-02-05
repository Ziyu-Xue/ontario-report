#plotting lake Ontario microbial cell abundances
#by Ziyu Xue
#date: Jan 29th 2025

#first install the packages
install.packages("tidyverse")
library(tidyverse)


#load in the data
?read_csv
sample_data <- read_csv(file="sample_data.csv")
#this is like the relative path in terminal

Sys.Date() #what's the data
getwd() #where am I

round(x = 3.1415)
round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415)

sum(2,3)

#what does are data look like?
view(sample_data)
str(sample_data)
#plotting
ggplot(data=sample_data) + 
  #temperature is from dataset
  aes(x=temperature,y=cells_per_ml/1000000,color=env_group, size=chlorophyll) +
  #labels
  labs(x="Temp (C)",y="Cell Abundance (millions/ml)",title="does temp affect microbial abundance?",color="environmental group", size="chlorophyll(ug/L)") +
  geom_point()
 

#buoy data
buoy_data<-read.csv("buoy_data.csv")
#number of rows then columns
dim(buoy_data) 
glimpse(buoy_data)
#give unique inputsof $ parameter
unique(buoy_data$sensor)
length(unique(buoy_data$sensor))

#plot buoy data
ggplot(data=buoy_data)+
  aes(x=day_of_year, y=temperature, group=sensor, color=buoy)+
  geom_line()

#facet plot
ggplot(data=buoy_data)+
  aes(x=day_of_year, y=temperature, group=sensor, color=buoy)+
  geom_line()+
  #make separate plot for each
  facet_wrap(~buoy,scales ="free_y")


ggplot(data=buoy_data)+
  aes(x=day_of_year, y=temperature, group=sensor, color=buoy)+
  geom_line()+
  #make separate plot for each
  facet_grid(rows=vars(buoy))


#cell abundances by envrionmental group
ggplot(data=sample_data)+
  aes(x=env_group, y=cells_per_ml)+
  geom_boxplot()+
  #give better visualization of data size (or geom_point)
  geom_jitter()

#change sequence can change layer of plots

ggplot(data=sample_data)+
  #these aes applies to both geom_jtter and geo_boxplot
  aes(x=env_group, y=cells_per_ml, col=env_group,fill=env_group)+
  geom_jitter(aes(size=chlorophyll))+
  #make it transparent
  #remove outlier because included in jitter
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  theme_bw()

ggsave("cells_per_env_group.png",width=6, height=4)

#assignment plot1
ggplot(data=sample_data)+
  aes(x=total_nitrogen, y=cells_per_ml/1000000,color=env_group, size=temperature)+
  labs(x="Total_nitrogen (mg/L)",y="Cell Abundance (millions/ml)",title="Does Nitrogen Affect Microbial Abundance?",color="environmental group", size="temperature(C))") +
  geom_point() +
  geom_smooth(aes(group = 1),method="lm", color="black", size=1)

ggsave("nitrogenplot.png",width=6, height=4)

#assignment plot2
ggplot(data=sample_data)+
  aes(x=total_phosphorus, y=cells_per_ml/1000000,color=env_group, size=temperature)+
  labs(x="Total_phosphorus (mg/L)",y="Cell Abundance (millions/ml)",title="Does Phosphorus Affect Microbial Abundance?",color="environmental group", size="temperature(C))") +
  geom_point() +
  geom_smooth(aes(group = 1),method="lm", color="black", size=1)

ggsave("phosphorusplot.png",width=6, height=4)
