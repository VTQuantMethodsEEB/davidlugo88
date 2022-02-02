#loaded important packages
library(tidyverse)
library(tidyr)
library(dplyr)

#read in .csv file
VACs=read.csv("VACs_Compiled_Station_Data.csv")


#read in unique column heads to find mispellings/capitilization errrors
head(VACs)
unique(VACs$Species)
unique(VACs$Trophic.Level)
unique(VACs$Station)
unique(VACs$Behavior)


#used group_by and summarised a specific column
VACs %>% 
  group_by(Species) %>%
  summarise(Behavior)

#made a table with grouping species and then summarised the behaviors for each species
species.behavior.table = VACs %>% 
  group_by(Species) %>% 
  summarise(Behavior)

##Used mutate on data 
VACs_with_sample_size = VACs %>% 
  #created a new dataframe  called VACs_with_sample_size
  group_by(Species,Trophic.Level,Date) %>% 
  #grouped by species, trophic level, and date
  mutate(sample.size=n())
#used mutate to add a new column with sample size
VACs_with_sample_size

