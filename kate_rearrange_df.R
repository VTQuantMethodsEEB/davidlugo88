library(tidyverse)

##example of rearranging data
VACS=read.csv("Completed_VACS_Data_2019-2020_1a.csv")

head(VACS)

VACS = VACS %>%
  select(!(starts_with("X")))

head(VACS)
str(VACS$No..of.Individuals)

##now isolate species - e.g. effect of black bear on others
VACS.wide = VACS %>%
  pivot_wider(names_from = Species, values_from = "No..of.Individuals")
  
VACS.wide2 = VACS.wide %>%
  group_by(Station, Date) %>%
  mutate(No.Black.Bear = sum(`American black bear`))
