#load in important packages
library(ggplot2)
library(gridExtra)
library(viridis)

#read in data
VACs=read.csv("VACs_Compiled_Station_Data.csv")

head(VACs)

#my first attempt to make a graph within ggplot using the variables: # of individuals by species and station
ggplot(data=VACs, aes(x = Species, y = No..of.Individuals, color=Station))+
  geom_point(size=2)

ggplot(data=VACs, aes(x = Species, y = No..of.Individuals, color=Station))+
  geom_point(size=2)
g1

g1=ggplot(data=VACs,aes(x = Species, y = No..of.Individuals))+
  geom_boxplot() +
  geom_point(aes(color=Date))
g1


#to condense data I did a sumdat and grouped species by station to get a total # of individuals
library(tidyverse)
sum.dat = VACs %>%
  group_by(Species,Station)%>%
  summarise(total.n = sum(No..of.Individuals))

#here I redo my graph and get a better boxplot representing total number of species by station
g1=ggplot(data=sum.dat,aes(x = Species, y = total.n))+
  geom_boxplot() +
  geom_point(aes(color=Station))+  
  theme(axis.text=element_text(size=10),panel.grid = element_blank(),axis.line=element_line(),axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))
g1


