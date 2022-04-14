###GLMs PART 1###

pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)
###read in data###

VACS=read.csv("Completed_VACS_Data_2019-2020_1a.csv")

###lubridate for duration time to be recognized as numeric and not character--> time = seconds###
library(lubridate)
VACS$Duration2 = hms(VACS$Duration) 
VACS$Duration3 = as.numeric(VACS$Duration2)

###GAMMA GLM###

#ADDITIVE MODEL#

#Hypothesis: The duration times at a carcass is independently affected by the specific season and trophic level for this Appalachian mammal community.

set.seed(101)
VACS$Duration3 = rgamma(length(VACS$Duration3),shape=1,rate=1)
g1 = glm(Duration3~Season+Trophic.Level,data=VACS, family="Gamma")
summary(g1)

library(effects)
plot(allEffects(g1))

pp1 <- with (VACS,expand.grid(Season=unique(Season),Trophic.Level=unique(Trophic.Level))) #creates new data frame
pp1$Duration3 <- predict(g1,newdata=pp1,type= "response")  #predict amount of duration times in new data frame

#ggplot with overlay of raw data
library(ggplot2)
ggplot(pp1,aes(x=Season,y=Duration3,colour=Trophic.Level))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Trophic.Level))+
  geom_point(data=VACS,aes(x=Season,y=Duration3,colour = Trophic.Level)) #add the observed data to the plot




