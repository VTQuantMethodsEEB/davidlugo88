###Week 12 - Mixed Models###

library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)

#probably a new package
#install.packages("lme4")
library(lme4)

#Call in data

VACS=read.csv("Completed_VACS_Data_2019-2020_1a.csv")

#Lubridate for duration time to be recognized as numeric and not character--> time = seconds###
library(lubridate)
VACS$Duration2 = hms(VACS$Duration) 
VACS$Duration3 = as.numeric(VACS$Duration2)

#Take look at data on ggplot

library(ggplot2); theme_set(theme_bw())
q0 <- (ggplot(VACS, aes(Season, Detection.., colour = Trophic.Level))
       + geom_point())  ## points only, use later
print(q0+geom_line())


#GLM Models & Hypotheses#

library(car)

#Hypothesis 1: Apex predators negatively affect the detection of other trophic levels throughout the seasons of the year.

glm1 = glmer.nb(Detection..~Season + (1|Trophic.Level),data=VACS)

summary(glm1)#season had direct affect on detection throughout the year

#Hypothesis 2: Apex predators negatively affect the duration times of other trophic levels throughout the seasons of the year.

glm2 = glmer.nb(Duration3~Season + (1|Trophic.Level),data=VACS)

summary(glm2)#season had a direct affect on duration times throughout the year

#ANOVAs of GLMs#

Anova(glm1)#season has greater significance on detection (p=2.2e-16)
Anova(glm2)#season also had significant effect on duration, but lower than glm1 (p=0.0116)

#Use Predict on best model = glm1#

newdat1 <- with (VACS,expand.grid(Season=unique(Season),Trophic.Level=unique(Trophic.Level))) #creates new data frame
newdat1$Detection.. <- predict(glm1,newdata=newdat1,type= "response")

#Plot predictions#

library(ggplot2)
ggplot(newdat1,aes(x=Season,y=Detection..,colour=Trophic.Level))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Trophic.Level))+
  ylab("Detection Number")+
  xlab("Season")+
  theme_bw() + 
  theme(axis.title=element_text(size=18),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.78,.18),legend.text = element_text(size=12,face="italic"))
print(r)#add the observed data to the plot
 


                                           
                     