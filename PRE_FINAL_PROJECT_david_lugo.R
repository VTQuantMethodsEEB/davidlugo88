###Final Project###

library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)

#probably a new package
install.packages(lme4)
library(lme4)

library(sjPlot)
library(sjmisc)

install.packages("sjPlot")

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

#Drop Prey Species, only carnivores#

VACS = subset(VACS, Species!="White-tailed deer")
VACS = subset(VACS, Species!="Eastern cottontail")
VACS = subset(VACS, Species!="Eastern gray squirrel")
VACS = subset(VACS, Species!="Bird spp")
VACS = subset(VACS, Species!="Mouse")
VACS = subset(VACS, Species!="Turkey")
VACS = subset(VACS, Species!="Eastern chipmunk")
VACS = subset(VACS, Species!="Turkey vulture")
VACS = subset(VACS, Species!="American crow")
VACS = subset(VACS, Species!="Red-tailed hawk")
VACS = subset(VACS, Species!="Common raven")
VACS = subset(VACS, Species!="Bald eagle")
VACS = subset(VACS, Species!="Golden eagle")
VACS = subset(VACS, Species!="Golden Eagle")
VACS = subset(VACS, Species!="Opossum")
VACS = subset(VACS, Trophic.Level!="Primary consumer")



#GLM Models & Hypotheses#

library(car)

#Hypothesis 1: Season has a direct effect on latency to detection throughout the year.

glm1A = glmer.nb(Detection..~Season + (1|Trophic.Level),data=VACS)
glm2A= glmer.nb(Detection..~Season + (1|Species),data=VACS)
glm3A= glmer.nb(Detection..~Season + (1|Feeding.Mode),data=VACS)
glm4A= glmer.nb(Detection..~Season + (1|Time.of.Day),data=VACS)

#Summaries
summary(glm1A)
summary(glm2A)
summary(glm3A)
summary(glm4A)

#Anovas
Anova(glm1A)
Anova(glm2A)
Anova(glm3A)
Anova(glm4A)

#Multivariate interactive mixed effects model

glmA1 = glmer.nb(Detection..~Season*Time.of.Day + (1|Species),data=VACS)
summary(glmA1)
Anova(glmA1)

#Multivariate mixed effects model

glm2A= glmer.nb(Detection..~Species + (1|Season),data=VACS)
summary(glm2A)
Anova(glm2A)

#Plot glm2a

newdat1 <- with(VACS, expand.grid(Species=unique(Species),Season=unique(Season))) #creates new data frame
newdat1$Detection.. <- predict(glm2A,newdata=newdat1,type= "response")

library(ggplot2)
ggplot(newdat1,aes(x=Species,y=Detection..,colour=Season))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Season))


#Hypothesis 2: Time of day has a direct effect on foraging times of species.

glm1B = glmer(Duration3~Species + (1|Time.of.Day),data=VACS,family="")
glm2B = glmer(Duration3~Time.of.Day + (1|Species),data=VACS,family="Gamma")
glm3B = glmer(Duration3~Time.of.Day + (1|Feeding.Mode),data=VACS,family="Gamma")
glm4B = glmer(Duration3~Time.of.Day + (1|Season),data=VACS,family="Gamma")


exp(-0.7164)

#Summaries
#Summaries
summary(glm1B)
summary(glm2B)
summary(glm3B)
summary(glm4B)

#Anovas
Anova(glm1B)
Anova(glm2B)
Anova(glm3B)
Anova(glm4B)

glmB1 = glmer(Duration3~Time.of.Day (1|Season),data=VACS,family="poisson")









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


