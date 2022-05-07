###Final Project###

library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)

#probably a new package
install.packages(lme4)
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

#Hypothesis 1: What has a direct effect on latency to detection throughout the year.

#foward stepwise selection
glm1A = glm.nb(Detection..~Trophic.Level,data=VACS)
glm2A= glm.nb(Detection..~Species,data=VACS)
glm3A= glm.nb(Detection..~Season,data=VACS)
glm4A= glm.nb(Detection..~Feeding.Mode,data=VACS)
glm5A=glm.nb(Detection..~Time.of.Day,data=VACS)

#Summaries
summary(glm1A)
summary(glm2A)
summary(glm3A)
summary(glm4A)
summary(glm5A)

#Anovas
Anova(glm1A)
Anova(glm2A)
Anova(glm3A)
Anova(glm4A)
Anova(glm5A)

#AIC 
h1 = glm.nb(Detection..~Trophic.Level,data=VACS)
h2 = glm.nb(Detection..~Species,data=VACS)
h3 = glm.nb(Detection..~Season,data=VACS)
h4 = glm.nb(Detection..~Feeding.Mode,data=VACS)
h5 = glm.nb(Detection..~Time.of.Day,data=VACS)

#Tabular AIC Output
library(AICcmodavg)
aictab(cand.set=list(h1,h2,h3,h4,h5),modnames=c("h1","h2","h3","h4","h5"))#AIC table
aictab(cand.set=list(h1,h2,h3,h4,h5),modnames=c("h1","h2","h3","h4","h5"), second.ord = F)#AIC table

#Top model = glm2A
#The simpler the model, the better it fit the data; tried additive/interactive but simple was better

summary(glm2A)

#Plot glmA

library(effects)
plot(allEffects(glm2A))

#New data frame + predict

newdat1 <- with(VACS, expand.grid(Species=unique(Species))) #creates new data frame
newdat1$Detection.. <- predict(glm2A,newdata=newdat1,type= "response")

#GGLPOT
library(ggplot2)
ggplot(VACS,aes(x=Species,y=Detection..,colour=Species))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Species))+
  theme(axis.text.x = element_text(angle = 90))
#Not the best figure

#Hypothesis 2: What has a direct effect on foraging times of species.

#Forward stepwise selection
glm1B = glm.nb(Duration3~Species,data=VACS)
glm2B = glm.nb(Duration3~Trophic.Level,data=VACS)
glm3B = glm.nb(Duration3~Feeding.Mode,data=VACS)
glm4B = glm.nb(Duration3~Time.of.Day,data=VACS)
glm5B = glm.nb(Duration3~Season,data=VACS)

#Anovas
Anova(glm1B)
Anova(glm2B)
Anova(glm3B)
Anova(glm4B)
Anova(glm5B)

#AIC

A1 = glm.nb(Duration3~Species,data=VACS)
A2 = glm.nb(Duration3~Trophic.Level,data=VACS)
A3 = glm.nb(Duration3~Feeding.Mode,data=VACS)
A4 = glm.nb(Duration3~Time.of.Day,data=VACS)
A5 = glm.nb(Duration3~Season,data=VACS)

#Tabular AIC Output
library(AICcmodavg)
aictab(cand.set=list(A1,A2,A3,A4,A5),modnames=c("A1","A2","A3","A4","A5"))#AIC table
aictab(cand.set=list(A1,A2,A3,A4,A5),modnames=c("A1","A2","A3","A4","A5"), second.ord = F)#AIC table

#Top Model = glm1B
#Same thing, the simpler the model the better it fit the data

summary(glm1B)

#Plot glm1B

library(effects)
plot(allEffects(glm1B))

#New data frame + predict
newdat2 <- with(VACS, expand.grid(Species=unique(Species))) #creates new data frame
newdat2$Duration3 <- predict(glm1B,newdata=newdat2,type= "response")

#GGLPOT
library(ggplot2)
ggplot(newdat2,aes(x=Species,y=Duration3,colour=Species))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Species))
#Not a good figure either

