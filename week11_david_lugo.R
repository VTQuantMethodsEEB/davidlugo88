###Week 11 Assgn###

###read in data###

VACS=read.csv("Completed_VACS_Data_2019-2020_1a.csv")

###lubridate for duration time to be recognized as numeric and not character--> time = seconds###
library(lubridate)
VACS$Duration2 = hms(VACS$Duration) 
VACS$Duration3 = as.numeric(VACS$Duration2)

###GAMMA GLMs###

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

#INTERACTIVE MODEL#

#Hypothesis: The duration time at a carcass is predicted by the direct interaction between season and specific trophic levels.

set.seed(101)
VACS$Duration3 = rgamma(length(VACS$Duration3),shape=1,rate=1)
g2 = glm(Duration3~Season*Trophic.Level,data=VACS, family="Gamma")
summary(g2)

pp2 <- with (VACS,expand.grid(Season=unique(Season),Trophic.Level=unique(Trophic.Level))) #creates new data frame
pp2$Duration3 <- predict(g2,newdata=pp2,Type = "response")  #predict amount of duration times in new data frame

library(ggplot2)
ggplot(pp2,aes(x=Season,y=Duration3,colour=Trophic.Level))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Trophic.Level))+
  geom_point(data=VACS,aes(x=Season,y=Duration3,colour = Trophic.Level)) #add the observed data to the plot

#SINGLE-VARIABLE GLM#

#Hypothesis: Duration times at a carcass site by a species is predicted by its trophic level designation.

set.seed(101)
VACS$Duration3 = rgamma(length(VACS$Duration3),shape=1,rate=1)
g3 = glm(Duration3~Trophic.Level,data=VACS, family="Gamma")
summary(g3)

mod_1 <- glm(Duration3~Trophic.Level, data = VACS)  # linear model
plot(mod_1) #plot of fitted values; residuals vs fitted

hist(resid(mod_1)) #histogram of plotted values

shapiro.test(resid(mod_1)) #shapiro wilks test
#p=2.2e-06 is normal
hist(VACS$Duration3)

library(ggplot2)
ggplot(data=VACS,aes(x=Trophic.Level,y=Duration3,colour=Trophic.Level))+
  geom_point()+
  geom_line(aes(group=Trophic.Level))

#Hypothesis: Duration times at a carcass site by a species is predicted by the current season.

set.seed(101)
VACS$Duration3 = rgamma(length(VACS$Duration3),shape=1,rate=1)
g4 = glm(Duration3~Season,data=VACS, family="Gamma")
summary(g4)

mod_2 <- glm(Duration3~Season, data = VACS)  # linear model
plot(mod_2) #plot of fitted values; residuals vs fitted

hist(resid(mod_2)) #histogram of plotted values

shapiro.test(resid(mod_2)) #shapiro wilks test
#p=2.2e-06 is normal
hist(VACS$Duration3)

library(ggplot2)
ggplot(data=VACS,aes(x=Season,y=Duration3,colour=Season))+
  geom_point()+
  geom_line(aes(group=Season))

####LIKELIHOOD TESTS###

#AIC Method - Information Critereon

#GLMs to be in AIC table
h1 = glm(Duration3~Season+Trophic.Level,data = VACS, family = Gamma)
h2 = glm(Duration3~Season*Trophic.Level,data = VACS, family = Gamma)
h3 = glm(Duration3~Trophic.Level,data = VACS, family = Gamma)
h4 = glm(Duration3~Season,data=VACS, family=Gamma)

#Simple AIC output
AIC(h1,h2,h3,h4)

#Tabular AIC Output
library(AICcmodavg)
aictab(cand.set=list(h1,h2,h3,h4),modnames=c("h1","h2","h3","h4"))#AIC table
aictab(cand.set=list(h1,h2,h3,h4),modnames=c("h1","h2","h3","h4"), second.ord = F)#AIC table

~FIN~




