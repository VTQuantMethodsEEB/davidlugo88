#MULTIVARIATE ADDITIVE/INTERACTIVE MODEL#

##utility function for pretty printing
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)
###read in data###

VACS=read.csv("Example_NEW_VACS_Data_1d.csv")

###lubridate for duration time to be recognized as numeric and not character--> time = seconds###
library(lubridate)
VACS$Duration2 = hms(VACS$Duration) 
VACS$Duration3 = as.duration(VACS$Duration2)

###some 'NA' were lower case--> changed all to uppercase"
VACS$Trophic.Level[VACS$Trophic.Level=="na"]=NA

## Additive model##

#Hypothesis: The total duration time at a carcass is effected by both the variables trophic level and detection number(Event #)#

head(VACS)
pr(lmTL1 <- lm(Duration3~Trophic.Level+Event..,data=VACS)) #additive LM,x=Trophic.Level,y=Duration3,grouped by=Event..
#the intercept is Apex predator; the Event #s are the diferences between the amount time for duration times
#Event# gave an intercept difference of -1.213
#Apex predator with the effects of trophic level and Event# had significant affects on all trophic levels;Primary: p=1.3e-09,Secondary: p=2.2e-09,Tertiary: p=0.00053

#plot and predict#
pp <- with(VACS,expand.grid(Trophic.Level=unique(Trophic.Level),Event..=unique(Event..)))#creates new data frame
pp$Duration3 <- predict(lmTL1,newdata=pp) #predict amount of duration times in new data frame

#plot in ggplot2#
library(ggplot2)
ggplot(pp,aes(x=Trophic.Level,y=Duration3,colour=Event..))+
  geom_point()+
  geom_line(aes(group=Event..))

#Add raW data to plot#
ggplot(pp,aes(x=Trophic.Level,y=Duration3,colour=Event..))+ #plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Event..))+
  geom_point(data=VACS,aes(x=Trophic.Level,y=Duration3,colour = Event..)) #add the observed data to the plot

## Interactive model##

#Hypothesis: The duration time on a carcass by specific species is directly effected by their trophic level designation#

head(VACS)
pr(lmTL2 <- lm(Duration3~Common.Name*Trophic.Level,data=VACS)) #interactive LM,x=Common.Name,y=Duration3,grouped by=Trophic.Level
lmTL2 <- lm(Duration3~Common.Name*Trophic.Level,data=VACS)
summary(lmTL2) #intercept was 'American black bear';had significant affect on Coyote,p=0.0139 and Turkey vulture,p=6.61e-06
#the overall affect was not found on the interaction of common name and trophic level, only for Bird spp,eastern cottontail,eastern gray squirrel but not significant

pp <- with(VACS,expand.grid(Common.Name=unique(Common.Name),Trophic.Level=unique(Trophic.Level))) #creates new data frame
pp$Duration3 <- predict(lmTL2,newdata=pp)  #predict amount of duration times in new data frame

#plot in ggplot2
library(ggplot2)
ggplot(pp,aes(x=Common.Name,y=Duration3,colour=Trophic.Level))+
  geom_point()+
  geom_line(aes(group=Trophic.Level))

#Add raw data to plot#
ggplot(pp,aes(x=Common.Name,y=Duration3,colour=Trophic.Level))+ #set up plot using predictions dataset
  geom_point()+ #plot the prediction
  geom_line(aes(group=Trophic.Level))+ #draw lines b/t predictions, group them by light conditions
  geom_point(data=VACS,aes(x=Common.Name,y=Duration3,colour = Trophic.Level))
