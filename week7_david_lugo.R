###linear models###

#UNIVARIATE LINEAR MODEL#

###Hypothesis###
#The duration time at carcass by a species is determined by their trophic level designation#

rm(list)
###read in data###

VACS=read.csv("Example_NEW_VACS_Data_1d.csv")

###lubridate for duration time to be recognized as numeric and not character--> time = seconds###
library(lubridate)
VACS$Duration2 = hms(VACS$Duration) 
VACS$Duration3 = as.duration(VACS$Duration2)

###some 'NA' were lower case--> changed all to uppercase"
VACS$Trophic.Level[VACS$Trophic.Level=="na"]=NA

###one parameter variables###

###run linear model, y=duration and x=Trophic level###
l1 <- lm(Duration3~Trophic.Level, data = VACS)
lm(Duration~Trophic.Level, data = VACS)
summary(l1) #there was a significant effect of 'apex predator' on the duration ofall levels tertiary p=0.0153, primary=6.63e-13, secondary=1.40e-09

###Diagnostic plots###

mod_1 <- lm(Duration3~Trophic.Level, data = VACS)  # linear model
plot(mod_1) #plot of fitted values; residuals vs fitted

hist(resid(mod_1)) #histogram of plotted values

shapiro.test(resid(mod_1)) #shapiro wilks test
#p=2.2e-06 is normal
hist(VACS$Duration3)

###ggplot###

library(ggplot2)
r=ggplot(data=VACS, aes(x=Trophic.Level, y=Duration3))+ 
  geom_point()+
  stat_summary(color="red")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)

