#cleared space and loaded ggplot
rm(list=ls())
library(ggplot2)

#read in VACS data
VACS=read.csv("Example_NEW_VACS_Data.csv")

###I couldn't figure out how to separate these trophic into their own levels with duration time so manually input it###

#created columns for secondary/tertiary consumers with their duration times and tested the mean diff between secondary and tertiary
###got a negtaive mean difference; meaning the mean duration time of tertiary consumers is higher than secondary consumers
secondary<-c(0:00:01, 0:00:01, 0:00:01, 0:00:01, 0:00:12, 0:00:14, 0:04:20, 0:00:01, 0:04:31, 0:02:50, 0:02:22, 0:03:23, 0:00:17, 0:03:18, 0:03:15,
                      0:04:52, 0:00:01, 0:00:01, 0:06:01, 0:01:34, 0:00:01, 0:00:12, 0:00:01, 0:01:32, 0:01:34, 0:00:01, 0:00:01, 0:00:01, 0:00:13, 0:01:55, 0:05:12,
                      0:00:01, 0:00:01, 0:00:01, 0:00:01, 0:00:01, 0:00:01, 0:01:30, 0:00:01, 0:01:34, 0:01:23, 0:00:01, 0:00:01, 0:00:01, 0:01:20, 0:00:01, 0:00:01, 0:03:01, 0:02:32)
tertiary<-c(0:02:37, 0:00:01, 0:00:01, 0:00:01, 0:22:17, 0:01:38, 0:00:33, 0:00:18, 0:00:01, 0:00:01, 0:00:18, 0:03:46, 0:00:01, 0:00:01, 0:48:13, 0:00:01, 0:00:01, 0:00:15, 0:00:01, 0:00:01, 0:05:23, 0:08:53, 0:01:57, 0:00:01, 0:01:45, 0:15:00, 0:00:01, 0:00:25, 0:11:30, 0:00:01, 0:00:01, 0:17:17, 0:00:01, 0:00:01, 0:26:41, 0:11:09, 0:34:38, 0:57:26, 0:10:27, 0:08:01, 0:03:49, 0:00:01, 0:02:22, 0:29:20, 0:02:44, 0:03:02, 0:02:34, 0:00:01, 0:00:01, 0:00:11, 0:00:01, 0:01:23, 0:29:07, 0:34:07, 0:44:15, 0:01:27, 0:05:24, 0:10:08, 0:08:59, 0:15:27, 0:00:01, 0:03:46, 0:27:58, 0:00:01, 0:09:29, 0:00:01, 0:00:01, 0:07:48, 0:06:56, 0:19:13, 0:09:55, 0:00:01, 0:01:43, 0:11:36, 0:15:32, 0:06:23, 0:15:37, 0:04:49, 0:00:01, 0:02:44, 0:19:39, 0:00:01, 0:00:01, 0:00:01, 0:28:40, 0:00:01, 0:00:01, 0:03:15, 0:02:32, 0:01:59, 0:01:24, 0:02:32)
mean(secondary) - mean(tertiary)

#created data frame (trophic.levels) and separated trophic levels by time
trophic.levels <- data.frame(
  trophic.level=rep(c("secondary","tertiary"),
            c(length(secondary), length(tertiary))),
  time=c(secondary,tertiary)
)
View(trophic.levels)

#loaded ggplot
library(ggplot2)
theme_set(theme_bw())

###Used ggplot to look at data with stat_sum to view overlapping data points 
ggplot(trophic.levels,aes(x = trophic.level,y = time))+
  stat_sum(aes(size=..n..),colour="darkgray")+
  scale_size_area(breaks=1:2,max_size=4)+
  geom_boxplot(fill=NA)

#Used ggplot with jitter
ggplot(trophic.levels,aes(trophic.level,time))+
  geom_point(aes(trophic.level,jitter(time,factor=1.5)),shape=2,size=3)+
  geom_boxplot(fill=NA)

###START OF PERMUTATION TEST###

#set a seed for randome # generator
set.seed(101)

#place for results to be stored
res <- NA

#10000 iterations
for (i in 1:10000) {
  trophic.levelboot <- sample(c(secondary,tertiary)) ## scramble
  ## pick out forest & field samples
  secondaryboot <- trophic.levelboot[1:length(secondary)] #assign the first 680 durations to secondaryboot
  tertiaryboot <- trophic.levelboot[(length(tertiary)+1):length(trophic.levelboot)] #assign the rest of duration to tertiaryboot
  #compute/store difference in means
  res[i] <- mean(secondaryboot)-mean(tertiaryboot) 

#observed mean difference
obs <- mean(secondary)-mean(tertiary)
obs

#histogram for data dsitribution
hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

#how to calc p-val?
res[res>=obs]
length(res[res>=obs]) ##gets 10000 in length for the 10000 iterations making the p-value 1...is my length correct?
10000/10000
mean(res>=obs)        
##My p-value is 1??

###Classical Test###
###T-test###

##Not sure if this is correct either###

#true student t-test since this is two-sample data#
tt <- t.test(time~trophic.level,data=trophic.levels,var.equal=TRUE)
tt
###P-val was almost 0 -> 2.097e-9###

