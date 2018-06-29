library(gmodels) #for CrossTable function

###Run Chi-square analysis. For all chi-square tests, the response variable  
###is true/false elites ("N", "Y").

#by gender
#We hypothesize that the proportion of true/false elites by the subjects'
#gender is the same.
CrossTable(best.hivdata$Gender,best.hivdata$art_y.n,expected = T)
#chi-square p-value = .1621
#Conclude that there is not enough evidence to change our hypothesis.

#by race
#We hypothesize that the proportion of true/false elites by the subjects'
#race is the same.
best.hivdata$race[best.hivdata$race=="Unknown"]<-NA #change unknown races to NA
best.hivdata.180<-subset(best.hivdata,!is.na(best.hivdata$race)) 
  #make new dataframe with the NA's in 'race' ommitted
best.hivdata.180$race<-factor(best.hivdata.180$race)
levels(best.hivdata.180$race)<-list(Black="Black",
                                     Other.Race=c("Asian","Coloured","White"))
#collapsed Asian, Coloured, and White because sample size is too small
CrossTable(best.hivdata.180$race,best.hivdata.180$art_y.n,expected=T,
           fisher = TRUE)
#chi-square p-value = .2525 but expected counts too small so inappropriate
#chi-square w/ Yates' continuity correction p-value = .4232
#Fisher's exact test p-value = .307 
#Conclude that there is not enough evidence to change our hypothesis.

#by fixed/mobile
#We hypothesize that the proportion of true/false elites by the location of
#the clinic (fixed/mobile) is the same.
CrossTable(best.hivdata$`fixed/mobile`,best.hivdata$art_y.n,expected=T)
#chi-square p-value = .1715
#Conclude that there is not enough evidence to change our hypothesis.

#by age
#We hypothesize that the subjects' age group is unassociated with them 
#being a true/false elite.
CrossTable(best.hivdata$age_group,best.hivdata$art_y.n,expected = T)
#chi-square p-value = .3609
#Conclude that there is not enough evidence to change our hypothesis.

#by zone
#We hypothesize that the zone the clinics are in is unassociated with the 
#subjects being a true/false elite.
CrossTable(best.hivdata$Zone,best.hivdata$art_y.n,expected = T)
#chi-square p-value = .2616
#Conclude that there is not enough evidence to change our hypothesis.

#by year (using chi-square test for trend in proportions)
#We hypothesize that as the years go on there is no upward or downward trend
#in the number of false elites.
false.counts<-c(5,8,8,20,24,34,51)
total.counts<-c(13,16,15,34,34,47,67)
prop.trend.test(false.counts,total.counts,score=seq_along(false.counts))
#p-value = <.001
#SIGNIFICANCE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Conclude that as the years go on there is an upward trend in the
#number of false elites.

#by incentive dates
#We hypothesize that the proportion of true/false elites who donated during 
#an incentive period or not is the same.
CrossTable(best.hivdata$incentive_period,best.hivdata$art_y.n,expected = T)
#p=.2168
#Conclude that there is not enough evidence to change our hypothesis.