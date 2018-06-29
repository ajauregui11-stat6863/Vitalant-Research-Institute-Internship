library(Amelia) #for missmap function
library(pedometrics) #for stepvif

###perform logistic regression

#visual check of missing values
missmap(hiv10to16)

#new dataframe keeping the true/false elite counts and the possible predictors
tf2.hiv10to16<-subset(hiv10to16,!is.na(hiv10to16$art_y.n))
tf2.hiv10to16<-tf2.hiv10to16[c(1,4,5,6,9,11,13,14)]
missmap(tf2.hiv10to16)
View(tf2.hiv10to16)

#recoding variables for usability in regression analysis
#Note 'donation_year' is an ordered factor
tf2.hiv10to16$art_y.n<-factor(tf2.hiv10to16$art_y.n)
tf2.hiv10to16$incentive_period<-factor(tf2.hiv10to16$incentive_period)
tf2.hiv10to16$age_group2<-factor(tf2.hiv10to16$age_group2) 
tf2.hiv10to16$donation_year<-factor(tf2.hiv10to16$donation_year)
tf2.hiv10to16$Gender<-factor(tf2.hiv10to16$Gender)
tf2.hiv10to16$`fixed/mobile`<-factor(tf2.hiv10to16$`fixed/mobile`)
tf2.hiv10to16$Zone<-factor(tf2.hiv10to16$Zone)
#collapse unknown/na values into "other.race"
tf2.hiv10to16$race[is.na(tf2.hiv10to16$race)]<-"Unknown"
tf2.hiv10to16$race<-factor(tf2.hiv10to16$race)
levels(tf2.hiv10to16$race)<-list(Black="Black",
                                 Other.Race=c("Asian","Coloured","White","Unknown"))

#see structure of data frame
str(tf2.hiv10to16)

#set Egoli as reference variable since Egoli is bigger to show negative association with E. Cape and no association with other zones
tf2.hiv10to16 <- within(tf2.hiv10to16, Zone <- relevel(Zone, ref = 2))
tf2.hiv10to16<-within(tf2.hiv10to16,race<-relevel(race,ref=2))

#see what assignment numbers R gave each variable
contrasts(tf2.hiv10to16$art_y.n) #true = 0, false = 1
contrasts(tf2.hiv10to16$incentive_period) #0=0, 1=1
contrasts(tf2.hiv10to16$age_group2) #reference is <20
contrasts(tf2.hiv10to16$donation_year) #reference is 2010
contrasts(tf2.hiv10to16$`fixed/mobile`) #fixed=0, mobile=1
contrasts(tf2.hiv10to16$Gender) #female = 0, male = 1
contrasts(tf2.hiv10to16$Zone) #egoli = 0
contrasts(tf2.hiv10to16$race) #other.race=0, black=1

#summary of two models with no predictors and all seven predictors are included
nothing.hivmod<-glm(art_y.n~1,family = binomial,data=tf2.hiv10to16)
summary(nothing.hivmod)
full.hivmod<-glm(art_y.n~donation_year+age_group2+Zone+`fixed/mobile`+Gender+race+
                   incentive_period,family=binomial,data=tf2.hiv10to16) 
summary(full.hivmod)
#Column Pr(>|z|) shows the probability that the coefficient of that variable term is equal to 0.

#test for correlation between predictors
stepVIF(full.hivmod,threshold=5) #variance inflation factor
#None of the predictor variables are correlated enough to warrant worry.

#Since donation_year showed significance we will start with that
hivmod.1<-glm(art_y.n~donation_year,family=binomial,data = tf2.hiv10to16)
summary(hivmod.1)
hivmod.2<-glm(art_y.n~Zone,family=binomial,data = tf2.hiv10to16)
summary(hivmod.2)
#The AIC's are about the same. Not really getting anwhere

#STEP function for variable selection
forwards=step(nothing.hivmod,scope=list(lower=nothing.hivmod,
                                        upper=full.hivmod),
              direction="forward")
#The model with the lowest AIC uses Gender and Incentive Period, but the difference in AIC is not that big between them and a model with just "Gender" or none of the predictors. I would say none of the predictors are good enough to include in a model.

#Lowest AIC Model
lowest.hivmod<-glm(art_y.n~Gender+incentive_period,family=binomial,
                   data = tf2.hiv10to16)
summary(lowest.hivmod)

#calculate odds ratios of full model
exp(cbind(ODDS=coef(full.hivmod), confint(full.hivmod)))