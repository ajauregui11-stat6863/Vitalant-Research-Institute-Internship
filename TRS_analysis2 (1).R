library(readr) #for read.csv()
library(plyr) #for rename()
library(ggplot2) #for ggplot()
library(likert) #for likert()
library(ggthemes) #for theme()
library(gmodels) #for CrossTable()
library(Amelia) #for missmap function()
library(pedometrics) #for stepvif()
library(data.table) #for rbindlist()
library(car) #for recode()

Copy_of_TRS_Data_excluding_New_AUG2016 <- read.csv("C:/Users/Adam/Documents/BSRI/Copy of TRS Data (excluding New) AUG2016.csv",na.strings="")
#Copy_of_TRS_Data_excluding_New_AUG2016 <- read_csv("~/Copy of TRS Data (excluding New) AUG2016.csv")
View(Copy_of_TRS_Data_excluding_New_AUG2016)
trs.data<-Copy_of_TRS_Data_excluding_New_AUG2016

#new dataframe that keeps those who agree to survey
trs.data<-subset(trs.data,trs.data$AGREE_TO_SURVEY=="Agree")

#new dataframe with just repeats and non-donors
nonsreps<-subset(trs.data,trs.data$DONOR_TYPE=="REPEAT" | 
                   trs.data$DONOR_TYPE=="NON")

#just question three and four
q3<-nonsreps[c(61:78)]
q4<-nonsreps[c(79:97)]
#q6<-nonsreps[c(109)]

#change the questions to ordered factors
q3 <- lapply(q3, factor, 
             levels=c("Strongly disagree","Disagree","Don't Know",
                      "Agree","Strongly agree"),ordered=T)
q4 <- lapply(q4, factor, 
             levels=c("Strongly disagree","Disagree","Don't Know",
                      "Agree","Strongly agree"),ordered=T)
#q6<-lapply(q6,factor, levels=c("Very unlikely","Somewhat unlikely",  "Don't know/Refuse to answer", "Somewhat likely","Very likely"), ordered=T)

#remove dots and rename some columns
q3<-as.data.frame(q3) #to remove dots and to perform likert
names(q3)<-gsub(x=names(q3),pattern = "\\.",
                replacement=" ")
names(q3)[3]<-"03. South Africa needs blood (for e.g. car accidents, childbirth, etc.)"
names(q3)[5]<-"05. Blood donation helps the community ('Ubuntu')"
names(q3)[7]<-"07. I donated blood so that I or my family won't have to pay for blood in the future (blood credits)"
names(q3)[10]<-"10. I donated blood to get loyalty incentives, gifts, or money"
names(q3)[11]<-"11. Friends, relatives, or coworkers asked me to donate blood"
names(q3)[14]<-"14. I donated blood in response to adverts/campaigns on the radio, TV, or newspapers from i.e. SANBS"
names(q3)[1]<-"01. Blood donation helps to save lives"
names(q3)[2]<-"02. I donated blood to receive money"
names(q3)[4]<-"04. I donated to get my blood test results"
names(q3)[6]<-"06. If I give blood then blood will be available when I need it"
names(q3)[8]<-"08. Blood donation is an easy way to make a difference"
names(q3)[9]<-"09. I donated blood to get school credit or time off from work"
names(q3)[12]<-"12. Blood donation is good for my health"
names(q3)[13]<-"13. My friends or relatives need blood"
names(q3)[15]<-"15. Helping others is the right thing to do"
names(q3)[16]<-"16. I gave blood because there was a blood drive at my school or workplace"
names(q3)[17]<-"17. My blood type is in high demand"
names(q3)[18]<-"18. Giving blood made me feel good about myself"

q4<-as.data.frame(q4)
names(q4)<-gsub(x=names(q4),pattern="\\.",replacement=" ")
names(q4)[1]<-"01. I don't like to do something for free"
names(q4)[3]<-"03. I was scared of the needle, pain, or discomfort"
names(q4)[6]<-"06. I didn't know there was a need for blood every day"
names(q4)[9]<-"09. I don't know where blood goes after donation"
names(q4)[10]<-"10. It takes too long to donate/the queues are too long"
names(q4)[15]<-"15. I don't know where the nearest blood collection point is"
names(q4)[16]<-"16. I don't like to complete the blood donor questionnaire"
names(q4)[18]<-"18. I wasn't treated well by the SANBS staff"
names(q4)[2]<-"02. I had a bad reaction when I gave blood"
names(q4)[4]<-"04. Blood donation is against my religion"
names(q4)[5]<-"05. I was afraid of catching HIV from donating blood"
names(q4)[7]<-"07. I was afraid of finding out about my HIV status"
names(q4)[8]<-"08. Blood collection times are not convenient"
names(q4)[11]<-"11. SANBS might throw away my blood because of my population group"
names(q4)[12]<-"12. Taking blood weakens the body or the spirit"
names(q4)[13]<-"13. I am afraid of the sight of blood"
names(q4)[14]<-"14. Blood donation goes against my culture"
names(q4)[17]<-"17. I think blood mostly goes to people who have money"
names(q4)[19]<-"19. I heard others had a bad reaction after donation"

#q6<-as.data.frame(q6)
#names(q6)[1]<-"How likely are you to donate blood at SANBS within the next year?"

#counts (optional)
summary(q3)
summary(q4)
#summary(q6)

#percent responses for each answer grouped by donor type
q3.lik<-likert(q3,grouping = nonsreps$DONOR_TYPE)
#summary(q3)
q4.lik<-likert(q4,grouping=nonsreps$DONOR_TYPE)
#summary(q4)
#q3.lik<-likert(q3[1:9],grouping = nonsreps$DONOR_TYPE)
#summary(q3.lik)
#q4.lik<-likert(q4[1:9],grouping=nonsreps$DONOR_TYPE)
#summary(q4.lik)
#q3.lik2<-likert(q3[10:18],grouping = nonsreps$DONOR_TYPE)
#summary(q3.lik2)
#q4.lik2<-likert(q4[10:19],grouping=nonsreps$DONOR_TYPE)
#summary(q4.lik2)
#q6<-likert(q6,grouping=nonsreps$DONOR_TYPE)
#summary(q6)

### barplots ###

#titles
title1<-"Motivators"
title2<-"Barriers"
#title3<-"Forecast"

#plots
q3.plot<-likert.bar.plot(q3.lik,
                         include.center=F,
                         plot.percents=T,
                         text.size=3,
                         wrap=500)+
  theme(legend.title = element_blank(),
        legend.position="bottom",
        text=element_text(size=rel(4)),
        legend.text=element_text(size=10),
        plot.title = element_text(hjust = 0.5,size=15))+
  scale_fill_manual(values=c("Strongly disagree"="brown3",
                             "Disagree"="lightsalmon",
                             "Agree"="olivedrab3",
                             "Strongly agree"="palegreen4"),
                    breaks=c('Strongly disagree', 'Disagree',
                             'Agree', 'Strongly agree'))
  #+ggtitle(title1)
q3.plot

q4.plot<-likert.bar.plot(q4.lik,
                         include.center=F,
                         plot.percents=T,
                         text.size=2,
                         wrap=500)+
  theme(legend.title = element_blank(),
        legend.position="bottom",
        text=element_text(size=rel(4)),
        legend.text=element_text(size=10),
        plot.title = element_text(hjust = 0.5,size=15))+
  scale_fill_manual(values=c("Strongly disagree"="brown3",
                             "Disagree"="lightsalmon",
                             "Agree"="olivedrab3",
                             "Strongly agree"="palegreen4"),
                    breaks=c('Strongly disagree', 'Disagree',
                             'Agree', 'Strongly agree'))
  #+ggtitle(title2)
q4.plot

###chi-square analysis###

#collapse responses to be a binomial outcome
chi.q3<-cbind(nonsreps$DONOR_TYPE,q3)
colnames(chi.q3)[1]<-"Donor_Type"
chi.q3[chi.q3=="Strongly agree"]<-"Agree"
chi.q3[chi.q3=="Strongly disagree"]<-"Disagree"
chi.q3[chi.q3=="Don't Know"]<-NA

chi.q4<-cbind(nonsreps$DONOR_TYPE,q4)
colnames(chi.q4)[1]<-"Donor_Type"
chi.q4[chi.q4=="Strongly agree"]<-"Agree"
chi.q4[chi.q4=="Strongly disagree"]<-"Disagree"
chi.q4[chi.q4=="Don't Know"]<-NA

#apply CrossTable to each question in data frame
lapply(nonsreps.logist[2:7],CrossTable,nonsreps.logist$DONOR_TYPE,expected=T)
lapply(chi.q3[2:19],CrossTable,chi.q3$Donor_Type,expected=T)
lapply(chi.q4[2:20],CrossTable,chi.q4$DONOR_TYPE,expected=T)

###logistic regression###

#re-run so that the columns have the original names
#new dataframe with just repeats and non-donors
nonsreps<-subset(trs.data,trs.data$DONOR_TYPE=="REPEAT" | 
                   trs.data$DONOR_TYPE=="NON")

#keep columns of interest
nonsreps.logist<-nonsreps[c(3,7,9,23,25,26,31,61:97)]
nonsreps.logist[nonsreps.logist=="Strongly agree"]<-"Agree"
nonsreps.logist[nonsreps.logist=="Strongly disagree"]<-"Disagree"

#recoding variables as factors
nonsreps.logist<-lapply(nonsreps.logist,factor)
nonsreps.logist<-data.frame(nonsreps.logist)

#collapse some factor levels
levels(nonsreps.logist$AGE_GRP)<-list("18 - 20"="18 - 20 years",
                                      "21 - 24"=c("21 - 22 years", 
                                                  "23 - 24 years",
                                                  "23-24 years"),
                                      "25 - 30"=c("25-26 years",
                                                  "25 - 26 years",
                                                  "27-28 years",
                                                  "27 - 28 years",
                                                  "29-30 years",
                                                  "29 - 30 years"),
                                      "31 - 35"=c("31-32 years",
                                                  "31 - 32 years",
                                                  "33-35 years",
                                                  "33 - 35 years",
                                                  "33 -35 years"))
nonsreps.logist$EDUCATION[nonsreps.logist$EDUCATION=="Refused"]<-NA
levels(nonsreps.logist$EDUCATION)<-list("College graduate/Post-grad"=
                                          c("Degree","Post Graduate level"),
                                        "Diploma/further education"=
                                          "Diploma/further education",
                                        "Secondary or lower"=
                                          c("Secondary complete",
                                            "Secondary incomplete",
                                            "Primary complete",
                                            "Primary not complete"))
levels(nonsreps.logist$MARITAL_STATUS)<-list("Married"=
                                               c("Married - Traditional",
                                                 "Live together",
                                                 "Married - Civil",
                                                 "Polygamous marriage"),
                                             "Single"=
                                               c("Divorced", "Separated",
                                                 "Single parent",
                                                 "Widowed","Never Married"))
levels(nonsreps.logist$INCOME_PERSO)<-list("Don't Know"=
                                             c("Don't know","Refused"),
                                           "No Income"="No income",
                                           "R1 - 5,000"=
                                             c("R1-R2000","R2001-R5000"),
                                           "R5,001 - 10,000"="R5001-R10000",
                                           ">R10,001"=
                                             c("R10001-R20000",
                                               "R20001-R30000",
                                               "R30001- R40000",
                                               "R40001- R50000",
                                               "More than R50 001"))
nonsreps.logist$Q1L[is.na(nonsreps.logist$Q1L)]<-"Don't know"
levels(nonsreps.logist$Q1L)<-list("Don't Know"=c("Don't know","Refused","Pensioner"),
                                  "Formal employment"="Formal employment",
                                  "Informal/Part-time/Self-employed"=
                                    c("Informal / part  time employment",
                                      "Self employed"),
                                  "Unemployed"=c("Homemaker/housewife",
                                                 "Out of school, and Unemployed"),
                                  "Student/Scholar"="Student/scholar")

#find the NAs
sapply(nonsreps.logist, function(x) sum(is.na(x))) #get the sum of NAs per column
which(is.na(nonsreps.logist$SEX==""))
which(is.na(nonsreps.logist$EDUCATION==""))

#for the table
lapply(nonsreps.logist[c(1:1000),c(2:7)],table) #nons
lapply(nonsreps.logist[c(1001:2000),c(2:7)],table) #nons

###motivations###
motives<-nonsreps.logist[1:25]
missmap(motives)
motives[8:25][is.na(motives[8:25])]<-"Don't Know"

#specify reference variables
motives <- within(motives, 
                  EDUCATION <- relevel(EDUCATION, ref = "Secondary or lower"))
motives<-within(motives,
                INCOME_PERSO<-relevel(INCOME_PERSO,ref="No Income"))
motives<-within(motives,
                Q1L<-relevel(Q1L,ref="Unemployed"))
motives<-within(motives,
                MARITAL_STATUS<-relevel(MARITAL_STATUS,ref="Single"))
motives<-within(motives,
                DONOR_TYPE<-relevel(DONOR_TYPE,ref="NON"))
motives<-within(motives,Q3_1<-relevel(Q3_1,ref="Disagree"))
motives<-within(motives,Q3_2<-relevel(Q3_2,ref="Disagree"))
motives<-within(motives,Q3_3<-relevel(Q3_3,ref="Disagree"))
motives<-within(motives,Q3_4<-relevel(Q3_4,ref="Disagree"))
motives<-within(motives,Q3_5<-relevel(Q3_5,ref="Disagree"))
motives<-within(motives,Q3_6<-relevel(Q3_6,ref="Disagree"))
motives<-within(motives,Q3_7<-relevel(Q3_7,ref="Disagree"))
motives<-within(motives,Q3_8<-relevel(Q3_8,ref="Disagree"))
motives<-within(motives,Q3_9<-relevel(Q3_9,ref="Disagree"))
motives<-within(motives,Q3_10<-relevel(Q3_10,ref="Disagree"))
motives<-within(motives,Q3_11<-relevel(Q3_11,ref="Disagree"))
motives<-within(motives,Q3_12<-relevel(Q3_12,ref="Disagree"))
motives<-within(motives,Q3_13<-relevel(Q3_13,ref="Disagree"))
motives<-within(motives,Q3_14<-relevel(Q3_14,ref="Disagree"))
motives<-within(motives,Q3_15<-relevel(Q3_15,ref="Disagree"))
motives<-within(motives,Q3_16<-relevel(Q3_16,ref="Disagree"))
motives<-within(motives,Q3_17<-relevel(Q3_17,ref="Disagree"))
motives<-within(motives,Q3_18<-relevel(Q3_18,ref="Disagree"))
lapply(motives,contrasts) #doublecheck references

#counts for each question
lapply(motives[8:25],motives$DONOR_TYPE,FUN=table) #get the response counts for each question by donor type

#odds ratios for unadjusted motivational-model
N<-18
q3s<-motives[8:25]
m0.glm<-lapply(1:N,function(x) glm(motives$DONOR_TYPE~q3s[,x],
                                   family=binomial))
summ.m0 <- lapply(m0.glm, summary)
coef.m0<-sapply(m0.glm,coef)
conf.m0<-sapply(m0.glm,confint)
ORs.m0<-t(exp(coef.m0))
CIs.m0<-t(exp(conf.m0))
ORsCIs.m0<-cbind(ORs.m0,CIs.m0)
m0.ors<-ORsCIs.m0[,c(1,3,5,2,4,6)]
colnames(m0.ors)<-c("(Intercept)","2.5%","97.5%","Coeff. ORs","2.5%","97.5%")
rownames(m0.ors)<-c("Q3_1"="Blood donation helps to save lives",
                    "Q3_2"="I donated blood to receive money",
                    "Q3_3"="South Africa needs blood (e.g. car accidents, childbirth, etc.)",
                    "Q3_4"="I donated to get my blood test results",
                    "Q3_5"="Blood donation helps the community",
                    "Q3_6"="If I give blood then blood will be available when I need it",
                    "Q3_7"="I donated blood so that I or my family won't have to pay for blood in the future",
                    "Q3_8"="Blood donation is an easy way to make a difference",
                    "Q3_9"="I donated blood to get school credit or time off from work",
                    "Q3_10"="I donated blood to get loyalty incentives, gifts, or money",
                    "Q3_11"="Friends, relatives, or coworkers asked me to donate blood",
                    "Q3_12"="Blood donation is good for my health",
                    "Q3_13"="My friends or relatives need blood",
                    "Q3_14"="I donated in response to adverts or campaigns on the radio, TV, or newspapers",
                    "Q3_15"="Helping others is the right thing to do",
                    "Q3_16"="I gave blood because there was a blood drive at my school or workplace",
                    "Q3_17"="My blood type is in high demand",
                    "Q3_18"="Giving blood made me feel good about myself")
m0.ors #motivation odds ratios

#check for correlation between predictor variables
mcorr.mod<-glm(DONOR_TYPE~Q3_1+Q3_2+Q3_3+Q3_4+Q3_5+Q3_6+Q3_7+Q3_8+Q3_9+Q3_10+
                 Q3_11+Q3_12+Q3_13+Q3_14+Q3_15+Q3_16+Q3_17+Q3_18+
                 AGE_GRP+SEX+EDUCATION+MARITAL_STATUS+INCOME_PERSO+Q1L,
               family=binomial,
               data=motives)
stepVIF(mcorr.mod)
#no correlations found

#logistic regression and odds ratios for all demographic factors and the likert question one by one
N<-18
q3s<-motives[8:25]
mfull.glm<-lapply(1:N,function(x) glm(DONOR_TYPE~q3s[,x]+
                                        AGE_GRP+SEX+EDUCATION+
                                        MARITAL_STATUS+INCOME_PERSO+Q1L,
                                      family=binomial,
                                      data=motives))
summ.mfull <- lapply(mfull.glm, summary)
coef.mfull<-sapply(mfull.glm,coef)
conf.mfull<-sapply(mfull.glm,confint)
ORs.mfull<-t(exp(coef.mfull))
CIs.mfull<-t(exp(conf.mfull))
ORsCIs.mfull<-cbind(ORs.mfull,CIs.mfull)
mfull.ors<-ORsCIs.mfull[,c(1,18,35,2,19,36,3,20,37,
                           4,21,38,5,22,39,6,23,40,
                           7,24,41,8,25,42,9,26,43,
                           10,27,44,11,28,45,12,29,46,
                           13,30,47,14,31,48,15,32,49,
                           16,33,50,17,34,51)]
rownames(mfull.ors)<-c("Q3_1"="Blood donation helps to save lives",
                       "Q3_2"="I donated blood to receive money",
                       "Q3_3"="South Africa needs blood (e.g. car accidents, childbirth, etc.)",
                       "Q3_4"="I donated to get my blood test results",
                       "Q3_5"="Blood donation helps the community",
                       "Q3_6"="If I give blood then blood will be available when I need it",
                       "Q3_7"="I donated blood so that I or my family won't have to pay for blood in the future",
                       "Q3_8"="Blood donation is an easy way to make a difference",
                       "Q3_9"="I donated blood to get school credit or time off from work",
                       "Q3_10"="I donated blood to get loyalty incentives, gifts, or money",
                       "Q3_11"="Friends, relatives, or coworkers asked me to donate blood",
                       "Q3_12"="Blood donation is good for my health",
                       "Q3_13"="My friends or relatives need blood",
                       "Q3_14"="I donated in response to adverts or campaigns on the radio, TV, or newspapers",
                       "Q3_15"="Helping others is the right thing to do",
                       "Q3_16"="I gave blood because there was a blood drive at my school or workplace",
                       "Q3_17"="My blood type is in high demand",
                       "Q3_18"="Giving blood made me feel good about myself")
mfull.ors #motivation odds ratios

#all at once
motives<-na.omit(motives)
mallatonce.mod<-glm(DONOR_TYPE~Q3_1+Q3_2+Q3_3+Q3_4+Q3_5+Q3_6+Q3_7+Q3_8+Q3_9+Q3_10+
                     Q3_11+Q3_12+Q3_13+Q3_14+Q3_15+Q3_16+Q3_17+Q3_18+
                     AGE_GRP+SEX+EDUCATION+MARITAL_STATUS+INCOME_PERSO+Q1L,
                   family=binomial,
                   data=motives)

#using stepwise regression to give us the lowest AIC model
mbackwards<-step(mallatonce.mod)
summary(mbackwards)
exp(cbind(ORs=coef(mbackwards),confint(mbackwards))) #odds ratios w/ CIs

#plot of odds ratios#
#just the statements
tmp<-data.frame(cbind(exp(coef(mbackwards)), exp(confint(mbackwards))))
odds<-tmp[-c(1,3,5,7,9,11,13,15,17,19,21,23,25:39),]
names(odds)<-c('OR', 'lower', 'upper')
row.names(odds)<-c("Q3_1Agree"=". Blood donation helps to save lives",
                  "Q3_2Agree"="*** I donated blood to receive money",
                  "Q3_4Agree"="*** I donated to get my blood test results",
                  "Q3_6Agree"="*** If I give blood then it will be available when I need it",
                  "Q3_7Agree"="I/My family won't have to pay for blood in the future",
                  "Q3_9Agree"="To get school credit or time off from work",
                  "Q3_11Agree"="*** Friends, relatives, or coworkers asked me to donate",
                  "Q3_12Agree"="*** Blood donation is good for my health",
                  "Q3_13Agree"="*** My friends or relatives need blood",
                  "Q3_16Agree"="*** Donated because of a blood drive at my school/workplace",
                  "Q3_17Agree"="*** My blood type is in high demand",
                  "Q3_18Agree"="** Giving blood made me feel good about myself")
odds$vars<-row.names(odds)          
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  theme_bw() + 
  theme(axis.text.y = element_text(size=15))

###deterrents##
deters<-nonsreps.logist[c(1:7,26:44)]
deters[8:26][is.na(deters[8:26])]<-"Don't Know"
lapply(deters[8:26],DONOR_TYPE,FUN=table) #get the response counts for each question by donor type

#specify reference
deters <- within(deters, 
                 EDUCATION <- relevel(EDUCATION, ref = "Secondary or lower"))
deters<-within(deters,
               INCOME_PERSO<-relevel(INCOME_PERSO,ref="No Income"))
deters<-within(deters,
               Q1L<-relevel(Q1L,ref="Unemployed"))
deters<-within(deters,
               MARITAL_STATUS<-relevel(MARITAL_STATUS,ref="Single"))
deters<-within(deters,
               DONOR_TYPE<-relevel(DONOR_TYPE,ref="REPEAT"))
deters<-within(deters,Q4_1<-relevel(Q4_1,ref="Disagree"))
deters<-within(deters,Q4_2<-relevel(Q4_2,ref="Disagree"))
deters<-within(deters,Q4_3<-relevel(Q4_3,ref="Disagree"))
deters<-within(deters,Q4_4<-relevel(Q4_4,ref="Disagree"))
deters<-within(deters,Q4_5<-relevel(Q4_5,ref="Disagree"))
deters<-within(deters,Q4_6<-relevel(Q4_6,ref="Disagree"))
deters<-within(deters,Q4_7<-relevel(Q4_7,ref="Disagree"))
deters<-within(deters,Q4_8<-relevel(Q4_8,ref="Disagree"))
deters<-within(deters,Q4_9<-relevel(Q4_9,ref="Disagree"))
deters<-within(deters,Q4_10<-relevel(Q4_10,ref="Disagree"))
deters<-within(deters,Q4_11<-relevel(Q4_11,ref="Disagree"))
deters<-within(deters,Q4_12<-relevel(Q4_12,ref="Disagree"))
deters<-within(deters,Q4_13<-relevel(Q4_13,ref="Disagree"))
deters<-within(deters,Q4_14<-relevel(Q4_14,ref="Disagree"))
deters<-within(deters,Q4_15<-relevel(Q4_15,ref="Disagree"))
deters<-within(deters,Q4_16<-relevel(Q4_16,ref="Disagree"))
deters<-within(deters,Q4_17<-relevel(Q4_17,ref="Disagree"))
deters<-within(deters,Q4_18<-relevel(Q4_18,ref="Disagree"))
deters<-within(deters,Q4_19<-relevel(Q4_19,ref="Disagree"))
lapply(deters,contrasts)

#odds ratios for unadjusted deterrent model
N<-19
q4s<-deters[8:26]
d0.glm<-lapply(1:N,function(x) glm(deters$DONOR_TYPE~q4s[,x],
                                   family=binomial))
summ.d0 <- lapply(d0.glm, summary)
coef.d0<-sapply(d0.glm,coef)
conf.d0<-sapply(d0.glm,confint)
ORs.d0<-t(exp(coef.d0))
CIs.d0<-t(exp(conf.d0))
ORsCIs.d0<-cbind(ORs.d0,CIs.d0)
d0.ors<-ORsCIs.d0[,c(1,3,5,2,4,6)]
colnames(d0.ors)<-c("(Intercept)","2.5%","97.5%","Coeff. ODDS","2.5%","97.5%")
rownames(d0.ors)<-c("Q4_1"="I don't like to do something for free",
                    "Q4_2"="I had a bad reaction when I gave blood",
                    "Q4_3"="I was scared of the needle, pain, or discomfort",
                    "Q4_4"="Blood donation is against my religion",
                    "Q4_5"="I was afraid of catching HIV from donating blood",
                    "Q4_6"="I didn't know there was a need for blood every day",
                    "Q4_7"="I was afraid of finding out about my HIV status",
                    "Q4_8"="Blood collection times are not convenient",
                    "Q4_9"="I do not know where blood goes after donation",
                    "Q4_10"="It takes too long to donate",
                    "Q4_11"="SANBS might throw away my blood because of my population group",
                    "Q4_12"="Taking blood weakens the body or the spirit",
                    "Q4_13"="I am afraid of the sight of blood",
                    "Q4_14"="Blood donation goes against my culture",
                    "Q4_15"="I don't know where the nearest blood collection point is",
                    "Q4_16"="I don't like to complete the blood donor questionnaire",
                    "Q4_17"="I think blood mostly goes to people who have money",
                    "Q4_18"="I wasn't treated well by the SANBS staff",
                    "Q4_19"="I heard others had a bad reaction after donation")
d0.ors #deterrent odds ratios

#check for correlation between predictor variables
dcorr.mod<-glm(DONOR_TYPE~Q4_1+Q4_2+Q4_3+Q4_4+Q4_5+Q4_6+Q4_7+Q4_8+Q4_9+Q4_10+
                 Q4_11+Q4_12+Q4_13+Q4_14+Q4_15+Q4_16+Q4_17+Q4_18+Q4_19+
                 AGE_GRP+SEX+EDUCATION+MARITAL_STATUS+INCOME_PERSO,
               family=binomial,
               data=deters)
stepVIF(dcorr.mod)
#no correlations

#logistic regression for deterrents full model
N<-19
q4s<-deters[8:26]
dfull.glm<-lapply(1:N,function(x) glm(DONOR_TYPE~q4s[,x]+
                                        AGE_GRP+SEX+EDUCATION+
                                        MARITAL_STATUS+INCOME_PERSO+Q1L,
                                      family=binomial,
                                      data=deters))
summ.dfull <- lapply(dfull.glm, summary)
coef.dfull<-sapply(dfull.glm,coef)
conf.dfull<-sapply(dfull.glm,confint)
ORs.dfull<-t(exp(coef.dfull))
CIs.dfull<-t(exp(conf.dfull))
ORsCIs.dfull<-cbind(ORs.dfull,CIs.dfull)
dfull.ors<-ORsCIs.dfull[,c(1,18,35,2,19,36,3,20,37,
                           4,21,38,5,22,39,6,23,40,
                           7,24,41,8,25,42,9,26,43,
                           10,27,44,11,28,45,12,29,46,
                           13,30,47,14,31,48,15,32,49,
                           16,33,50,17,34,51)]
rownames(dfull.ors)<-c("Q4_1"="I don't like to do something for free",
                       "Q4_2"="I had a bad reaction when I gave blood",
                       "Q4_3"="I was scared of the needle, pain, or discomfort",
                       "Q4_4"="Blood donation is against my religion",
                       "Q4_5"="I was afraid of catching HIV from donating blood",
                       "Q4_6"="I didn't know there was a need for blood every day",
                       "Q4_7"="I was afraid of finding out about my HIV status",
                       "Q4_8"="Blood collection times are not convenient",
                       "Q4_9"="I do not know where blood goes after donation",
                       "Q4_10"="It takes too long to donate",
                       "Q4_11"="SANBS might throw away my blood because of my population group",
                       "Q4_12"="Taking blood weakens the body or the spirit",
                       "Q4_13"="I am afraid of the sight of blood",
                       "Q4_14"="Blood donation goes against my culture",
                       "Q4_15"="I don't know where the nearest blood collection point is",
                       "Q4_16"="I don't like to complete the blood donor questionnaire",
                       "Q4_17"="I think blood mostly goes to people who have money",
                       "Q4_18"="I wasn't treated well by the SANBS staff",
                       "Q4_19"="I heard others had a bad reaction after donation")
dfull.ors #deterrent odds ratios

#all at once
deters<-na.omit(deters)
dallatonce.mod<-glm(DONOR_TYPE~Q4_1+Q4_2+Q4_3+Q4_4+Q4_5+Q4_6+Q4_7+Q4_8+Q4_9+Q4_10+
                      Q4_11+Q4_12+Q4_13+Q4_14+Q4_15+Q4_16+Q4_17+Q4_18+Q4_19+
                      AGE_GRP+SEX+EDUCATION+MARITAL_STATUS+INCOME_PERSO+Q1L,
                    family=binomial,
                    data=deters)
summary(dallatonce.mod)

#using step to reduce model
dbackwards<-step(dallatonce.mod)
summary(dbackwards)

#plot of odds ratios
tmp<-data.frame(cbind(exp(coef(dbackwards)), exp(confint(dbackwards))))
odds<-tmp[-c(1,3,5,7,9,11,13,15,17,19,21,23,25:36),]
names(odds)<-c('OR', 'lower', 'upper')
row.names(odds)<-c("Q4_1Agree"="*** I don't like to do something for free",
                   "Q4_2Agree"="* I had a bad reaction when I gave blood",
                   "Q4_7Agree"="** I was afraid of finding out about my HIV status",
                   "Q4_8Agree"="*** Blood collection times are not convenient",
                   "Q4_10Agree"="It takes too long to donate",
                   "Q4_12Agree"=". Taking blood weakens the body or the spirit",
                   "Q4_13Agree"="*** I am afraid of the sight of blood",
                   "Q4_14Agree"="** Blood donation goes against my culture",
                   "Q4_15Agree"="** I don't know where the nearest blood collection point is",
                   "Q4_16Agree"="*** I don't like to complete the blood donor questionnaire",
                   "Q4_17Agree"="I think blood mostly goes to people who have money",
                   "Q4_18Agree"=". I wasn't treated well by the SANBS staff")
odds$vars<-row.names(odds)          
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  theme_bw() + 
  theme(axis.text.y = element_text(size=15))

###demographics###
m.tmp<-data.frame(cbind(exp(coef(mbackwards)), exp(confint(mbackwards))))
m.odds<-m.tmp[-c(1:25),]
names(m.odds)<-c('OR', 'lower', 'upper')
row.names(m.odds)<-c("AGE_GRP21 - 24"=". Age (21 - 24)",
                   "AGE_GRP25 - 30"="** Age (25 - 30)",
                   "AGE_GRP31 - 35"="** Age (31 - 35)",
                   "SEXMale"="*** Male",
                   "EDUCATIONCollege graduate/Post-grad"="* College grad/Post-grad",
                   "EDUCATIONDiploma/further education"="*** Diploma/further education",
                   "INCOME_PERSODon't Know"="*** P. Income (Unknown)",
                   "INCOME_PERSO1 - 5,000"="P. Income (1 - 5,000)",
                   "INCOME_PERSO5,001 - 10,000"="*** P. Income (5,001 - 10,000)",
                   "INCOME_PERSO>R10,001"="*** P. Income (>10,000)",
                   "Q1LDon't Know"="*** Employment Unknown",
                   "Q1LFormal employment"="Formal employment",
                   "Q1LInformal/Part-time/Self-employed"="* Informal/Part-time/Self-employed",
                   "Q1LStudent/Scholar"="Student/Scholar")
m.odds$vars<-row.names(m.odds)          
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(m.odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  theme_bw() + 
  theme(axis.text.y = element_text(size=15),
        axis.text.x=element_text(size=15))

d.tmp<-data.frame(cbind(exp(coef(dbackwards)), exp(confint(dbackwards))))
d.odds<-d.tmp[-c(1:25),]
names(d.odds)<-c('OR', 'lower', 'upper')
row.names(d.odds)<-c("SEXMale"="*** Male",
                   "EDUCATIONCollege graduate/Post-grad"="* College grad/Post-grad",
                   "EDUCATIONDiploma/further education"="** Diploma/further education",
                   "INCOME_PERSODon't Know"="** P. Income (Unknown)",
                   "INCOME_PERSO1 - 5,000"="P. Income (1 - 5,000)",
                   "INCOME_PERSO5,001 - 10,000"="*** P. Income (5,001 - 10,000)",
                   "INCOME_PERSO>R10,001"="*** P. Income (>10,000)",
                   "Q1LDon't Know"="* Employment Unknown",
                   "Q1LFormal employment"="Formal employment",
                   "Q1LInformal/Part-time/Self-employed"=". Informal/Part-time/Self-employed",
                   "Q1LStudent/Scholar"="Student/Scholar")
d.odds$vars<-row.names(d.odds)          
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(d.odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size=15))


