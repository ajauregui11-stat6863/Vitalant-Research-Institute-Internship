library(readr) #use read.csv function
library(dplyr) #activate %>% function
library(lubridate) #use the date functions

#NOTE: R had trouble reading the "donation_date" variable, at least when 
#the file was saved as a CSV. So within the CSV file, I created a new 
#"donation_date" column that was an exact copy of the original 
#"donation_date" column, except that I formatted the cells as a "Date" 
#category, then choosing the type as "*3/14/2001." The new "donation_date"
#column is named within R as "donation_date_1."

###import data
HIV_Data_2010_2016_updated_05Sept_17_1 <- read_csv(
  "~/CSUEB Biostatistics/HIV Data 2010 - 2016 updated 05Sept 17 1.csv", 
  col_types = cols(donation_date = col_date(format = "%m/%d/%Y")))
#changed variables "donation_date" from the class
#"character" to the class "date"
View(HIV_Data_2010_2016_updated_05Sept_17_1)

#keeping the columns from data frame that will be utilized
hiv05to16<-HIV_Data_2010_2016_updated_05Sept_17_1[
  c(3,4,10,14,15,21,34,38)] 
View(hiv05to16)

#select rows with date at or after 01/01/10
attach(hiv05to16)
hiv10to16<-hiv05to16 %>%
  select(donation_date,Zone,Gender,age,race,Code,
         `fixed/mobile`,`ART Y N`) %>%
  filter(donation_date >= as.Date("2010-01-01")) 
View(hiv10to16)
detach(hiv05to16)

###create new variables: 'donation_year', 'age_group', and 
###'incentive_period'
#donation_year
hiv10to16$donation_year<-
  format(as.Date(hiv10to16$donation_date, 
                 format="%Y-%m-%d"),"%Y")
#age_group
hiv10to16$age_group<-cut(hiv10to16$age,
                         breaks=c(-Inf,20,31,41,51,Inf),
                         labels=c("<21","21-30","31-40","41-50",">50"),
                         right=FALSE) 
#incentive_period
hiv10to16$incentive_period=NA #create blank column vector
incentivedates <- c(seq(as.Date("2014-11-15"), as.Date("2015-01-15"), 
                        by = "1 day"),
                    seq(as.Date("2015-06-15"), as.Date("2015-08-15"), 
                        by = "1 day"),
                    seq(as.Date("2015-11-15"),as.Date("2016-01-15"),
                        by="1 day"),
                    seq(as.Date("2016-06-15"),as.Date("2016-08-15"),
                        by="1 day"))
hiv10to16$incentive_period <- as.numeric(hiv10to16$donation_date %in% 
                                           incentivedates) 
#'1' will mean that it is an incentive period
hiv10to16<-hiv10to16[,c("donation_year","donation_date",
                        "incentive_period","Zone","Gender","age",
                        "age_group","race","Code","fixed/mobile","ART Y N")]
colnames(hiv10to16)[11]<-"art_y.n"
View(hiv10to16)

###subset data to only include the true/false elites indicated by Code="S"
new.hivdata <- subset(hiv10to16,!hiv10to16$art_y.n=="#N/A")
best.hivdata <- subset(new.hivdata,new.hivdata$Code=="S")

###use table() to count how many observations there are of each categorical
###level
#Y (false elites), N (true elites)
table(best.hivdata$art_y.n)

#Gender
table(best.hivdata$Gender,best.hivdata$art_y.n)

#by race
best.hivdata$race[is.na(best.hivdata$race)]<-"Unknown" #change blank observations to unknown
table(best.hivdata$race,best.hivdata$art_y.n)

#by age_group
table(best.hivdata$age_group,best.hivdata$art_y.n)

#by zone
table(best.hivdata$Zone,best.hivdata$art_y.n)

#by fixed/mobile
table(best.hivdata$`fixed/mobile`,best.hivdata$art_y.n)

#by year
table(best.hivdata$donation_year,best.hivdata$art_y.n)

#by incentive period
table(best.hivdata$incentive_period,best.hivdata$art_y.n)