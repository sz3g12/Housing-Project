# load data
setwd('C:/Users/zhangxinhua/Desktop/Housing/Data')
load('es_df.RData')
library(stringr)
library(dplyr)
library(tidyverse)


# Initial data exploration ------------------------------------------------

# simple data exploration to see which data needs pre-processing
summary(es_df)
View(as.data.frame(table(es_df$Project_Name)))
# landed housing development is a big proportion, needs to look into it and tag differently

View(as.data.frame(table(es_df$Street_Name)))
# 1 street can be multiple properties

View(as.data.frame(table(es_df$Type)))
# search and understand the meanig of these

View(as.data.frame(table(es_df$Postal_District)))
# map to the postal regions then regions

View(as.data.frame(table(es_df$Market_Segment)))
# understand the market segments

View(as.data.frame(table(es_df$Tenure)))

# from tenure can derive part of the TOP time, 
# except for freehold cases and the ones that do not specify

View(as.data.frame(table(es_df$Type_of_Sale)))
# understand subsale

View(as.data.frame(table(es_df$Type_of_Area)))
# condo and EC ->strata?

View(as.data.frame(table(es_df$Floor_Level)))
# group floor levels, imputate missing values

View(as.data.frame(table(es_df$Date_of_Sale)))

# numeric variabls
summary(es_df$No_of_Units)
View(as.data.frame(table(es_df$No_of_Units)))
mul_units<-es_df[es_df$No_of_Units!=1,]
View(mul_units)
# there are 99 cases of multiple units. 

summary(es_df$Price)
#min 40,000 median 1238k, max is 980000K, it should be aggregated and need to be broken


summary(es_df$Nett_Price)
# not many Nett Price are filled
check<-es_df[es_df$Nett_Price!='-',]
check$diff<-as.numeric(check$Price)-as.numeric(check$Nett_Price)
check$diff2<-as.numeric(check$price_sr)-as.numeric(check$Price)
check$price_sr<-check$Unit_Price_psm*check$Area_Sqm
View(as.data.frame(table(es_df$Nett_Price)))
# not sure what is Nett Price but it seems like the calculation of sqm and psm are based on Price

summary(es_df$Area_Sqm)
# some need to be divided

summary(es_df$Unit_Price_psm)
# some psm are super low-> after checking, it's because the tenure left is very low


# Data preprocessing ------------------------------------------------------
# drop columns: Nett_Price, Area_Sqft, Unit_Price_psf
keeps=c("Project_Name",  "Street_Name", "Type", "Postal_District",
        "Market_Segment", "Tenure", "Type_of_Sale", "No_of_Units",
        "Price", "Type_of_Area", "Floor_Level", "Date_of_Sale", 
        "Area_Sqm","Unit_Price_psm")
estate_df = es_df[,keeps]

# project name and street name handling
table(estate_df$Project_Name)
# 2K+ project names and 1.8K+ street names, impossible and not meaningful to map

View(as.data.frame(table(es_df$Tenure)))
# we can see a pattern of expression e.g. '999999 yrs lease commencing from 1993'
# subset the obs with 6 chars
estate_df$Tenure_fm<-sapply(strsplit(estate_df$Tenure, " "), length)
check<-estate_df[estate_df$Tenure_fm!=6,]
# there are only three cases that the length is not in that format
# 'freehold / 99 years lease hold / 110 Yrs From 01/11/2017 / NA'
# let's handle NA first
estate_df%>%
  filter(is.na(Tenure))
# 5 records have NA in tenure
# try to map back the ones with the same project name
estate_df[estate_df$Project_Name=='SILVER HILL'& !is.na(estate_df$Tenure),]
# by comparing with the other records, the missing info should be 
# '945 yrs lease commencing from 1936'
estate_df[estate_df$Project_Name=='SILVER HILL'& is.na(estate_df$Tenure),]$Tenure->estate_df[estate_df$Project_Name=='SILVER HILL'& !is.na(estate_df$Tenure),]$Tenure

estate_df[estate_df$Project_Name=='LANDED HOUSING DEVELOPMENT'&estate_df$Street_Name=='CLOVER RISE'& !is.na(estate_df$Tenure),]
# the missing info should be 'Freehold'
estate_df[estate_df$Project_Name=='LANDED HOUSING DEVELOPMENT'&estate_df$Street_Name=='CLOVER RISE'& is.na(estate_df$Tenure),]$Tenure->estate_df[estate_df$Project_Name=='LANDED HOUSING DEVELOPMENT'&estate_df$Street_Name=='CLOVER RISE'& !is.na(estate_df$Tenure),]$Tenure

estate_df[estate_df$Project_Name=='LANDED HOUSING DEVELOPMENT'&estate_df$Street_Name=='LORONG NANGKA'& !is.na(estate_df$Tenure),]
# the missing info should be 'Freehold'
estate_df[estate_df$Project_Name=='LANDED HOUSING DEVELOPMENT'&estate_df$Street_Name=='LORONG NANGKA'& is.na(estate_df$Tenure),]$Tenure->estate_df[estate_df$Project_Name=='LANDED HOUSING DEVELOPMENT'&estate_df$Street_Name=='LORONG NANGKA'& !is.na(estate_df$Tenure),]$Tenure

estate_df[estate_df$Project_Name=='EMERY POINT'& !is.na(estate_df$Tenure),]
# the missing info should be 'Freehold'
estate_df[estate_df$Project_Name=='EMERY POINT'& is.na(estate_df$Tenure),]$Tenure->estate_df[estate_df$Project_Name=='EMERY POINT'& !is.na(estate_df$Tenure),]$Tenure

estate_df[estate_df$Project_Name=='RV RESIDENCES'& !is.na(estate_df$Tenure),]
# the missing info should be 999 yrs lease commencing from 1877
estate_df[estate_df$Project_Name=='RV RESIDENCES'& is.na(estate_df$Tenure),]$Tenure->estate_df[estate_df$Project_Name=='RV RESIDENCES'& !is.na(estate_df$Tenure),]$Tenure

#! this is manual matching due to few records and fast iteration. Need to convert the codes
# e.g. if find same project name, then..if not then mode..

# get the tail of the string, it's the TOP year
estate_df$TOP<-word(estate_df$Tenure[],-1)
# get the head of the string, it's the lease period
estate_df$Lease<-word(estate_df$Tenure[],1)

# looks like it's done. Let's check what happens to the special cases
View(as.data.frame(table(check$TOP)))
View(as.data.frame(table(check$Lease)))

# the lease is perfectly done, TOP only needs to update the 01/11/2017 to 2017
# try to map the 99 year lease starting years
# hanld the one with 01/11/2017 first
#! to be updated, in here i just simple hardcode as 2017. need to be flexible
estate_df$TOP[estate_df$TOP=="01/11/2017"] <- "2017"
# library(plyr)
# estate_df$TOP <- revalue(estate_df$TOP, c("01/11/2017" = "2017"))
# detach("package:plyr", unload=TRUE)
View(as.data.frame(table(estate_df$TOP)))
# 19.5K Freehold abd 29 leasehold without TOP year 
# it's ok for Freehold to be without TOP year since it has unlimited tenure, but for leasehold one, we need to check
estate_df %>% 
  filter(estate_df$TOP=="leasehold")
# after inspecting, I realised that all the ones with 'leasshold' instead of a TOP year is the same project called "VIEW AT KISMIS"
# after searching on interenet, I realised that it does not have a TOP year.
# after further research, i realised the the 'TOP' i marked is not exactly 'TOP' it's just when the lease starts
# anyways, checked the online the starting year is 2019 https://esingaporeproperty.sg/property/view-at-kismis-condo/
estate_df$TOP[estate_df$TOP=="leasehold"] <- "2019"
# now the TOP part are really years and 'Freehold'
