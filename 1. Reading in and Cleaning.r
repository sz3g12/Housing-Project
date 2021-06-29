
# Set up and test on 1 file -----------------------------------------------
library(stringr)
library(dplyr)
library(plyr)
library(tidyverse)

setwd('C:/Users/zhangxinhua/Desktop/Housing/Data')
# test operation on one file to apply to all
test <- read.csv('C:/Users/zhangxinhua/Desktop/Housing/Data/original/4HGj3vDLfgzN6LhYkEmINj8XTBQy0W1qPyrzvnSFG5N76MBKYn8p!-657403293!1641300427!1553232490699 - 2019-03-22T141119.359.csv', header = FALSE)
# open the file and find a few issues with the input
# 1. extra text on the top row-> to drop
test<-test[-1,]
# need to as.character and unlist to index the colnames
# this is to name the first row into colnames
colnames(test)<-as.character(unlist(test[1,]))
# to drop the names row as they are extra (they already became headers)
test<-test[-1,]
# to remove the bottom part with extra text remarks
# becasue the remarks are consistent and they do not go over to columns
# such as "Type of Area", "Floor Level","Date of Sale" so we use these to filter
test<-test[test$`Date of Sale`!='',]

# Handle all files and merge together -------------------------------------
# make all files in the working directory as lists and use apply function to 
# do the same for all files
files<-list.files(path='C:/Users/zhangxinhua/Desktop/Housing/Data/original', pattern='*.csv', full.names=TRUE, recursive = FALSE)

i<-0
for (file in files){
  i<-i+1
  t<-read.csv(file, header = FALSE)
  t<-t[-1,]
  colnames(t)<-as.character(unlist(t[1,]))
  t<-t[-1,]
  t<-t[t$`Date of Sale`!='',]
  write.csv(t, file = paste("file_",i,".csv"))
}
# bind together with ldply
myfiles<-list.files(path='C:/Users/zhangxinhua/Desktop/Housing/Data', pattern='*.csv', full.names=TRUE, recursive = FALSE)
estate_raw<-ldply(myfiles, read_csv)
#!!! this might be adjusted
estate<-estate_raw[,-c(1,2,20:35)]
# rename the variables to remove spaces so that it's easier to type
colnames(estate) <- c("Project_Name",  "Street_Name", "Type", "Postal_District",
                      "Market_Segment", "Tenure", "Type_of_Sale", "No_of_Units",
                      "Price", "Nett_Price", "Area_Sqft", "Type_of_Area", 
                      "Floor_Level", "Unit_Price_psf", "Date_of_Sale", "Area_Sqm",
                      "Unit_Price_psm")
# too shift the column in proper order 
estate1<-estate[, c(1:10, 12, 13, 15, 11, 14, 16, 17)]
# last thing to hanle in raw data is that some area units are using sqft and some sqm
# same issue for unit price units. need to convert and drop the extra 2 columns
# convert rate is 10.764 as indicated in the file
estate1$Area_Sqft_new<-round(estate1$Area_Sqm*10.764)
estate1$Area_Sqm_new<-round(estate1$Area_Sqft/10.764)
estate1$Area_Sqft<-pmax(estate1$Area_Sqft, estate1$Area_Sqft_new, na.rm=TRUE)
estate1$Area_Sqm<-pmax(estate1$Area_Sqm, estate1$Area_Sqm_new, na.rm=TRUE)
estate1$Unit_Price_psf<-round(estate1$Price/estate1$Area_Sqft)
estate1$Unit_Price_psm<-round(estate1$Price/estate1$Area_Sqm)
estate<-estate1[, -c(18, 19)]


# Check duplicates --------------------------------------------------------
# there may be duplicates in manual downloads
# estate$x<-duplicated(estate)
# check<-estate[estate$x==TRUE,]
# x<-distinct(estate)
# y<-unique(estate)
# write.csv(check, 'check.csv')
# write.csv(estate, 'estate.csv')

# After check, there are really duplicates
# remove them by just getting the unique entries 
# (if same price, same sqm and same transaction date, they are the same)
es_df<-distinct(estate)

# save rdata
save(es_df, file = "es_df.RData")
