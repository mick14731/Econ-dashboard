##################################################
## Project: Dashboard
## Script purpose: visializing custom CPI aggregate indexs 
## Date: Feb 2nd 2019 
## Author: Mitchell Hughes
##################################################


library(plyr)
library(tidyr)
library(xts)
library(dygraphs)
library(quantmod)

source("functions.R")

###############################################################
#
#  Downlaoding tables from stats Canada
#  Need indexs and weights
#
###############################################################

# Should add something that check is we need to downlaod again or not

last_month <-  max(as.yearmon(read.csv("data/CPI.csv")$REF_DATE))  
# if( 12*as.numeric((as.yearmon(Sys.Date()) - last_month)) >= 2)
# 
if( 12*as.numeric(as.yearmon(Sys.Date()) - last_month) >= 2){

  CPI_INDEX <-download_statscan("18100004")
  
  
  # This is a big table with each base year need to pick one, 2002= 100 has all the history (other don't), so we will pick those observations
  # and we are onlt interested in national aggregates so only keeping rows with GEO == Canada
  # for size reasons and since looking at inflation pre and post targeting doesn't often provide meaningfull insights, I will limit the sample
  # to after 1991
  # We only need the the date, product and value. explinations of the other columns can be found at:
  # https://www.statcan.gc.ca/eng/developers/csv/user-guide
  
  
  # I will focusing on the CPI-55 top level catagories most often focused on. These catagories comprise the inputs for the 3 core measures so 
  # they are the most worth exploring (note: need to Keep All-Items)
  
  top_55 <- c('v41690976', 'v41690987', 'v41690992', 'v41691000',
              'v41691010', 'v41691020', 'v41691029', 'v41691046',
              'v41691051', 'v41691056', 'v41691057', 'v41691058',
              'v41691059', 'v41691060', 'v41691061', 'v41691063',
              'v41691064', 'v41691065', 'v41691066', 'v41691069',
              'v41691072', 'v41691075', 'v41691078', 'v41691081',
              'v41691089', 'v41691093', 'v41691097', 'v41691107',
              'v41691109', 'v41691113', 'v41691118', 'v41691123',
              'v41691132', 'v41691133', 'v41691134', 'v41691136',
              'v41691137', 'v41691140', 'v41691147', 'v41691150',
              'v41713463', 'v41713464', 'v41691164', 'v41691169',
              'v41691172', 'v41691180', 'v41691181', 'v41691184',
              'v41691190', 'v41691193', 'v41691198', 'v41691202',
              'v41691208', 'v41691212', 'v41691216', 'v41690973') # v41690973 is All-items 
  
  names <- as.character(unique(CPI_INDEX[which(CPI_INDEX$VECTOR %in% top_55),"Products and product groups"]))
  
  CPI_INDEX_2002 <- CPI_INDEX[which(CPI_INDEX$UOM == "2002=100"& CPI_INDEX$GEO == "Canada" & CPI_INDEX$VECTOR %in% top_55),
                              c("REF_DATE","Products and product groups","VALUE")]
  write.csv(CPI_INDEX_2002,"data/CPI.csv",row.names = FALSE)
  CPI_INDEX_2002$REF_DATE <- as.yearmon(CPI_INDEX_2002$REF_DATE,format="%Y-%m")
   
  CPI_INDEX_2002<- spread(CPI_INDEX_2002,`Products and product groups`,VALUE)
  
 
  
} else {
  CPI_INDEX_2002 <- read.csv("data/CPI.csv",check.names = FALSE)
  CPI_INDEX_2002$REF_DATE <- as.yearmon(CPI_INDEX_2002$REF_DATE,format="%Y-%m")
  CPI_INDEX_2002<- spread(CPI_INDEX_2002,`Products and product groups`,VALUE)
} 
  

  CPI_INDEX.xts <- as.xts(CPI_INDEX_2002[,-1],CPI_INDEX_2002[,1])["1992-01-01/"]
  
  CPI_INDEX.xts <- CPI_INDEX.xts[,names]
  
  rm(CPI_INDEX_2002)

  
  ############################################
  #                                          #
  #    Setting up the weights data frame     #
  #                                          #
  ############################################

if( 12*as.numeric(as.yearmon(Sys.Date()) - last_month) >= 2){
  CPI_Weights <- download_statscan("18100007")  
  
  CPI_Weights_canada <- CPI_Weights[which(CPI_Weights$GEO == "Canada" & 
                                     CPI_Weights$`Price period of weight` =="Weight at basket link month prices" &
                                     CPI_Weights$`Products and product groups` %in% colnames(CPI_INDEX.xts) &
                                     CPI_Weights$`Geographic distribution of weight` =="Distribution to selected geographies"),
                              c("REF_DATE","Products and product groups","VALUE")]
  write.csv(CPI_Weights_canada,file = "data/WEIGHTS.csv",row.names = FALSE)
  CPI_Weights_canada$REF_DATE <- as.yearmon(CPI_Weights_canada$REF_DATE,format="%Y-%m")
  
  CPI_Weights_canada<- spread(CPI_Weights_canada,`Products and product groups`,VALUE)
  
} else {
  CPI_Weights_canada <- read.csv("data/WEIGHTS.csv",check.names = FALSE)
  CPI_Weights_canada$REF_DATE <- as.yearmon(CPI_Weights_canada$REF_DATE,format="%Y-%m")
  CPI_Weights_canada<- spread(CPI_Weights_canada,`Products and product groups`,VALUE)
}  

  CPI_Weights.xts <- as.xts(CPI_Weights_canada[,-1],CPI_Weights_canada[,1])


# since this is essentially a toy example and some of the measures that are now in the the CPI-55 didn't previously exists, 
# I am going to assume their weights backwards from when they were introduced than scale everything so they sum to 100

CPI_Weights.xts <- na.locf(CPI_Weights.xts,fromLast = TRUE) 

CPI_Weights_tmp.xts <- CPI_Weights.xts[,-grep("All-items",colnames(CPI_Weights.xts))]

CPI_Weights_tmp.xts <- 100*(CPI_Weights_tmp.xts/rowSums(CPI_Weights_tmp.xts))



CPI_Weights.xts <- merge.xts(CPI_Weights.xts$`All-items`,CPI_Weights_tmp.xts, suffixes = NULL)



CPI_Weights.xts <- na.locf(merge.xts(CPI_Weights.xts,index(CPI_INDEX.xts)),fromLast = FALSE)["1992-01/"]

names.dot <- gsub("[[:space:]]",".",names)

names.dot <- gsub("[[:punct:]]",".",names.dot)


CPI_Weights.xts <- CPI_Weights.xts[,names.dot]
colnames(CPI_Weights.xts) <- colnames(CPI_INDEX.xts)

rm(CPI_Weights_tmp.xts)
rm(CPI_Weights_canada)

# for Jan 1991 - Dec 1991 RV = INDEX[Jan 1991]
# for Jan 1992 - Dec 1995 RV = INDEX[Jan 1992]
# for Jan 1996 - Dec 2000 RV = INDEX[Jan 1996]
# for Jan 2001 - Dec 2004 RV = INDEX[Jan 2001]
# for Jan 2005 - Dec 2010 RV = INDEX[Jan 2005]
# for Jan 2011 - Dec 2012 RV = INDEX[Jan 2011]
# for Jan 2013 - Dec 2014 RV = INDEX[Jan 2013]
# for Jan 2015 - Dec 2018 RV = INDEX[Jan 2015]

TMP <-rbind(CPI_Weights.xts[1,], CPI_Weights.xts[-nrow(CPI_Weights.xts),])
index(TMP) <- index(CPI_Weights.xts)
tmp <-as.xts(rowSums(abs(TMP - CPI_Weights.xts)),index(CPI_Weights.xts))
tmp["1992-01-01"] <- 1

RV <- CPI_INDEX.xts[index(tmp[tmp !=0]),]

RV <- na.locf(merge.xts(RV,index(CPI_INDEX.xts)),fromLast = F)

RV <- RV[,names.dot]
colnames(RV) <- colnames(CPI_INDEX.xts)


rm(TMP)
rm(tmp)



