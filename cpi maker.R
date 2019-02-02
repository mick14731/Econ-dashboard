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

###############################################################
#
#  Downlaoding tables from stats Canada
#  Need indexs and weights
#
###############################################################

download_statscan <- function(tbl_num){
  tmp <- tempfile()
  
  url <- paste0("https://www150.statcan.gc.ca/n1/en/tbl/csv/", tbl_num, "-eng.zip" )
  
  download.file(url,tmp)
  
  tbl_data <- read.csv(unz(tmp,paste0(tbl_num,".csv")),check.names = FALSE)
  # For some reason, downloading CSV's from StatsCan always have the "REF_DATE" 
  # column with random characters in frot, but it's always the first column.
  colnames(tbl_data)[1] <- "REF_DATE"
  return(tbl_data)
}  
  


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
            'v41691208', 'v41691212', 'v41691216', 'v41690973')


CPI_INDEX_2002 <- CPI_INDEX[which(CPI_INDEX$UOM == "2002=100"& CPI_INDEX$GEO == "Canada" & CPI_INDEX$VECTOR %in% top_55),
                            c("REF_DATE","Products and product groups","VALUE")]

CPI_INDEX_2002$REF_DATE <- as.yearmon(CPI_INDEX_2002$REF_DATE,format="%Y-%m")
 
CPI_INDEX_2002<- spread(CPI_INDEX_2002,`Products and product groups`,VALUE)

 
CPI_INDEX.xts <- as.xts(CPI_INDEX_2002[,-1],CPI_INDEX_2002[,1])["1991-01-01/"]

 



CPI_Weights <- download_statscan("18100007")  

CPI_Weights_c <- CPI_Weights[which(CPI_Weights$GEO == "Canada" & 
                                   CPI_Weights$`Price period of weight` =="Weight at basket link month prices" &
                                   CPI_Weights$`Products and product groups` %in% colnames(CPI_INDEX.xts) &
                                   CPI_Weights$`Geographic distribution of weight` =="Distribution to selected geographies"),
                            c("REF_DATE","Products and product groups","VALUE")]

CPI_Weights_c$REF_DATE <- as.yearmon(CPI_Weights_c$REF_DATE,format="%Y-%m")

CPI_Weights_c<- spread(CPI_Weights_c,`Products and product groups`,VALUE)


CPI_Weights.xts <- as.xts(CPI_Weights_c[,-1],CPI_Weights_c[,1])

# since this is essentially a toy example and some of the measures that are now in the the CPI-55 didn't previously exists, 
# I am going to assume their weights backwards from when they were introduced than scale everything so they sum to 100

CPI_Weights_2.xts <- na.locf(CPI_Weights.xts,fromLast = TRUE) 
CPI_Weights_2.xts <- CPI_Weights_2.xts[,-grep("All-items",colnames(CPI_Weights_2.xts))]

CPI_Weights_2.xts <- 100*(CPI_Weights_2.xts/rowSums(CPI_Weights_2.xts))

# we need to calulate the referance values for the indexe. The referenece values change when the basket weights change

# for Jan 1991 - Dec 1991 RV = INDEX[Jan 1991]
# for Jan 1992 - Dec 1995 RV = INDEX[Jan 1992]
# for Jan 1996 - Dec 2000 RV = INDEX[Jan 1996]
# for Jan 2001 - Dec 2004 RV = INDEX[Jan 2001]
# for Jan 2005 - Dec 2010 RV = INDEX[Jan 2005]
# for Jan 2011 - Dec 2012 RV = INDEX[Jan 2011]
# for Jan 2013 - Dec 2014 RV = INDEX[Jan 2013]
# for Jan 2015 - Dec 2018 RV = INDEX[Jan 2015]

RV <- CPI_INDEX.xts["1991-01"]
# Worst way to do this? I think so
for(i in 1:11){
  RV <- rbind(RV,CPI_INDEX.xts["1991-01"])
}
for(i in 1:48){
  RV <- rbind(RV,CPI_INDEX.xts["1992-01"])
}
for(i in 1:60){
  RV <- rbind(RV,CPI_INDEX.xts["1996-01"])
}
for(i in 1:48){
  RV <- rbind(RV,CPI_INDEX.xts["2001-01"])
}

for(i in 1:72){
  RV <- rbind(RV,CPI_INDEX.xts["2005-01"])
}
for(i in 1:24){
  RV <- rbind(RV,CPI_INDEX.xts["2011-01"])
}
for(i in 1:24){
  RV <- rbind(RV,CPI_INDEX.xts["2013-01"])
}

for(i in 1:48){
  RV <- rbind(RV,CPI_INDEX.xts["2015-01"])
}

index(RV) <- index(CPI_INDEX.xts)

CPI_WEIGHTS <- as.xts(matrix(data = NA, nrow=nrow(CPI_INDEX.xts),ncol=ncol(CPI_INDEX.xts)),index(CPI_INDEX.xts))
colnames(CPI_WEIGHTS) <- colnames(CPI_INDEX.xts)


CPI_Weights_2.xts <- na.locf(merge.xts(CPI_Weights_2.xts,index(CPI_INDEX.xts)),fromLast = FALSE)["1991-01/"]

colnames(CPI_Weights_2.xts) <- colnames(CPI_INDEX.xts)


# *NEED* ALL Items in each table, the function depends on the series being ordered, but that might not hold
# check over function and tables for ordering, without setting col names  

# CPI_total <- CPI_INDEX.xts$`All-items`
# CPI_INDEX.xts$`All-items` <- NULL
# CPI_total_RV <- RV$`All-items`
# RV$`All-items` <- NULL




ind_sub <- function(base,choices,indexs,refValues,Weights){
  
  
  
  sub <- vector("numeric",dim(refValues)[1])
  
  for(i in 1:length(choices)){
    
    sub <- sub + as.data.frame(indexs[,choices[i]]*Weights[,choices[i]]/refValues[,choices[i]])  
    
  }
  
  IND <- base - as.xts(sub,index(base))
  W = 100 - apply(Weights[,choices],1,"sum")
  con <- IND/W
  
  refVal <- vector("numeric",dim(refValues)[1])
  
  refVal[1] <- 100
  
  for(i in 2:length(refVal)){
    if(as.numeric(refValues[i,6])==as.numeric(refValues[i-1,6])){
      refVal[i] <- refVal[i-1]
    }
    else{
      refVal[i]<- con[i-1]*refVal[i-1]
      
    }
    
  }
  
  target <- Delt(con*refVal,k=12)*100
  all_items <- Delt(indexs[,1],k=12)*100
  finish <-cbind(all_items,target)
  colnames(finish) <- c("All Items", "Target")
  
  
  return(finish)
  
  
  
}

test <- ind_sub(CPI_total,c("Electricity","Gasoline"),indexs = CPI_INDEX.xts,refValues = RV,CPI_Weights_2.xts)["2000-01/"]


dygraph(test)
