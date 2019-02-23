
library(plyr)
library(tidyr)
library(xts)
library(dygraphs)
library(quantmod)

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


ind_add <- function(choices,indexs,refValues,Weights){
  
  
  
  sub <- vector("numeric",dim(refValues)[1])
  
  for(i in 1:length(choices)){
    
    sub <- sub + as.data.frame(indexs[,choices[i]]*Weights[,choices[i]]/refValues[,choices[i]])  
    
  }
  
  IND <- as.xts(sub,index(indexs))
  W = apply(Weights[,choices],1,"sum")
  con <- IND/W
  
  refVal <- vector("numeric",dim(refValues)[1])
  
  refVal[1] <- 100
  
  for(i in 2:length(refVal)){
    if(as.numeric(refValues[i,"All-items"])==as.numeric(refValues[i-1,"All-items"])){
      refVal[i] <- refVal[i-1]
    }
    else{
      refVal[i]<- con[i-1]*refVal[i-1]
      
    }
    
  }
  
  target <- Delt(con*refVal,k=12)*100
  all_items <- Delt(indexs[,"All-items"],k=12)*100
  finish <-cbind(all_items,target)
  colnames(finish) <- c("All Items", "Target")
  
  
  return(finish)
  
} 

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
    if(as.numeric(refValues[i,"All-items"])==as.numeric(refValues[i-1,"All-items"])){
      refVal[i] <- refVal[i-1]
    }
    else{
      refVal[i]<- con[i-1]*refVal[i-1]
      
    }
    
  }
  
  target <- Delt(con*refVal,k=12)*100
  all_items <- Delt(indexs[,"All-items"],k=12)*100
  finish <-cbind(all_items,target)
  colnames(finish) <- c("All Items", "Target")
  
  
  return(finish)
  
} 