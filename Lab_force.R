##################################################
## Project: Dashboard
## Script purpose: visializing Labour force characteristics 
## Date: Feb 23rd 2019 
## Author: Mitchell Hughes
##################################################

library(plyr)
library(tidyr)
library(xts)
library(stats)
library(dplyr)
library(leaflet)
library(sf)
library(rmapshaper)

source("functions.R")

###############################################################
#
#  Downlaoding tables from stats Canada
#  Need indexs and weights
#
###############################################################


# Should add something that check is we need to downlaod again or not
last_month <- max(as.yearmon(read.csv("data/LFS_Table_clean_an.csv")$REF_DATE,format ="%Y-%m"))    

if(as.yearmon(Sys.Date()) - last_month >= 2){
  LFS_Table <-download_statscan("14100287")
      
    ######
  #
  # For this example we will only look at the largest, adjusted provincial aggregates. 
  # other tables will have breakdowns by industry and other socioeconomic,
  # characteristics. The micro file would also allow for further deatialed analysis
  #
  ######
  
  LFS_Table_clean <- LFS_Table[which(LFS_Table$GEO != "Canada" &
                                     LFS_Table$Statistics == "Estimate" &
                                     LFS_Table$`Age group` == "15 years and over" &
                                     LFS_Table$`Data type` == "Seasonally adjusted" &
                                     LFS_Table$Sex == "Both sexes"),
                               c("REF_DATE","GEO","Labour force characteristics","VALUE")]
  
  
  LFS_Table_clean_an <- LFS_Table_clean
  write.csv(LFS_Table_clean_an,"data/LFS_Table_clean_an.csv",row.names = FALSE)
  LFS_Table_clean_an$REF_DATE <- as.numeric(substr(LFS_Table_clean_an$REF_DATE,1,4))
  
  lab_chars <- unique(LFS_Table_clean_an$`Labour force characteristics`)
  
  LFS_Table_clean_an <- aggregate(LFS_Table_clean_an$VALUE,
                    list(LFS_Table_clean_an$REF_DATE,LFS_Table_clean_an$GEO,LFS_Table_clean$`Labour force characteristics`),
                    mean)
  
  
  colnames(LFS_Table_clean_an) <- c("year","GEO","Labour force characteristics","VALUE")

} else{
  LFS_Table_clean_an <- read.csv("data/LFS_Table_clean_an.csv",check.names = FALSE)
  LFS_Table_clean_an$REF_DATE <- as.numeric(substr(LFS_Table_clean_an$REF_DATE,1,4))
  
  lab_chars <- unique(LFS_Table_clean_an$`Labour force characteristics`)
  
  LFS_Table_clean_an <- aggregate(LFS_Table_clean_an$VALUE,
                                  list(LFS_Table_clean_an$REF_DATE,LFS_Table_clean_an$GEO,LFS_Table_clean$`Labour force characteristics`),
                                  mean)
  
  
  colnames(LFS_Table_clean_an) <- c("year","GEO","Labour force characteristics","VALUE")
  
}

  
tmp <- st_read("data/gpr_000a11a_e.shp")
tmp$PRNAME <- tmp$PRENAME
tmp$PRFNAME <- tmp$PRENAME
tmp$PRFABBR <- tmp$PREABBR  
data.p <- tmp %>% 
  st_transform(4326) %>%
  rmapshaper::ms_simplify()
data.p$PRUID <- as.character(data.p$PRUID) %>% as.numeric
data.p <- data.p[which(data.p$PRUID < 60),]


# 
# 
# #dataframe with same structure as statscan csv after processing
# unem <- runif(10,min=0,max=100)
# unem1 <- unem+runif(1,-10,10)
# unem2 <- unem1+runif(1,-10,10)
# unemployment <- c(unem,unem1,unem2)
# #dataframe with same structure as statscan csv after processing
# X <- data.frame("id" = c(10,11,12,13,24,35,46,47,48,59,
#                          10,11,12,13,24,35,46,47,48,59,
#                          10,11,12,13,24,35,46,47,48,59),
#                 "Unemployment" = unemployment,
#                 "year" = c(rep(2000,10),rep(2001,10),rep(2002,10))
#)


lab_data <- left_join(data.p, LFS_Table_clean_an, by = c("PRENAME"= "GEO"))

ranges <- vector("list",length = length(lab_chars))

names(ranges) <- lab_chars

for(mes in lab_chars){
  ranges[[mes]] <- range(LFS_Table_clean_an[which(LFS_Table_clean_an$`Labour force characteristics` == mes), "VALUE"],na.rm = TRUE)
}
