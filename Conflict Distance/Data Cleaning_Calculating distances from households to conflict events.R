#Clear all
rm(list=ls())


# install packages
library(pacman)
library(tidyverse)
library(geosphere)
library(data.table)
library(reshape)

#p_load(geosphere, data.table, maps, reshape, dpylr, ggmap, tidyverse, maps, mapdata, ggplot2, 
#       mapproj, JavaGD, DeducerSpatial)

## Set wording directory
setwd("C:/Users/avuwa/Dropbox (UFL)/Nigeria ag livestock conflict/Data")


# import data
geodata.hh10 <- read.csv("LSMS/hh10/data/Geodata/nga_householdgeovariables_y1.csv")
geodata.hh12 <- read.csv("LSMS/hh12/data/Geodata Wave 2/nga_householdgeovars_y2.csv")
geodata.hh15 <- read.csv("LSMS/hh15/data/Geodata/nga_householdgeovars_y3.csv")
conflict.nig <- read.csv("ACLED/Nigeria/data/1900-01-01-2019-04-16-Nigeria.csv")

#Add unique identifier row into the ACLED dataset - ACLED drops some event IDs
conflict.nig <- conflict.nig %>%
  arrange(event_id_no_cnty) %>%
  tibble::rowid_to_column("ID")

# Select columns of household geodata that identifies specific households
geodata.hh10 <- geodata.hh10 %>%
  select(zone:dist_admctr)

for (i in 1:length(conflict.nig$ID)) {
  
  #Select each conflict event
  conflict_temp <- conflict.nig %>%
    filter(ID==i)
  
  #Merge the two datasets
  full_temp <- expand.grid.df(geodata.hh10, conflict_temp)
  
  #Calculate the distance (distVincentyEllipsoid) between each household and conflict event i
  temp_dist <- full_temp %>%
    mutate(dist2_km := distVincentyEllipsoid(matrix(c(lon_dd_mod, lat_dd_mod), ncol = 2), matrix(c(longitude, latitude), ncol = 2))/1000) %>%
    select(dist2_km)
  
  #Store the calculated distances in a matrix - initialize the matrix if it's the first loop in the sequence
  if (i==1) {
    hh10_dist_matrix <- temp_dist
  } else {
    hh10_dist_matrix <- hh10_dist_matrix %>%
      bind_cols(temp_dist)
  }
  
}


#Rename columns and rownames
id <- conflict.nig[,5]
id <- as.data.frame(matrix(id, 14103, 1))
id <- transpose((id))
names(hh10_dist_matrix) <- id[,1:14103]
colnames(hh10_dist_matrix) <- paste("event_id_no_cnty_",colnames(hh10_dist_matrix),sep="")
rownames(hh10_dist_matrix) <- paste("HH_",rownames(hh10_dist_matrix),sep="")


## Save file
save(hh10_dist_matrix, file="hh10_dist_matrix.RData")


#### ## Wave 2 - year 2012
# Select columns of household geodata that identifies specific households
geodata.hh12 <- geodata.hh12[,-c(13:45)]


for (i in 1:length(conflict.nig$ID)) {
  
  #Select each conflict event
  conflict_temp <- conflict.nig %>%
    filter(ID==i)
  
  #Merge the two datasets
  full_temp <- expand.grid.df(geodata.hh12, conflict_temp)
  
  #Calculate the distance (distVincentyEllipsoid) between each household and conflict event i
  temp_dist <- full_temp %>%
    mutate(dist2_km := distVincentyEllipsoid(matrix(c(LON_DD_MOD, LAT_DD_MOD), ncol = 2), matrix(c(longitude, latitude), ncol = 2))/1000) %>%
    select(dist2_km)
  
  #Store the calculated distances in a matrix - initialize the matrix if it's the first loop in the sequence
  if (i==1) {
    hh12_dist_matrix <- temp_dist
  } else {
    hh12_dist_matrix <- hh12_dist_matrix %>%
      bind_cols(temp_dist)
  }
  
}




#Rename columns and rownames
names(hh12_dist_matrix) <-  id[,1:14103]
colnames(hh12_dist_matrix) <- paste("event_id_no_cnty_",colnames(hh12_dist_matrix),sep="")
rownames(hh12_dist_matrix) <- paste("HH_",rownames(hh12_dist_matrix),sep="")

## Save file
save(hh12_dist_matrix, file="hh12_dist_matrix.RData")



#### ## Wave 3 - year 2015
# Select columns of household geodata that identifies specific households
geodata.hh15 <- geodata.hh15[,-c(13:45)]


for (i in 1:length(conflict.nig$ID)) {
  
  #Select each conflict event
  conflict_temp <- conflict.nig %>%
    filter(ID==i)
  
  #Merge the two datasets
  full_temp <- expand.grid.df(geodata.hh15, conflict_temp)
  
  #Calculate the distance (distVincentyEllipsoid) between each household and conflict event i
  temp_dist <- full_temp %>%
    mutate(dist2_km := distVincentyEllipsoid(matrix(c(LON_DD_MOD, LAT_DD_MOD), ncol = 2), matrix(c(longitude, latitude), ncol = 2))/1000) %>%
    select(dist2_km)
  
  #Store the calculated distances in a matrix - initialize the matrix if it's the first loop in the sequence
  if (i==1) {
    hh15_dist_matrix <- temp_dist
  } else {
    hh15_dist_matrix <- hh15_dist_matrix %>%
      bind_cols(temp_dist)
  }
  
}



#Rename columns and rownames
names(hh15_dist_matrix) <- id[,1:14103]
colnames(hh15_dist_matrix) <- paste("event_id_no_cnty_",colnames(hh15_dist_matrix),sep="")
rownames(hh15_dist_matrix) <- paste("HH_",rownames(hh15_dist_matrix),sep="")

## Save file
save(hh15_dist_matrix, file="hh15_dist_matrix.RData")
