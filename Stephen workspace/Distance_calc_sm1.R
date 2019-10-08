#Stephen Morgan
#


#Clear all
rm(list=ls())

setwd("~/Dropbox/Nigeria ag livestock conflict/Data")


# install packages
library(pacman)
library(tidyverse)
library(geosphere)
library(data.table)
library(reshape)

#p_load(geosphere, data.table, maps, reshape, dpylr, ggmap, tidyverse, maps, mapdata, ggplot2, 
#       mapproj, JavaGD, DeducerSpatial)
  

# import data
geodata.hh10 <- read.csv("LSMS/hh10/data/Geodata/nga_householdgeovariables_y1.csv")
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

save(hh10_dist_matrix, file="~/Dropbox/Nigeria ag livestock conflict/Stephen workspace/hh10_dist_matrix.Rda")

#Once complete, need to rename the columns after the new event IDs - should probably name them using the event_id_no_cnty vector



