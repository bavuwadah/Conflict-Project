######################
#
#  Wave 2
#
#######################
setwd("C:/Users/avuwa/Dropbox (UFL)/Nigeria ag livestock conflict")

library(dplyr)
library(lubridate)
library(purrr)

#
# LOADING DATA -----
#


## Events dataset

conflict_event <- read.csv("Data/ACLED/Nigeria/data/1900-01-01-2019-04-16-Nigeria.csv")
conflict_event$event.date <- as.Date(conflict_event$event_date, format="%d %B %Y")

conflict_event_wave2 <- conflict_event %>% filter(event.date >= "2011-09-01" & event.date <= "2013-04-30")

geodata.hh10 <- read.csv("Data/LSMS/hh10/data/Geodata/nga_householdgeovariables_y1.csv")
geodata.hh12 <- read.csv("Data/LSMS/hh12/data/Geodata Wave 2/nga_householdgeovars_y2.csv")

## Distance matrix
### load distance matrix

load("C:/Users/avuwa/Dropbox (UFL)/Nigeria ag livestock conflict/Benjamin workspace/Conflict Distance/hh12_dist_matrix.RData")

dist_matrix_wave2 <- hh12_dist_matrix %>% select(event_id_no_cnty_3215:event_id_no_cnty_4624, event_id_no_cnty_13328)
###1335
rm(hh12_dist_matrix)




######################
#
#  All conflict data
#
#######################


#
# SELECTING EVENTS -----
#

## List and object where the data will be stored

HH_vector <- lst()
id_events <- c()



for(h in 1:nrow(dist_matrix_wave2)){ ## Complete dataset
  # for(h in 1:5000){ ## Complete dataset
  hh <- c(dist_matrix_wave2[h,]) ## Selecting each Household (hh)
  events <- names(which(hh<=5)) ## selecting # of Events which met criteria
  distance <- unlist(hh[events]) ## Selecting the Distance for previous events
  names(distance) <- NULL
  for(i in seq_along(events)){ ## Capturing number of event
    id_events[i] <- strsplit(events,split = "_")[[i]][5]
  }
  ## Creating list with Events and Distances
  
  HH_vector[[h]] <- data.frame(idEvents = as.numeric(id_events), Distances = 1/(distance)^2)
  cat('Household No: ', h," - ", base::date(),'\n')
  id_events <- c()
}


rm(hh,h,i,distance,events,id_events) ## cleaning

gc() ## releasing memory

## Saving data
# save(HH_vector,file='List_HH_Events.RData')


#
# MATCHING WITH OTHER DATASET AND COUNTING FATALITIES ----
#


nFatalities <- matrix(0, nrow = 4802, ncol =  max(map_dbl(lapply(HH_vector,nrow),max))) ## Complete dataset


for (e in 1:4802){ ## complete dataset
  # for (e in 1:50){ ## sample dataset
  for(f in 1:ncol(nFatalities)){
    fa_vector <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    if(length(fa_vector) == ncol(nFatalities)){
      nFatalities[e,] <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    } else {
      di <- ncol(nFatalities) - length(fa_vector)
      fa_vector_ <- rbind(fa_vector,as.matrix(rep(0,di)))
      nFatalities[e,] <- fa_vector_
    }
    fa_vector <- c()
  }
  cat('Household No: ', e,' - ',base::date(),'\n')
}


# nFatalities
nFatalities[1:5,1:10] ## just inspecting

#
# CREATING SEVERITY MEASURE FOR EACH HH
#


S <- matrix(0,1,4802)
We <- lapply(HH_vector, "[[", 2) ## extracting weights (inverse of distances) from HH_vector

for(n in 1:4802){
  
  s <- We[[n]] * nFatalities[n,] 
  S[n] <- sum(s, NA, na.rm = TRUE)
  
  cat('Household No: ', n,' - ',base::date(),'\n')
  
}


S[1:50] ## just inspecting the first 100 hh


## Conflict Measures per Household

weighted_events <- data.frame(do.call(rbind, lapply(HH_vector, colSums))) ## summation of weights

weighted_events <- weighted_events[-1]

total_events <- data.frame(sapply(HH_vector, NROW))
total_fatalities <- data.frame(rowSums(nFatalities))
hh.id <- data.frame(geodata.hh12[,6])
Severity <- data.frame(matrix(S, 4802, 1))

wave2_conflict_measure_5km <- data.frame(cbind(hh.id, Severity, weighted_events, total_events, total_fatalities))


names(wave2_conflict_measure_5km) <- c("hhid", "severity_conflict_5km", "dist_weighted_events_5km", "total_events_5km", "total_fatalities_5km")


save(wave2_conflict_measure_5km, file='wave2_conflict_measure_5km.RData')


rm(HH_vector, weighted_events, total_events, total_fatalities, Severity, nFatalities)



## List and object where the data will be stored 20Km

HH_vector <- lst()
id_events <- c()



for(h in 1:nrow(dist_matrix_wave2)){ ## Complete dataset
  # for(h in 1:5000){ ## Complete dataset
  hh <- c(dist_matrix_wave2[h,]) ## Selecting each Household (hh)
  events <- names(which(hh<=20)) ## selecting # of Events which met criteria
  distance <- unlist(hh[events]) ## Selecting the Distance for previous events
  names(distance) <- NULL
  for(i in seq_along(events)){ ## Capturing number of event
    id_events[i] <- strsplit(events,split = "_")[[i]][5]
  }
  ## Creating list with Events and Distances
  
  HH_vector[[h]] <- data.frame(idEvents = as.numeric(id_events), Distances = 1/(distance)^2)
  cat('Household No: ', h," - ", base::date(),'\n')
  id_events <- c()
}


rm(hh,h,i,distance,events,id_events) ## cleaning

gc() ## releasing memory

## Saving data
# save(HH_vector,file='List_HH_Events.RData')


#
# MATCHING WITH OTHER DATASET AND COUNTING FATALITIES ----
#


nFatalities <- matrix(0, nrow = 4802, ncol =  max(map_dbl(lapply(HH_vector,nrow),max))) ## Complete dataset


for (e in 1:4802){ ## complete dataset
  # for (e in 1:50){ ## sample dataset
  for(f in 1:ncol(nFatalities)){
    fa_vector <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    if(length(fa_vector) == ncol(nFatalities)){
      nFatalities[e,] <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    } else {
      di <- ncol(nFatalities) - length(fa_vector)
      fa_vector_ <- rbind(fa_vector,as.matrix(rep(0,di)))
      nFatalities[e,] <- fa_vector_
    }
    fa_vector <- c()
  }
  cat('Household No: ', e,' - ',base::date(),'\n')
}


# nFatalities
nFatalities[1:5,1:10] ## just inspecting

#
# CREATING SEVERITY MEASURE FOR EACH HH
#


S <- matrix(0,1,4802)
We <- lapply(HH_vector, "[[", 2) ## extracting weights (inverse of distances) from HH_vector

for(n in 1:4802){
  
  s <- We[[n]] * nFatalities[n,] 
  S[n] <- sum(s, NA, na.rm = TRUE)
  
  cat('Household No: ', n,' - ',base::date(),'\n')
  
}


S[1:50] ## just inspecting the first 100 hh


## Conflict Measures per Household

weighted_events <- data.frame(do.call(rbind, lapply(HH_vector, colSums))) ## summation of weights

weighted_events <- weighted_events[-1]

total_events <- data.frame(sapply(HH_vector, NROW))
total_fatalities <- data.frame(rowSums(nFatalities))
hh.id <- data.frame(geodata.hh12[,6])
Severity <- data.frame(matrix(S, 4802, 1))

wave2_conflict_measure_20km <- data.frame(cbind(hh.id, Severity, weighted_events, total_events, total_fatalities))


names(wave2_conflict_measure_20km) <- c("hhid", "severity_conflict_20km", "dist_weighted_events_20km", "total_events_20km", "total_fatalities_20km")


save(wave2_conflict_measure_20km, file='wave2_conflict_measure_20km.RData')


rm(HH_vector, weighted_events, total_events, total_fatalities, Severity, nFatalities)



## List and object where the data will be stored 50Km

HH_vector <- lst()
id_events <- c()



for(h in 1:nrow(dist_matrix_wave2)){ ## Complete dataset
  # for(h in 1:5000){ ## Complete dataset
  hh <- c(dist_matrix_wave2[h,]) ## Selecting each Household (hh)
  events <- names(which(hh<=50)) ## selecting # of Events which met criteria
  distance <- unlist(hh[events]) ## Selecting the Distance for previous events
  names(distance) <- NULL
  for(i in seq_along(events)){ ## Capturing number of event
    id_events[i] <- strsplit(events,split = "_")[[i]][5]
  }
  ## Creating list with Events and Distances
  
  HH_vector[[h]] <- data.frame(idEvents = as.numeric(id_events), Distances = 1/(distance)^2)
  cat('Household No: ', h," - ", base::date(),'\n')
  id_events <- c()
}


rm(hh,h,i,distance,events,id_events) ## cleaning

gc() ## releasing memory

## Saving data
# save(HH_vector,file='List_HH_Events.RData')


#
# MATCHING WITH OTHER DATASET AND COUNTING FATALITIES ----
#


nFatalities <- matrix(0, nrow = 4802, ncol =  max(map_dbl(lapply(HH_vector,nrow),max))) ## Complete dataset


for (e in 1:4802){ ## complete dataset
  # for (e in 1:50){ ## sample dataset
  for(f in 1:ncol(nFatalities)){
    fa_vector <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    if(length(fa_vector) == ncol(nFatalities)){
      nFatalities[e,] <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    } else {
      di <- ncol(nFatalities) - length(fa_vector)
      fa_vector_ <- rbind(fa_vector,as.matrix(rep(0,di)))
      nFatalities[e,] <- fa_vector_
    }
    fa_vector <- c()
  }
  cat('Household No: ', e,' - ',base::date(),'\n')
}


# nFatalities
nFatalities[1:5,1:10] ## just inspecting

#
# CREATING SEVERITY MEASURE FOR EACH HH
#


S <- matrix(0,1,4802)
We <- lapply(HH_vector, "[[", 2) ## extracting weights (inverse of distances) from HH_vector

for(n in 1:4802){
  
  s <- We[[n]] * nFatalities[n,] 
  S[n] <- sum(s, NA, na.rm = TRUE)
  
  cat('Household No: ', n,' - ',base::date(),'\n')
  
}


S[1:50] ## just inspecting the first 100 hh


## Conflict Measures per Household

weighted_events <- data.frame(do.call(rbind, lapply(HH_vector, colSums))) ## summation of weights

weighted_events <- weighted_events[-1]

total_events <- data.frame(sapply(HH_vector, NROW))
total_fatalities <- data.frame(rowSums(nFatalities))
hh.id <- data.frame(geodata.hh12[,6])
Severity <- data.frame(matrix(S, 4802, 1))

wave2_conflict_measure_50km <- data.frame(cbind(hh.id, Severity, weighted_events, total_events, total_fatalities))


names(wave2_conflict_measure_50km) <- c("hhid", "severity_conflict_50km", "dist_weighted_events_50km", "total_events_50km", "total_fatalities_50km")


save(wave2_conflict_measure_50km, file='wave2_conflict_measure_50km.RData')


rm(HH_vector, weighted_events, total_events, total_fatalities, Severity, nFatalities)



## List and object where the data will be stored 100Km

HH_vector <- lst()
id_events <- c()



for(h in 1:nrow(dist_matrix_wave2)){ ## Complete dataset
  # for(h in 1:5000){ ## Complete dataset
  hh <- c(dist_matrix_wave2[h,]) ## Selecting each Household (hh)
  events <- names(which(hh<=100)) ## selecting # of Events which met criteria
  distance <- unlist(hh[events]) ## Selecting the Distance for previous events
  names(distance) <- NULL
  for(i in seq_along(events)){ ## Capturing number of event
    id_events[i] <- strsplit(events,split = "_")[[i]][5]
  }
  ## Creating list with Events and Distances
  
  HH_vector[[h]] <- data.frame(idEvents = as.numeric(id_events), Distances = 1/(distance)^2)
  cat('Household No: ', h," - ", base::date(),'\n')
  id_events <- c()
}


rm(hh,h,i,distance,events,id_events) ## cleaning

gc() ## releasing memory

## Saving data
# save(HH_vector,file='List_HH_Events.RData')


#
# MATCHING WITH OTHER DATASET AND COUNTING FATALITIES ----
#


nFatalities <- matrix(0, nrow = 4802, ncol =  max(map_dbl(lapply(HH_vector,nrow),max))) ## Complete dataset


for (e in 1:4802){ ## complete dataset
  # for (e in 1:50){ ## sample dataset
  for(f in 1:ncol(nFatalities)){
    fa_vector <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    if(length(fa_vector) == ncol(nFatalities)){
      nFatalities[e,] <- as.matrix(conflict_event_wave2[match(HH_vector[[e]][,1], conflict_event_wave2$event_id_no_cnty),'fatalities'])
    } else {
      di <- ncol(nFatalities) - length(fa_vector)
      fa_vector_ <- rbind(fa_vector,as.matrix(rep(0,di)))
      nFatalities[e,] <- fa_vector_
    }
    fa_vector <- c()
  }
  cat('Household No: ', e,' - ',base::date(),'\n')
}


# nFatalities
nFatalities[1:5,1:10] ## just inspecting

#
# CREATING SEVERITY MEASURE FOR EACH HH
#


S <- matrix(0,1,4802)
We <- lapply(HH_vector, "[[", 2) ## extracting weights (inverse of distances) from HH_vector

for(n in 1:4802){
  
  s <- We[[n]] * nFatalities[n,] 
  S[n] <- sum(s, NA, na.rm = TRUE)
  
  cat('Household No: ', n,' - ',base::date(),'\n')
  
}


S[1:50] ## just inspecting the first 100 hh


## Conflict Measures per Household

weighted_events <- data.frame(do.call(rbind, lapply(HH_vector, colSums))) ## summation of weights

weighted_events <- weighted_events[-1]

total_events <- data.frame(sapply(HH_vector, NROW))
total_fatalities <- data.frame(rowSums(nFatalities))
hh.id <- data.frame(geodata.hh12[,6])
Severity <- data.frame(matrix(S, 4802, 1))

wave2_conflict_measure_100km <- data.frame(cbind(hh.id, Severity, weighted_events, total_events, total_fatalities))


names(wave2_conflict_measure_100km) <- c("hhid", "severity_conflict_100km", "dist_weighted_events_100km", "total_events_100km", "total_fatalities_100km")


save(wave2_conflict_measure_100km, file='wave2_conflict_measure_100km.RData')
