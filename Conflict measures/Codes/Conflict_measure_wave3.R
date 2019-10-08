######################
#
#  Wave 3
#
#######################

library(dplyr)
library(lubridate)
library(purrr)

#
# LOADING DATA -----
#


## Events dataset

conflict_event <- read.csv("Data/ACLED/Nigeria/data/1900-01-01-2019-04-16-Nigeria.csv")
conflict_event$event.date <- as.Date(conflict_event$event_date, format="%d %B %Y")

conflict_event_wave3 <- conflict_event %>% filter(event.date >= "2014-09-01" & event.date <= "2016-04-30")


geodata.hh10 <- read.csv("Data/LSMS/hh10/data/Geodata/nga_householdgeovariables_y1.csv")

geodata.hh15 <- read.csv("Data/LSMS/hh15/data/Geodata/nga_householdgeovars_y3.csv")

## Distance matrix
### load distance matrix

load("Benjamin workspace/Conflict Distance/hh12_dist_matrix_wave3.RData")

dist_matrix_wave3 <- hh15_dist_matrix %>% select(event_id_no_cnty_6369:event_id_no_cnty_9094, 
                                                       event_id_no_cnty_12403, event_id_no_cnty_12404,
                                                       event_id_no_cnty_13059, event_id_no_cnty_13818)

rm(hh10_dist_matrix)
rm(hh15_dist_matrix)



#
# SELECTING EVENTS -----
#

## List and object where the data will be stored

HH_vector <- lst()
id_events <- c()



for(h in 1:nrow(dist_matrix_wave3)){ ## Complete dataset
  # for(h in 1:5000){ ## Complete dataset
  hh <- c(dist_matrix_wave3[h,]) ## Selecting each Household (hh)
  events <- names(which(hh<100)) ## selecting # of Events which met criteria
  distance <- unlist(hh[events]) ## Selecting the Distance for previous events
  names(distance) <- NULL
  for(i in seq_along(events)){ ## Capturing number of event
    id_events[i] <- strsplit(events,split = "_")[[i]][5]
  }
  ## Creating list with Events and Distances
  
  HH_vector[[h]] <- data.frame(idEvents = as.numeric(id_events), Distances = 1/distance)
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


nFatalities <- matrix(0, nrow = 4613, ncol =  max(map_dbl(lapply(HH_vector,nrow),max))) ## Complete dataset


for (e in 1:4613){ ## complete dataset
  # for (e in 1:50){ ## sample dataset
  for(f in 1:ncol(nFatalities)){
    fa_vector <- as.matrix(conflict_event_wave3[match(HH_vector[[e]][,1], conflict_event_wave3$event_id_no_cnty),'fatalities'])
    if(length(fa_vector) == ncol(nFatalities)){
      nFatalities[e,] <- as.matrix(conflict_event_wave3[match(HH_vector[[e]][,1], conflict_event_wave3$event_id_no_cnty),'fatalities'])
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


S <- matrix(0,1,4613)
We <- lapply(HH_vector, "[[", 2) ## extracting weights (inverse of distances) from HH_vector

for(n in 1:4613){
  
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
hh.id <- data.frame(geodata.hh15[,6])
Severity <- data.frame(matrix(S, 4613, 1))

wave3_conflict_measure <- data.frame(cbind(hh.id, Severity, weighted_events, total_events, total_fatalities))


names(wave3_conflict_measure) <- c("hh_id", "severity_conflict", "dist_weighted_events", "total_events", "total_fatalities")

save(wave3_conflict_measure, file='wave3_conflict_measure.RData')

