# load data 

library(leaflet)
library(RColorBrewer)
library(dplyr)
library(lubridate)
library(sp)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)
library(foreach)


# download from movebank directly or files. 

raw_dat <- file.path("data", "movebank_locations_20230407")
filesoi <- list.files(raw_dat)

# data_set1 : atlantic
key = "Atlantic"

d1 <- list.files(raw_dat, pattern = key)





bdata <- foreach(x= filesoi, .combine = rbind) %do% {
  x <- filesoi[2]
  print(x)
  btemp <- read.csv(file.path(raw_dat, x))
  bout <- btemp %>%
    dplyr::select(visible, timestamp, end.timestamp, location.long, location.lat,
                  sensor.type, individual.taxon.canonical.name,
                  tag.local.identifier)
  bout
}




# data_set2 : spring migration 
key = "Spring Migration"

filesoi <- list.files(raw_dat, pattern = key)

bdata <- foreach(x= filesoi, .combine = rbind) %do% {
  x <- filesoi[2]
  print(x)
  btemp <- read.csv(file.path(raw_dat, x))
  bout <- btemp %>%
    dplyr::select(visible, timestamp, end.timestamp, location.long, location.lat,
                  sensor.type, individual.taxon.canonical.name,
                  tag.local.identifier)
  bout
}

"study.local.timestamp" , "utm.easting", "utm.northing", "utm.zone" ,
"study.timezone", "individual-taxon-canonical-name",  "tag.voltage",
"mortality.status" ,"lotek.crc.status.text", "import.marked.outlier",
"height.above.ellipsoid", "gps.fix.type.raw", "argos.nopc" ,                   
"argos.orientation",  "argos.pass.duration", "argos.semi.major",               
"argos.semi.minor" , "argos.sensor.1", "argos.sensor.2", "argos.sensor.3",                 
"argos.sensor.4", "argos.lon1", "argos.lon2"  , "argos.lat2", "argos.lat1" 


# filter 

"sensor.type"

"lotek.crc.status" %in% c("G", "C")
"argos.lc" %in% c("G", "3", '2', '1') # remove the "Z', 'B', 'A'

