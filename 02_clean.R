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


# define folder structire
data.dir <- file.path ("data")
processed.dir <- file.path("temp")
out.dir <- file.path("outputs")


filesoi <- list.files(file.path("data"), pattern = "^Red Knot", full.names = T
, recursive = T)


filesoi <- filesoi[2]

bdata <- foreach(x= filesoi, .combine = rbind) %do% {
   #x <- filesoi[1]
  print(x)
  btemp <- read.csv(x)
  bout <- btemp %>%
    dplyr::select(timestamp, location.long, location.lat,
                  sensor.type, lotek.crc.status.text, individual.taxon.canonical.name,
                  tag.local.identifier)
  bout
}


# calculate time difference
bdat_sp <- bdata %>%
  dplyr::mutate(arrive = ymd_hms(timestamp))%>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive)) %>%
  mutate(lat = as.numeric(location.lat), 
         lng = location.long) 

saveRDS(bdat_sp, file.path(processed.dir, "rekn_gps_locations.RDS"))

