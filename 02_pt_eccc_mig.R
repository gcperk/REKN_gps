# ECCC tags


library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


#raw_dat <- file.path("data", "movebank_locations_20230407")
#raw_dat <- file.path("data", "movebank_locations_20230724")
#raw_dat <- file.path("data", "movebank_locations_20231006")
raw_dat <- file.path("data", "movebank_locations_20231219")

filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "ECCC"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]



# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  # dplyr::select(c(tag.id , "animal.id" ,deploy.on.date, "animal.marker.id","animal.ring.id", "tag.serial.no", animal.life.stage, deploy.on.measurements,  animal.mass,  tag.model, 
  #                 tag.manufacturer.name, tag.serial.no))%>%
  mutate(tag.local.identifier = tag.id)

# read in the location data 
btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier, height.above.ellipsoid)%>%
  mutate(date_time = ymd_hms(timestamp))

all_dat <- left_join(bout, brep )

#head(all_dat)

all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date, -import.marked.outlier, -algorithm.marked.outlier, 
                -height.above.ellipsoid) %>%
  mutate(tag.id = as.character(tag.id)) %>%
  filter() %>% 
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")%>%
  filter(!is.na(location.long), 
         !is.na(location.lat))%>%
  mutate(tag.model = case_when(
    tag.model == "gps-pinpoint" ~ "Lotek PinPoint GPS-Argos 75", 
    tag.model == "PinPoint 75" ~ "Lotek PinPoint GPS-Argos 75", 
    tag.model ==  "Sunbird" ~ "Sunbird Solar Argos",
    tag.model ==  "sunbird" ~ "Sunbird Solar Argos",
    tag.model == "microwave telemetry"~ "Solar 2-g PTT",
    .default = as.character(tag.model))) %>%
  mutate(tag.manufacturer.name = case_when(
    tag.model == "Solar 2-g PTT" ~ "Microwave Telemetry", 
    TRUE ~ as.character(tag.manufacturer.name)))


#unique(all_dat$animal.id)

#"FORTESCU" "KIMBLESO" "MOORES"   "EASTPIT"  "PEIXE"    "NBRIG"   

all_dat <- all_dat %>% 
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "FORTESCU" ~ 39.0322,
    study.site == "KIMBLESO" ~ 39.20075,
    study.site == "MOORES" ~ 39.20075,
    study.site == "EASTPIT" ~ 39.1958,
    study.site == "PEIXE" ~ -31.401,
    study.site == "NBRIG" ~ 39.436)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "FORTESCU" ~ -74.7948,
    study.site == "KIMBLESO" ~ -75.0255,
    study.site == "MOORES" ~ -75.0255,
    study.site == "EASTPIT" ~ -75.02589,
    study.site == "PEIXE" ~  -51.066,
    study.site == "NBRIG" ~ -74.346))


# #save out file
# #save out file
clean_save = all_dat %>% mutate(proj = "ECCC")
#saveRDS(clean_save, file = file.path("output", "rekn_eccc_20240129.rds"))



# 
# 
# # generate output file for manual review
# 
# out <- all_dat %>%
#   mutate(year = year(date_time )) %>%
#   mutate(month = month(date_time ),
#          day = day(date_time),
#          hour = hour(date_time),
#          minute = minute(date_time))
# 
# 
# ##############################
# # durations
# 
# # Duration between pings/
# bdd <- out  |>
#   mutate(ddate = ymd_hms(date_time)) |>
#   arrange(tag.id, ddate)
# 
# bdd_dur <- bdd  |>
#   group_by(tag.id) |>
#   mutate(diff = difftime(ddate, lag(ddate),  units = c("hours")),
#          diff = as.numeric(diff))%>%
#   dplyr::filter(diff >0)
# 
# 
# ## Calculate distance between points and bearing
# 
# bdd_det <- bdd_dur  |>
#   #filter(tag.id == 230318) |>
#   group_by(tag.id) |>
#   mutate(location.long_prior = lag(location.long, 1L),
#          location.lat_prior = lag(location.lat, 1L)) %>%
#   rowwise() %>%
#   mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
#          bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
#          speed_mhr = round((gcd_m/diff)/1000,1))%>%
#   ungroup()
# 
# # determine the location of direction
# 
# #head(bdd_det)
# #stop over = within 25 km of previous point?
# 
# bt <- bdd_det |>
#   dplyr::select(tag.id, location.long, location.lat, date_time,  "gps.fix.type.raw",
#                 "lotek.crc.status", "argos.lc","year", "month", "day" , "hour", "minute",
#                 "diff" , "location.long_prior","location.lat_prior","gcd_m",
#                 "bearing" , "speed_mhr") %>%
#   group_by(tag.id)%>%
#   mutate(stopover = ifelse( gcd_m <= 25000, "stop-over", "migration")) %>%
#   mutate(breeding = case_when(
#     stopover == "stop-over" & month %in% c(6,7,8) & location.lat > 60 ~ "breeding",
#     .default = "NA")) %>%
#   mutate(direction = case_when(
#     location.lat >= location.lat_prior ~ "northward",
#     location.lat <= location.lat_prior~ "southward",
#     .default = "NA"))%>%
#   ungroup()
# 
# clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path("output", "rekn_eccc_20240207.gpkg"), append = F)
# 
# #


