# atlantic shores & spring tags 

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


### Spring ##########################################

#raw_dat <- file.path("data", "movebank_locations_20230407")
raw_dat <- file.path("data", "movebank_locations_20231219")

filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "Spring Migration"

filesoi <- list.files(raw_dat, pattern = key)

filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]


# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  mutate(tag.local.identifier = tag.id)

# number of unique tags = no of ids 

btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status, 
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier, height.above.ellipsoid)%>%
  mutate(date_time = ymd_hms(timestamp))#%>%
#mutate(year = year(date_time )) %>%
#mutate(month = month(date_time ),
#       day = day(date_time),
#       hour = hour(date_time),
#       minute = minute(date_time))


all_dat <- left_join(bout, brep ) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z") %>%
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "THOMPS" ~ 39.20075,
    study.site == "NORBURYC" ~ 39.20075)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "THOMPS" ~ -75.0255,
    study.site == "NORBURYC" ~ -75.0255)) %>%
  dplyr::select(-tag.local.identifier, -individual.local.identifier, -height.above.ellipsoid) %>%
  dplyr::mutate(argos.lc = as.character(argos.lc))%>%
  mutate(tag.id = as.character(tag.id)) %>%
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75",
         tag.manufacturer.name = "Lotek")|> 
  filter(location.lat < 90 ) |> 
  filter(location.lat> -90)


head(all_dat)

spring_tags <- unique(all_dat$tag.id)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))

spring_all_dat <- all_dat


# # Duration between pings/ 
# bdd <- all_dat |> 
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
# head(bdd_det)
# # 
# 
# # stop over = within 25 km of previous point? 
# 
# bt <- bdd_det |> 
#   dplyr::select(id, tag.id, ddate, day, month, year,location.long, location.lat,location.lat_prior, diff, gcd_m,bearing, speed_mhr) %>%
#   group_by(tag.id)%>%
#   mutate(stopover = ifelse( gcd_m <= 25000, "stop-over", "migration")) %>%
#   mutate(breeding = case_when(
#     stopover == "stop-over" & month %in% c(6,7,8) & location.lat > 60 ~ "breeding", 
#     .default = "NA")) %>%
#  mutate(direction = case_when(
#     location.lat >= location.lat_prior ~ "northward", 
#     location.lat <= location.lat_prior~ "southward",
#     .default = "NA"))%>%
#   ungroup()
# 
# 
# idll <- bt|> 
#   dplyr::select("location.long", "location.lat", id)
# 
# 
# clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path("output", "pt_spring_20240123.gpkg"), append = F)
# 

# 
# 
# # #save out file
# # #save out file
# clean_save = all_dat %>% mutate(proj = "spring_USFWS")
# saveRDS(clean_save, file = file.path("output", "rekn_spring_usfws_20240101.rds"))
# 



#########################################################################

## Atlantic dataset



#raw_dat <- file.path("data", "movebank_locations_20230724")
#raw_dat <- file.path("data", "movebank_locations_20230918")
raw_dat <- file.path("data", "movebank_locations_20231219")
filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "Atlantic"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  mutate(tag.local.identifier = tag.id)

# read in track data

btemp <- read.csv(file.path(raw_dat, filesoi))

bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                algorithm.marked.outlier, argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, 
                tag.local.identifier)%>%
  mutate(date_time = ymd_hms(timestamp))

all_dat <- left_join(bout, brep )

head(all_dat)#2089


all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date, -algorithm.marked.outlier, -import.marked.outlier) %>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")

#unique(all_dat$animal.id)

library(stringr)

all_dat <- all_dat %>% 
  mutate(study.site1 = substr( animal.id, start = 1, stop = 4)) %>%
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site1 == "NBRI" ~ 39.436,
    study.site1 == "Thom" ~ 39.20075,
    study.site1 == "Norb" ~ 39.20075)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site1 == "NBRI" ~ -74.346,
    study.site1 == "Thom" ~ -75.0255,
    study.site1 == "Norb" ~ -75.0255))%>%
  mutate(study.site = study.site1) %>%
  dplyr::select(-study.site1)


spring_tag_data <- all_dat %>% 
  filter(tag.id %in% spring_tags)

at_tag <- all_dat %>%
  filter(!tag.id %in% spring_tags)
  
  
  
# # Nbrig latitutde/ longitide 
# Noth Brigatine 
# -74.346 39.436
# 
# 
# # Thom = thompson beach 
# -75.0255 , 39.20075
# 
# # norb 
#    -75.0255 , 39.20075


# #save out file
# #save out file
clean_save = at_tag %>% mutate(proj = "atlantic")
saveRDS(clean_save, file = file.path("output", "rekn_atlantic_20240129.rds"))


############################################################################


# merge the spring_tag from atlantic and stpring tag data together 

ospring <- bind_rows(spring_all_dat, spring_tag_data)
  
spring_tag_data 

spring_all_dat 

# #save out file
# #save out file
clean_save = ospring %>% mutate(proj = "spring_USFWS")
saveRDS(clean_save, file = file.path("output", "rekn_spring_usfws_20240129.rds"))

###############################################



# # generate output file for manual review for atlantic
# 
# out <- at_tag %>%
#   mutate(year = year(date_time )) %>%
#   mutate(month = month(date_time ),
#          day = day(date_time),
#          hour = hour(date_time),
#          minute = minute(date_time)) |> 
#   filter(location.lat < 90) |> 
#   filter(location.lat > -90) |> 
#   filter(location.long <180)
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
# st_write(clean_sf, file.path("output", "rekn_atlantic_20240207.gpkg"), append = F)
