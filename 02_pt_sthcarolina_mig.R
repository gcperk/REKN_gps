
# South Carolina

library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


raw_dat <- file.path("data", "movebank_locations_20231219")

filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "South Carolina"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
  dplyr::select(c(tag.id , "animal.id" , deploy.on.date, animal.life.stage, tag.model, animal.sex,
                  deployment.comments,tag.manufacturer.name )) %>% 
  rename("animal.ring.id" = animal.id,
         "animal.marker.id" = deployment.comments) %>%
  mutate(study.site = "Kiawah")


brep  <- brep [complete.cases(brep ), ]

brep <- brep %>%
  mutate(tag.local.identifier = tag.id)

btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                #gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, argos.altitude, #algorithm.marked.outlier,
                tag.local.identifier)

# calculate time differences
bout <- bout  %>% mutate(date_time = ymd_hms(timestamp)) 

# 
# 
# %>%
#   mutate(year = year(date_time )) %>%
#   mutate(month = month(date_time ),
#          day = day(date_time),
#          hour = hour(date_time),
#          minute = minute(date_time))

#no.tags <- unique(bout$tag.local.identifier)
#no.ids <- unique(bout$individual.local.identifier)

# 
# ids <- bout %>%
#   dplyr::select(tag.local.identifier, individual.local.identifier) %>%
#   group_by(tag.local.identifier, individual.local.identifier)%>%
#   summarise(n = n())
#   
# 
# ymin = min(range(bout$year))
# ymax = max(range(bout$year))


# merge these together and output 

all_dat <- left_join(bout, brep) %>%
  dplyr::mutate(argos.lc = as.character(argos.lc)) %>%
  dplyr::select(-individual.local.identifier, -tag.local.identifier)%>%
  mutate(tag.id = as.character(tag.id)) %>%
  filter(location.long >= -120,
         location.long <= -62) %>% 
  mutate(tag.model = "Sunbird Solar Argos") %>%
  filter(!is.na(location.long), 
         !is.na(location.lat)) %>%
  mutate(deploy.on.latitude = 32.53945, 
         deploy.on.longitude = -80.17069)%>%
  dplyr::filter(argos.lc != "Z")%>%
  dplyr::filter(argos.lc != "") 

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))


# #save out file
clean_save = all_dat  %>% mutate(proj = "sthcarolina_arctic")
saveRDS(clean_save, file = file.path("output", "rekn_sthcarolina_20240129.rds"))


# write out 
#clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path("output", "pt_sth_20240123.gpkg"), append = F)



#unique(all_dat$argos.lc)

#type_dur <-all_dat|> 
#  dplyr::select(tag.id, argos.lc) |> 
#  group_by(argos.lc, tag.id) |> 
#  summarise( n =  n())

##############################################################################
## Calculate the stopoverlocations: 
# 
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
# # 
# # type_dur <- bdd_dur |> 
# #   dplyr::select(tag.id, argos.lc, diff) |> 
# #   group_by(argos.lc, tag.id) |> 
# #   summarise( n =  n(), 
# #              min = min(diff, na.rm= T),
# #              max =max(diff, na.rm = T), 
# #              mean = mean(diff, na.rm = T))
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
# 
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
#   mutate(direction = case_when(
#     location.lat >= location.lat_prior ~ "northward", 
#     location.lat <= location.lat_prior~ "southward",
#     .default = "NA"))%>%
#   ungroup()

clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "pt_sth_20240123.gpkg"), append = F)

#