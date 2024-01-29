
## Summary of Marsh Mispillion Harour DE

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)



#raw_dat <- file.path("data", "movebank_locations_20230407")
#raw_dat <- file.path("data", "movebank_locations_20230724")
raw_dat <- file.path("data", "movebank_locations_20231219")

filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "Mispillion Harbor, DE"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>%
    mutate(tag.local.identifier = tag.id,
         animal.marker.id = animal.id, 
         animal.id = str_c("MISP_", tag.id, "_2023")) %>% 
  mutate(animal.marker.id = str_sub(animal.marker.id, -9, -1), 
         tag.model = "Lotek PinPoint GPS-Argos 75",
         tag.manufacturer.name = "Lotek") 

btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                height.above.ellipsoid, import.marked.outlier,
                argos.lc, algorithm.marked.outlier,argos.altitude,
                height.above.ellipsoid, tag.local.identifier) %>%
  mutate(date_time = ymd_hms(timestamp))

# %>%
#   mutate(year = year(date_time )) %>%
#   mutate(month = month(date_time ),
#          day = day(date_time),
#          hour = hour(date_time),
#          minute = minute(date_time))


all_dat <- left_join(bout, brep )%>% 
  dplyr::select(-individual.local.identifier, 
                -deploy.off.date, - animal.taxon, -tag.local.identifier ) %>%
  mutate(argos.lc = as.character(argos.lc))%>%
  mutate(deploy.on.latitude = 39.20075, 
         deploy.on.longitude = -75.0255 , 
         study.site = "Thomp")%>%
  mutate(tag.id = as.character(tag.id))


head(all_dat)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))


# #save out file
clean_save = all_dat %>% mutate(proj = "Mispillion")
saveRDS(clean_save, file = file.path("output", "rekn_mispillion_20240129.rds"))


#clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(manual_edits, "rekn_mils_mig_20240123.gpkg"), append = F)






# 
# 
# 
# 
# # ##############################
# # # durations
# # 
# # # Duration between pings/ 
# # bdd <- all_dat |> 
# #   mutate(ddate = ymd_hms(date_time)) |> 
# #   arrange(tag.id, ddate)
# # 
# # bdd_dur <- bdd  |> 
# #   group_by(tag.id) |> 
# #   mutate(diff = difftime(ddate, lag(ddate),  units = c("hours")), 
# #          diff = as.numeric(diff))%>%
# #   dplyr::filter(diff >0)
# # 
# # 
# # ## Calculate distance between points and bearing
# # 
# # bdd_det <- bdd_dur  |> 
# #   #filter(tag.id == 230318) |> 
# #   group_by(tag.id) |> 
# #   mutate(location.long_prior = lag(location.long, 1L),
# #          location.lat_prior = lag(location.lat, 1L)) %>%
# #   rowwise() %>%
# #   mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
# #          bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
# #          speed_mhr = round((gcd_m/diff)/1000,1))%>%
# #   ungroup()
# # 
# # # determine the location of direction 
# # 
# # head(bdd_det)
# # 
# # 
# # # stop over = within 25 km of previous point? 
# # 
# # bt <- bdd_det |> 
# #   dplyr::select(id, tag.id, ddate, day, month, year,location.long, location.lat,location.lat_prior, diff, gcd_m,bearing, speed_mhr) %>%
# #   group_by(tag.id)%>%
# #   mutate(stopover = ifelse( gcd_m <= 25000, "stop-over", "migration")) %>%
# #   mutate(breeding = case_when(
# #     stopover == "stop-over" & month %in% c(6,7,8) & location.lat > 60 ~ "breeding", 
# #     .default = "NA")) %>%
# #   mutate(direction = case_when(
# #     location.lat >= location.lat_prior ~ "northward", 
# #     location.lat <= location.lat_prior~ "southward",
# #     .default = "NA"))%>%
# #   ungroup()
# # 
# # 
# # idll <- bt|> 
# #   dplyr::select("location.long", "location.lat", id)
# # 
# 
# clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path("output", "pt_misoillion_20240123.gpkg"), append = F)
# 
# # 
# 
# ## Post manual edits
# 
# manual_edits <- file.path("output", "manual_edited_complete")
# milsedit <- read.csv(file.path( manual_edits, "pt_mils_mig_all.csv")) %>%
#   left_join(idll) %>%
#   mutate(direction = case_when(
#     breeding == "wintering"  ~ NA, 
#     stopover == "breeding" ~ NA,
#     .default = as.character(direction)))%>% 
#   dplyr::filter(!is.na(location.lat)) %>%
#   dplyr::select(-tag.id, -ddate, -day, -month, -year, -"location.long", -"location.lat", -fid)
# 
# 
# head(milsedit)
# milout <- left_join(all_dat, milsedit, by = "id")
# 
# # #save out file
# clean_save = milout %>% mutate(proj = "Mispillion")
# saveRDS(clean_save, file = file.path("output", "rekn_mispillion_20240123.rds"))
# 
# 
# clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path(manual_edits, "rekn_mils_mig_20240123.gpkg"), append = F)
# 
# 
# ```