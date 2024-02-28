
#library(leaflet)
#library(RColorBrewer)
 
library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)
library(geosphere)


raw_dat <- file.path("output")

filesoi <- list.files(raw_dat)

spring <- readRDS(file.path(raw_dat, "rekn_spring_usfws_20240129.rds")) #### WAITING ON MOVEBANK
at <- readRDS(file.path(raw_dat, "rekn_atlantic_20240129.rds")) #### WAITING ON MOVEBANK
eccc <- readRDS(file.path(raw_dat, "rekn_eccc_20240129.rds")) #### WAITING ON MOVEBANK

ma <- readRDS(file.path(raw_dat, "rekn_ma_mig_20240129.rds" ))        # edited for directions
dom <- readRDS(file.path(raw_dat, "rekn_dominion_20240129.rds"))      # edited for directions
oc <- readRDS(file.path(raw_dat, "rekn_ocean_20240129.rds"))          # edited for directions
mils <- readRDS(file.path(raw_dat, "rekn_mispillion_20240129.rds"))   # edited for directions
sth <- readRDS(file.path(raw_dat, "rekn_sthcarolina_20240129.rds"))   # edited for directions
qu <- readRDS(file.path(raw_dat, "rekn_mignon_raw_20240129.rds"))     # edited for directions
john <- readRDS(file.path(raw_dat, "rekn_john_20240129.rds"))         # edited for directions
new <- readRDS(file.path(raw_dat, "rekn_newstead_20240129.rds"))      # edited for directions

all <- bind_rows(ma, dom, oc, mils, sth, qu, john, new, eccc, at, spring) 

#all <- bind_rows(ma, john)
unique(all$proj)

unique(all$algorithm.marked.outlier)
unique(all$visible)
unique(all$import.marked.outlier)

unique(all$study.site)
# write out: 
#write.csv(all, file.path("output", "xall_rekn_20240203.csv"), row.names = F)
##clean_sf <- st_as_sf(all, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path("output", "xall_rekn_20240203.gpkg"), append = F)


#######################################
# add the year, month, day values 

out <- all %>%
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time)) %>%
  filter(year<=2023) %>%
  arrange(date_time, by_group = tag.id)

out <- out %>%
  group_by(tag.id) |> 
  mutate(diff = difftime(date_time, lag(date_time),  units = c("hours")), 
         diff = as.numeric(diff)) 
  

############################################################################
## Calculate distance between points and bearing

bdd_det <- out  |> 
  #filter(tag.id == 230318) |> 
  group_by(tag.id) |> 
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L))

bdd_det <- bdd_det |> 
  rowwise() %>%
  dplyr::mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         speed_mhr = round((gcd_m/diff)/1000,1))

# 
# # we can see a big range in the time intervals for the fixes
# range(bdd_det$diff, na.rm = TRUE)
# 
# ggplot(bdd_det, aes(tag.id, diff)) +
#   geom_col(aes(fill = tag.model))#, position = position_stack(reverse = TRUE))


# write out: 
#write.csv(bdd_det, file.path("output", "xall_rekn_20240207.csv"), row.names = F)
clean_sf <- st_as_sf(all, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "xall_rekn_20240207.gpkg"), append = F)


