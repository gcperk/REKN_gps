# 02 ma migration 

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
key = "Red Knot Fall MA Migration Study"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))

brep <- brep %>%
  mutate(tag.local.identifier = tag.id)


btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier, gps.fix.type.raw, 
                import.marked.outlier,
                height.above.ellipsoid) %>%
  mutate(date_time = ymd_hms(timestamp))

all_dat <- left_join(bout, brep )%>%
  mutate(argos.lc = as.character(argos.lc)) %>%
  rename("animal.marker.id" = animal.id)%>%
  dplyr::select(-attachment.type, -animal.taxon.detail)%>%
  mutate(tag.id = as.character(tag.id))%>%
  mutate(animal.id = deployment.id)

head(all_dat)

all_dat <- all_dat %>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")%>%
  filter(!is.na(location.long), 
         !is.na(location.lat)) %>%
  mutate(deploy.on.latitude = 39.20075, 
         deploy.on.longitude = -75.0255,
         study.site = "THOMP")

all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date, - deployment.id, -animal.comments)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))


# #save out file
clean_save = all_dat %>% mutate(proj = "ma_migration")
saveRDS(clean_save, file = file.path("output", "rekn_ma_mig_20240129.rds"))


clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "pt_MAmig_20240129.gpkg"), append = F)








# 
# 
# 
# 
# 
# 
# 
# 
# %>%
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
# 
# 
# idll <- bt|> 
#   dplyr::select("location.long", "location.lat", id)
# 
# 
# clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path("output", "pt_MAmig_20240123.gpkg"), append = F)
# 
# # 
# 
# ## Post manual edits
# 
# manual_edits <- file.path("output", "manual_edited_complete")
# maedit <- read.csv(file.path( manual_edits, "pt_ma_mig_all.csv")) %>%
#   left_join(idll) %>%
#   mutate(direction = case_when(
#     breeding == "wintering"  ~ NA, 
#     stopover == "breeding" ~ NA,
#     .default = as.character(direction)))%>% 
#   dplyr::filter(!is.na(location.lat)) %>%
#   dplyr::select(-tag.id, -ddate, -day, -month, -year, -"location.long", -"location.lat")
# 
# 
# #head()
# 
# maout <- left_join(all_dat, maedit, by = "id")%>%
#   dplyr::select(-fid)
# 
# 
# # #save out file
# clean_save = maout %>% mutate(proj = "ma_migration")
# saveRDS(clean_save, file = file.path("output", "rekn_ma_mig_20240123.rds"))
# 
# 
# clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path(manual_edits, "rekn_ma_mig_20240123.gpkg"), append = F)
# 
# 
# ```
# 
# This dataset has `r length(no.tags)` tags (tag.local.identifier) and `r length(no.ids)` (individual.local.identifier). 
# 
# ```{r, echo = FALSE}
# knitr::kable(ids)
# 
# ```
# 
# ```{r, echo=FALSE}
# 
# clean <- bout %>%
#   filter(visible == "true")%>%
#   filter(!is.na(location.long)) %>%
#   filter(!is.na(location.lat))
# 
# 
# cleanargos <- clean %>% 
#   dplyr::filter(argos.lc != "Z")%>%
#   dplyr::filter(argos.lc != "A")%>% 
#   dplyr::filter(argos.lc != "B")
# 
# #length(cleanargos$visible)
# 
# clean = cleanargos
# 
# errrors_tble <- clean %>%
#   group_by(tag.local.identifier, year, visible) %>%
#   summarise(count = n())
# 
# 
# # #save out file
# clean_save = clean %>% mutate(proj = "ma_migration")
# saveRDS(clean_save, file = file.path("output", "rekn_ma_mig_20231102.rds"))
# 
# 
# 
# ```
# 
# 
# # Raw data
# 
# The number of raw records as of September 2023 is `r length(bout$tag.local.identifier)`.  The raw date range varies from 
# `r min(range(bout$year))` to `r max(range(bout$year))`.
# 
# 
# If we remove all the algorithm marked outliers (lotek = error, argos accuracy value = A, B, Z). We reduce the number of observations in the data set from `r length(bout$tag.local.identifier)` to `r length(clean$tag.local.identifier)`.
# Argos accuracy codes are: 
#   
#   - Class 0: over 1500 m radius (not currently filtered)
# 
# - Class A: Argos no accuracy estimation (3 messages received)
# - Class B: Argos no accuracy estimation (1 or 2 messages received)
# - Class Z: Argos Invalid Location
# 
# 
# The cleaned date range varies from 
# `r min(range(clean$year))` to `r max(range(clean$year))`.
# 
# 
# # GPS vs argos
# 
# 
# The data split between gps and argos data records. Large proportion are argos doppler shift.   These represent two different methods of calculating the locations and all types can be used. 
# 
# ```{r, echo = FALSE, message=FALSE}
# tag_type <- clean %>%
#   dplyr::select(tag.local.identifier, sensor.type) %>%
#   group_by(tag.local.identifier, sensor.type)%>%
#   summarise(n = n())
# 
# #tag_type
# 
# tag_type_date <- clean %>%
#   dplyr::select(tag.local.identifier, sensor.type, year, month) %>%
#   group_by(tag.local.identifier, sensor.type, year,month)%>%
#   summarise(n = n())
# 
# #tag_type_date
# 
# #p_alldat <- ggplot(clean, aes(year, fill = sensor.type))+
# #  geom_bar(position = "dodge") 
# 
# p_alldat <- ggplot(clean, aes(year, fill = sensor.type))+
#   geom_bar(position = "dodge") +
#   xlim(2021, 2024)+
#   facet_wrap(~tag.local.identifier)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
