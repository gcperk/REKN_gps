## Summary of Ocean Winds

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
key = "Red Knot Fall Migration"

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
                argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier,
                height.above.ellipsoid)%>%
  mutate(date_time = ymd_hms(timestamp))

# 
# %>%
#   mutate(year = year(date_time )) %>%
#   mutate(month = month(date_time),
#          day = day(date_time),
#          hour = hour(date_time),
#          minute = minute(date_time))

# no.tags <- unique(bout$tag.local.identifier)
# no.ids <- unique(bout$individual.local.identifier)
# 
# ids <- bout %>%
#   dplyr::select(tag.local.identifier, individual.local.identifier) %>%
#   group_by(tag.local.identifier, individual.local.identifier)%>%
#   summarise(n = n())
#   
# ymin = min(range(bout$year))
# ymax = max(range(bout$year))

###########################
# 
# brep 
# bout

all_dat <- left_join(bout, brep )

all_dat <- all_dat |> 
  mutate(tag.id = as.character(tag.id)) %>%
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75", 
         tag.manufacturer.name="Lotek") %>%
  filter(!is.na(location.long), 
         !is.na(location.lat)) %>%
  dplyr::mutate(lotek.crc.status = case_when(
    is.na(lotek.crc.status) ~ "",
    TRUE ~ as.character(lotek.crc.status)))%>%
  dplyr::filter(lotek.crc.status != "E")%>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z")


all_dat <- all_dat %>% 
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "TWOMILE" ~ 39.0277,
    study.site == "STOHARB" ~ 39.0277,
    study.site == "AVALON" ~ 39.0979,
    study.site == "NBRIG" ~ 39.436)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "TWOMILE" ~ -74.7808,
    study.site == "STOHARB" ~ -74.7808,
    study.site == "AVALON" ~ -74.7138,
    study.site == "NBRIG" ~ -74.346))


head(all_dat)

all_dat <- all_dat %>% 
  dplyr::select(-individual.local.identifier, -tag.local.identifier, -animal.taxon, 
                -deploy.off.date)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$visible), 1))

#save out file
clean_save = all_dat %>% mutate(proj = "OceanWinds")
saveRDS(clean_save, file = file.path("output", "rekn_ocean_20240129.rds"))

#clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(manual_edits, "rekn_oceanwinds_20240123.gpkg"), append = F)








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
# st_write(clean_sf, file.path("output", "pt_ow_20240123.gpkg"), append = F)
# 
# 
# 
# ## Post manual edits
# 
# manual_edits <- file.path("output", "manual_edited_complete")
# owedit <- read.csv(file.path( manual_edits, "pt_ow_mig_all.csv")) %>%
#   left_join(idll) %>%
#   mutate(direction = case_when(
#     breeding == "unknown"  ~ NA,
#     breeding == "wintering"  ~ NA,
#     stopover == "breeding" ~ NA,
#     .default = as.character(direction)))%>%
#   dplyr::filter(!is.na(location.lat)) %>%
#   dplyr::select(-tag.id, -ddate, -day, -month, -year, -"location.long", -"location.lat", -fid)
# 
# 
# head(milsedit)
# milout <- left_join(all_dat, owedit,  by = "id")
# 
# #save out file
# clean_save = milout%>% mutate(proj = "OceanWinds")
# saveRDS(clean_save, file = file.path("output", "rekn_ocean_20240123.rds"))
# 
# clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path(manual_edits, "rekn_oceanwinds_20240123.gpkg"), append = F)
# 
# 
# ```
# 
# This dataset has `r length(no.tags)` tags (tag.local.identifier) and `r length(no.ids)` (individual.local.identifier). 
# 
# 
# For the reference data there are `r length(no.tags.ref)` tags and `r length(no.ids.ref)` (individual.local.identifier). 
# 
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
# #length(clean$visible)
# 
# cleanargos <- clean %>% 
#   dplyr::filter(argos.lc != "Z")%>%
#   dplyr::filter(argos.lc != "A")%>% 
#   dplyr::filter(argos.lc != "B")
# 
# 
# #length(cleanargos$visible)
# 
# clean = cleanargos
# 
# # #save out file
# clean_save = clean %>% mutate(proj = "OceanWinds")
# saveRDS(clean_save, file = file.path("output", "rekn_ocean_20231102.rds"))
# 
# 
# ```
# 
# 
# # Raw data
# 
# The number of records as of September 2023 is `r length(bout$tag.local.identifier)`.  The raw date range varies from 
# `r min(range(bout$year))` to `r max(range(bout$year))`. 
# 
# If we remove all the algorithm marked outliers (lotek = error, argos accuracy value = A, B, Z). We reduce the number of observations in the data set from `r length(bout$tag.local.identifier)` to `r length(clean$tag.local.identifier)`.
# 
# - Class A: Argos no accuracy estimation (3 messages received)
# - Class B: Argos no accuracy estimation (1 or 2 messages received)
# - Class Z: Argos Invalid Location
# 
# The cleaned date range varies from 
# `r min(range(clean$year))` to `r max(range(clean$year))`.
# 
# 
# # GPS vs argos
# 
# The data split between gps and argos data records. 
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
# p_alldat_all <- ggplot(clean, aes(year, fill = sensor.type))+
#   geom_bar(position = "dodge") +
#   xlim(2020, 2023)
# 
# p_alldat <- ggplot(clean, aes(year, fill = sensor.type))+
#   geom_bar(position = "dodge") +
#   xlim(2020, 2023)+
#   facet_wrap(~tag.local.identifier, scales = "free_y")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ```
# 
# 
# 
# ```{r,echo=FALSE}
# p_alldat_all
# p_alldat 
# 
# ```
# 
# 
# # Duration of Tag summary
# 
# ```{r, echo = FALSE}
# # 
# # p_duration <- ggplot(clean, aes(factor(month), fill = factor(year)))+
# #   geom_bar(position = "dodge") +
# #   #xlim(1,12)+
# #   facet_wrap(~tag.local.identifier)+
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# # 
# # p_duration
# 
# table_max <- clean %>% 
#   dplyr::select(individual.local.identifier, date_time) %>%
#   # group_by(tag.local.identifier)%>% 
#   slice_max(date_time, by = individual.local.identifier)%>%
#   unique()
# colnames(table_max)<- c("individual.local.identifier", "max")
# 
# table_min <- clean %>% 
#   dplyr::select(individual.local.identifier, date_time) %>%
#   # group_by(tag.local.identifier)%>% 
#   slice_min(date_time, by = individual.local.identifier) %>%
#   unique()
# colnames(table_min)<- c("individual.local.identifier", "min")
# 
# dur <- left_join(table_max, table_min,by = join_by(individual.local.identifier)) %>%
#   distinct() %>%
#   dplyr::mutate(duration = max- min)%>%
#   mutate(dur_days = round(as.numeric(duration),1))%>%
#   mutate(year = year(min))
# 
# dur_data = dur
# 
# 
# dur_plot <- ggplot(dur, aes(y=factor(individual.local.identifier))) +
#   geom_segment(aes(x=min, xend=max, y=factor(individual.local.identifier),    yend=factor(individual.local.identifier)), size=1)+
#   xlab("Date") + ylab("Tag") 
# 
# #dur_plot
# 
# dur_hist <- ggplot(dur, aes(dur_days))+
#   geom_histogram() +
#   xlab("duration (days)") 
# 
# #dur_hist
# 
# p_duration <- ggplot(clean, aes(factor(month), fill = factor(year)))+
#   geom_bar(position = "dodge") +
#   #xlim(1,12)+
#   facet_wrap(~individual.local.identifier)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# #p_duration
# 
# 
# 
# ```