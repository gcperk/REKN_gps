
# Quebec updates 

library(lubridate)
library(sf)
library(stringr)
library(readxl)
library(dplyr)

raw_dat <- file.path("data", "other_dataset")

filesoi <- list.files(raw_dat)


#####################################################
# 1) Yves_Aubres 

qbirds <- read.csv(file.path(raw_dat, "aurey_yves", "redknotcan_6687_QuebecRawLotek.csv"))

qref <- read_xlsx(file.path(raw_dat, "aurey_yves", "ReferenceQuebec_2020_2023_ArgosDeployment26Jan2024.xlsx"), 
                   .name_repair = "universal") %>%
  mutate(tag.id = as.numeric(Tag.ID)) %>%
  filter(!is.na(tag.id)) %>%
  dplyr::select(-animal.id, -...23, -deploy.on.timestamp) 

qb <- qbirds |> 
  dplyr::select(Tag_ID, UTC_Date,  UTC_Time,  Latitude,  Longitude,  Location.Quality)

#length(qb$Tag_ID)

# calculate time differences
qb <- qb %>%
  mutate(date_time = ymd(UTC_Date),
         timestamp = str_c(UTC_Date, " ", UTC_Time))%>%
  mutate(date_time = ymd_hms(timestamp )) %>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         time = hms(UTC_Time),
         hour = hour(time),
         minute = minute(time)) %>% 
  dplyr::select(-time) %>%
  mutate(Tag_ID = gsub("6687:", "", Tag_ID)) %>%
  rename("tag.id" = Tag_ID,
         "location.lat" = Latitude,
         "location.long" = Longitude ,
         "argos.lc" = Location.Quality) %>%
  mutate(animal.id = str_c("MING_", tag.id ),
         tag.id = as.numeric(tag.id)) %>%
  dplyr::select(- UTC_Date, -UTC_Time, -day, -hour, -minute)


qout <- left_join(qb, qref, by = "tag.id")

# filter out Quebec 
qb <- qout |> 
  mutate(toremove = case_when(
    tag.id == 232347~ 1, # this is a upland sandpiper
    tag.id == 213948 ~ 1, 
    tag.id == 232341 ~ 1, 
    tag.id == 232342 ~ 1, 
    tag.id == 232341 ~ 1, 
    tag.id == 232342 ~ 1, 
    tag.id == 232344 ~ 1, 
    tag.id == 239414 ~ 1,
    tag.id == 242699 ~ 1,
    tag.id == 239423 & month == 7 ~ 1,
    tag.id == 239420 & month == 7 ~ 1,
    tag.id == 232346 & month == 5 ~1,
    tag.id == 239425 & month == 8 ~1,
    tag.id == 229370 & year == 2022 & month ==6 ~ 1,
    tag.id == 239414 & year == 2023 & month ==2 ~ 1,
    tag.id == 239413 & year == 2023 & month ==2 ~ 1,
    tag.id == 239409 & year == 2023 & month ==2 ~ 1,
    tag.id == 239408 & year == 2023 & month ==2 ~ 1,
    tag.id == 239412 & year == 2023 & month ==2 ~ 1,
    tag.id == 239411 & year == 2023 & month ==7 ~ 1,
    tag.id == 232345 & year == 2022 & month ==5~ 1,
    tag.id == 232351 & year == 2022 & month ==5~ 1,
    tag.id == 232352 & year == 2022 & month ==5~ 1,
    tag.id == 232353 & year == 2022 & month ==5~ 1,
    tag.id == 232350 & year == 2022 & month ==5~ 1,
    tag.id == 232348 & year == 2022 & month ==5~ 1,
    tag.id == 239408 & year == 2023 & month ==7~ 1,
    tag.id == 239409 & year == 2023 & month ==7~ 1,
    tag.id == 239410 & year == 2023 & month ==7~ 1,
    tag.id == 239421 & year == 2023 & month ==7~ 1,
    tag.id == 239412 & year == 2023 & month ==7~ 1,
    tag.id == 239413 & year == 2023 & month ==7~ 1,
    tag.id == 239415 & year == 2023 & month ==7~ 1,
    tag.id == 239416 & year == 2023 & month ==7~ 1,
    tag.id == 239417 & year == 2023 & month ==7~ 1,
    tag.id == 239418 & year == 2023 & month ==7~ 1,
    tag.id == 239422 & year == 2023 & month ==7~ 1,
    tag.id == 239419 & year == 2023 & month ==7~ 1,
    tag.id == 239424 & year == 2023 & month ==7~ 1,
    tag.id == 239424 & year == 2023 & month ==8~ 1,
    tag.id == 239425 & year == 2023 & month ==7~ 1,
    tag.id == 229364 & year == 2022 & month ==6~ 1,
    tag.id == 229363 & year == 2022 & month ==6~ 1,
    tag.id == 229366 & year == 2022 & month ==6~ 1,
    tag.id == 229368 & year == 2022 & month ==6~ 1,
    tag.id == 229369 & year == 2022 & month ==6~ 1,
    year == 2022 & month ==6~ 1,
    location.long > 160 ~ 1, 
    TRUE ~ NA))

qb <- qb %>% 
  filter(is.na(toremove)) %>% 
  dplyr::select(-toremove) 

qb <- qb %>% 
  filter(!is.na(location.long), 
         !is.na(location.lat))%>%
  mutate(tag.id = as.character(tag.id)) %>%
  dplyr::mutate(argos.lc = case_when(
    is.na(argos.lc) ~ "",
    TRUE ~ as.character(argos.lc)))%>%
  dplyr::filter(argos.lc != "Z") 

all_dat <- qb  %>%
  mutate(id = seq(1, length(qb$tag.id), 1)) %>%
  dplyr::select(-year, -month) %>%
  filter(!is.na(location.lat))%>%
  dplyr::mutate(tag.mass = as.numeric(tag.mass))%>% 
  mutate(tag.model = case_when(
    tag.model == "lotek PinPoint 75" ~ "Lotek PinPoint GPS-Argos 75",
    tag.model == "Sunbird Lotek" ~ "Sunbird Solar Argos",
    #tag.model == "PTT solarLotek" ~ "Solar 2-g PTT",
    TRUE ~ as.character(tag.model))) %>%
  dplyr::select(-old.tag.id, -Tag.ID)%>%
  filter(animal.taxon == "Calidris canutus") |> 
  rename("animal.comments" = comment.for.not.working)




# #save out file
clean_save = all_dat %>% mutate(proj = "Mingnan")
saveRDS(clean_save, file = file.path("output", "rekn_mignon_raw_20240129.rds"))


#clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(manual_edits, "rekn_qu_20240128.gpkg"), append = F)




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
#   #dplyr::select(id, tag.id, ddate, day, month, year,location.long, location.lat,location.lat_prior, diff, gcd_m,bearing, speed_mhr) %>%
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
# st_write(clean_sf, file.path("output", "pt_ming_20240123.gpkg"), append = F)
# 
# # 
# 
# 
# 
# ## Post manual edits
# 
# manual_edits <- file.path("output", "manual_edited_complete")
# quedit <- read.csv(file.path( manual_edits, "pt_qu_all2.csv")) %>%
#   mutate(tag.id = as.character(tag.id)) |> 
#   #left_join(idll) %>%
#   mutate(direction = case_when(
#     breeding == "wintering"  ~ NA, 
#     stopover == "breeding" ~ NA,
#     .default = as.character(direction)))
# 
# quedit <- left_join(quedit, idll)
# 
# #head(qb)
# 
# qb <- qb %>% 
#   filter(is.na(toremove)) %>% 
#   dplyr::select(-toremove) %>%
#   dplyr::select(-tag.id, -ddate, -day, -month, -year, -"location.long", -"location.lat")
# 

# 
# head(qb)
# head(all_dat)
# 
# 
# sort(names(qb))
# 
# qb <- qb %>%
#   dplyr::select("id", animal.id, animal.life.stage, animal.marker.id, animal.mass, 
#                 animal.ring.id, animal.sex,  animal.taxon,  argos.lc, 
#                 bearing, breeding, capture.timestamp, date_time, 
#                 deploy.on.date ,"deploy.on.latitude" ,    "deploy.on.longitude" ,   "deploy.on.measurements",
#                 "deploy.on.person", "deployment.comment" , "diff" ,"direction" ,  "fid" ,
#                 "gcd_m" ,  "location.lat_prior" ,"location.long_prior" ,
#                 "speed_mhr" , "stopover" , "study.site","tag.beacon.frequency" ,"tag.manufacturer.name" ,
#                 "tag.mass" , "tag.model" ,"tag.serial.no" , "timestamp" )
# 
# 
# 
# sort(names(all_dat))
# 
# all_dat <- all_dat %>%
#   dplyr::select("id","tag.id" ,  "minute",  "hour", "day", "month", "year",  
#                 "location.lat", "location.long")                                      
# 
# quout <- left_join(qb,all_dat, by = "id") %>%
#   dplyr::select(-fid) %>%
#   mutate(date_time = ymd_hms(date_time)) %>%
#   filter(!is.na(location.lat))
# 
# 
# # #save out file
# clean_save = quout %>% mutate(proj = "Mingnan")
# saveRDS(clean_save, file = file.path("output", "rekn_mignon_raw_20240128.rds"))
# 
# 
# clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path(manual_edits, "rekn_qu_20240128.gpkg"), append = F)
# 

