
#####################################################
# 2)  Johnson 2017 and 2018 

## Summary of  Dominion data set 
library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)

raw_dat <- file.path("data", "other_dataset")

filesoi <- list.files(raw_dat)



j17 <- read.csv(file.path(raw_dat, "johnson", "REKN_GPSprocessed_2017.csv")) %>% 
  dplyr::mutate(Lat = as.numeric(Lat), 
                Long = as.numeric(as.character(Long)),
                Long_new = as.numeric(Long_new), 
  )

j18 <- read.csv(file.path(raw_dat, "johnson", "REKN_GPSprocessed_2018.csv"))


jd <- bind_rows(j17, j18)%>%
  #   filter(CRC %in% c(NA , "OK")) %>%
  #   filter(LocType_new != "Bad") %>%
  rename("animal.id" = FlagID,
         "location.lat" = Lat,
         "location.long" = Long) %>%
  mutate(proj ="Johnson_GPS",
         data_type = LocType)

# Basic summary of individuals

jgps <- jd %>%
  mutate(arrive = mdy(Date)) %>%
  mutate(year = year(arrive), 
         month = month(arrive), 
         day = day(arrive)) |> 
  rename("animal.ring.id" = Band,
         "animal.mass" = Mass,
         "lotek.crc.status" = CRC, 
         "sensor.type"  = data_type,
         "gps.fix.type.raw" = Accuracy,
         "tag.id" = TagID,
         "animal.sex" = Sex,
         "animal.comments" = Note) %>%
  dplyr::select(-Cohort, -Sequence, -Site, -Period)%>%
  mutate(Time = case_when(
    is.na(Time) ~ "11:00:00",
    Time == "na" ~"11:00:00",
    TRUE ~ as.character(Time))) %>%
  mutate(Time2 = hms(Time),
         animal.ring.id = as.character(animal.ring.id)) %>%
  mutate(hour = hour(Time2), 
         minute= minute(Time2))%>%
  mutate(timestamp = str_c(as.character(arrive), " ", as.character(Time)))%>%
  mutate(date_time = ymd_hms(timestamp)) 


jgps <- jgps %>%
  mutate(sensor.type = case_when(
    sensor.type %in% c("Doppler", "Doppler ") ~ "argos-doppler-shift", 
    sensor.type == "GPS" ~ "gps", 
    sensor.type == "deploy" ~ "deploy", 
    sensor.type == "3D" ~ NA, 
    sensor.type == "A3" ~ NA, 
    sensor.type == "AB" ~ NA,
    sensor.type == "Resight"  ~ "resight", 
    .default = NA
  ))



jgps  <- jgps  |> 
  mutate(toremove = case_when(
    tag.id == 175067 & year == 2023 & month ==5 ~ 1,
    tag.id == 175071 & year == 2022 & month ==4 ~ 1,
    tag.id == 175077 & year == 2019 & month ==1 ~ 1,
    tag.id == 175068 & year == 2018 & month ==12 ~ 1,
    TRUE ~ NA))


head(jgps)

jgps <- jgps %>% 
  filter(is.na(toremove)) %>% 
  dplyr::select(-toremove) |> 
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75", 
         tag.manufacturer.name = "Lotek" )%>%
  mutate(tag.id = as.character(tag.id))%>%
  mutate(sensor.type = tolower(sensor.type)) %>% 
  mutate(deploy.on.latitude = 46.95371, 
         deploy.on.longitude = -124.043211, 
         study.site = "SandIs")


# needs to be fixed here 
deploy_dates <- jgps %>%
  dplyr::select(tag.id, date_time, LocType) %>%
  filter(LocType == "Deploy") %>% 
  mutate(deploy.on.date = date_time) %>% 
  dplyr::select(-LocType,-date_time)


jgps <- left_join(jgps , deploy_dates)%>%
  mutate(tag.id = as.character(tag.id))%>%
  mutate(sensor.type = tolower(sensor.type), 
         deploy.on.date = as.character(deploy.on.date))


#str(jgps)
jgps <- jgps |> 
  dplyr::select(-TagID2, -LocType_new, -Corr, -Time2, -Time, 
                -Long_new, -LocType_new, -LocType, - Date, -arrive, - month, -day, -hour, -minute, -year)%>% 
  filter(!is.na(location.long), 
         !is.na(location.lat))


all_dat <- jgps |> 
   mutate(date_time = ymd_hms(timestamp))|> 
  filter(location.lat < 90 ) |> 
  filter(location.lat> -90) 

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$tag.id), 1))



## Re edit the deploy date
deploy_dates <- all_dat %>%
  dplyr::select(tag.id, date_time) %>% 
  arrange(date_time,  by = tag.id) %>% 
  slice_min(date_time, by = tag.id) %>% 
  rename("deploy.on.date" = date_time) %>% 
  distinct()%>%
  mutate(deploy.on.date = as.character(deploy.on.date))


all_dat <- left_join(all_dat, deploy_dates)

# #save out file
saveRDS(all_dat, file = file.path("output", "rekn_john_20240129.rds"))

#clean_sf <- st_as_sf(johnout, coords = c("location.long", "location.lat"), crs = st_crs(4326))
#st_write(clean_sf, file.path(manual_edits, "rekn_john_mig_20240123.gpkg"), append = F)
# 
# clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
# st_write(clean_sf, file.path("output", "pt_john_20240123.gpkg"), append = F)





# ## Post manual edits
# 
# manual_edits <- file.path("output", "manual_edited_complete")
# johnedit <- read.csv(file.path( manual_edits, "pt_john_mig_all.csv")) %>%
#   mutate(tag.id = as.character(tag.id)) |> 
#   left_join(idll) %>%
#   mutate(direction = case_when(
#     breeding == "wintering"  ~ NA, 
#     stopover == "breeding" ~ NA,
#     .default = as.character(direction)))%>% 
#   dplyr::filter(!is.na(location.lat)) %>%
#   dplyr::select(-tag.id, -ddate, -day, -month, -year, -"location.long", -"location.lat")
# 
# head(johnedit)
# 
# johnedit <- johnedit %>% 
#   dplyr::select(-"animal.id" , -animal.ring.id, -animal.sex,  -deploy.on.date,
#                 -"deploy.on.latitude", -"deploy.on.longitude", -gps.fix.type.raw, -"hour",
#                 -lotek.crc.status, -minute, -proj, -sensor.type, -study.site, 
#                 -tag.manufacturer.name, -tag.model, -timestamp, -Event1 )
# 
# 
# all_dat <- all_dat %>% dplyr::select(id, tag.id, day, month, year, ddate, location.lat, location.long, proj)
# 
# 
# johnout <- left_join(all_dat, johnedit, by = "id")%>%
#   dplyr::select(-fid)%>% 
#   mutate(date_time = ymd_hms(date_time))

# #save out file
#saveRDS(johnout, file = file.path("output", "rekn_john_20240123.rds"))
#saveRDS(jgps , file = file.path("output", "rekn_johnson_raw_20240101.rds"))

clean_sf <- st_as_sf(johnout, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(manual_edits, "rekn_john_mig_20240123.gpkg"), append = F)


