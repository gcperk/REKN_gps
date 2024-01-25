# Compile al the other pieces of data - not curretnly on movebank format 

## 


#library(leaflet)
#library(RColorBrewer)
library(lubridate)
library(sf)
#library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)
library(geosphere)

raw_dat <- file.path("data", "other_dataset")

filesoi <- list.files(raw_dat)


#####################################################
# 1) Yves_Aubres 

qbirds <- read.csv(file.path(raw_dat, "aurey_yves", "redknotcan_6687_QuebecRawLotek.csv"))
qref <- read_xlsx(file.path(raw_dat, "aurey_yves", "ReferenceQiuebec_2020_2023_ArgosDeployment.xlsx"), 
                  .name_repair = "universal") %>%
  dplyr::select(-animal.id, -...22, -deploy.on.timestamp) |> 
  mutate(tag.id = as.numeric(tag.id)) %>%
  filter(!is.na(tag.id)) 

qb <- qbirds |> 
  dplyr::select(Tag_ID, UTC_Date,  UTC_Time,  Latitude,  Longitude,  Location.Quality)
  
length(qb$Tag_ID)

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
  dplyr::select(- UTC_Date, -UTC_Time)


qout <- left_join(qb, qref, by = "tag.id")


# filter out Quebec 


qb <-qout |> 
  mutate(toremove = case_when(
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
    

#head(qb)

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
  

all_dat <- qb

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$tag.id), 1))


##############################
# durations

# Duration between pings/ 
bdd <- all_dat |> 
  mutate(ddate = ymd_hms(date_time)) |> 
  arrange(tag.id, ddate)

bdd_dur <- bdd  |> 
  group_by(tag.id) |> 
  mutate(diff = difftime(ddate, lag(ddate),  units = c("hours")), 
         diff = as.numeric(diff))%>%
  dplyr::filter(diff >0)


## Calculate distance between points and bearing

bdd_det <- bdd_dur  |> 
  #filter(tag.id == 230318) |> 
  group_by(tag.id) |> 
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L)) %>%
  rowwise() %>%
  mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         speed_mhr = round((gcd_m/diff)/1000,1))%>%
  ungroup()

# determine the location of direction 

head(bdd_det)


# stop over = within 25 km of previous point? 

bt <- bdd_det |> 
  #dplyr::select(id, tag.id, ddate, day, month, year,location.long, location.lat,location.lat_prior, diff, gcd_m,bearing, speed_mhr) %>%
  group_by(tag.id)%>%
  mutate(stopover = ifelse( gcd_m <= 25000, "stop-over", "migration")) %>%
  mutate(breeding = case_when(
    stopover == "stop-over" & month %in% c(6,7,8) & location.lat > 60 ~ "breeding", 
    .default = "NA")) %>%
  mutate(direction = case_when(
    location.lat >= location.lat_prior ~ "northward", 
    location.lat <= location.lat_prior~ "southward",
    .default = "NA"))%>%
  ungroup()


idll <- bt|> 
  dplyr::select("location.long", "location.lat", id)


clean_sf <- st_as_sf(bt, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "pt_ming_20240123.gpkg"), append = F)

# 

## Post manual edits

manual_edits <- file.path("output", "manual_edited_complete")
quedit <- read.csv(file.path( manual_edits, "pt_qu_all.csv")) %>%
  left_join(idll) %>%
  mutate(direction = case_when(
    breeding == "wintering"  ~ NA, 
    stopover == "breeding" ~ NA,
    .default = as.character(direction)))%>% 
  dplyr::filter(!is.na(location.lat)) 


qb <-quedit |> 
  mutate(toremove = case_when(
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


#head(qb)

qb <- qb %>% 
  filter(is.na(toremove)) %>% 
  dplyr::select(-toremove) %>%
  dplyr::select(-tag.id, -ddate, -day, -month, -year, -"location.long", -"location.lat")


quout <- left_join(all_dat, qb, by = "id")%>%
  dplyr::select(-fid)



# #save out file
clean_save = quout %>% mutate(proj = "Mingnan")
saveRDS(clean_save, file = file.path("output", "rekn_mignon_raw_20240123.rds"))


clean_sf <- st_as_sf(clean_save, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path(manual_edits, "rekn_qu_20240123.gpkg"), append = F)







#####################################################
# 2)  Johnson 2017 and 2018 


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
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time))


head(all_dat)

all_dat <- all_dat %>%
  mutate(id = seq(1, length(all_dat$tag.id), 1))


# Duration between pings/
bdd <- all_dat |>
  mutate(ddate = ymd_hms(date_time)) |>
  arrange(tag.id, ddate)

bdd_dur <- bdd  |>
  group_by(tag.id) |>
  mutate(diff = difftime(ddate, lag(ddate),  units = c("hours")),
         diff = as.numeric(diff))%>%
  dplyr::filter(diff >0)|> 
  filter(location.lat < 90 ) |> 
  filter(location.lat> -90)


## Calculate distance between points and bearing

bdd_det <- bdd_dur  |>
  #filter(tag.id == 230318) |>
  group_by(tag.id) |>
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L)) %>%
  rowwise() %>%
  mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         speed_mhr = round((gcd_m/diff)/1000,1))%>%
  ungroup()

# determine the location of direction

head(bdd_det)


# # stop over = within 25 km of previous point?
# 
bt <- bdd_det |>
  #dplyr::select(id, tag.id, ddate, day, month, year,location.long, location.lat,location.lat_prior, diff, gcd_m,bearing, speed_mhr) %>%
  group_by(tag.id)%>%
  mutate(stopover = ifelse( gcd_m <= 25000, "stop-over", "migration")) %>%
  mutate(breeding = case_when(
    stopover == "stop-over" & month %in% c(6,7,8) & location.lat > 60 ~ "breeding",
    .default = "NA")) %>%
  mutate(direction = case_when(
    location.lat >= location.lat_prior ~ "northward",
    location.lat <= location.lat_prior~ "southward",
    .default = "NA"))%>%
  mutate(Event1 = Event)
  ungroup()
 
all_dat = bt

# idll <- bt|>
#   dplyr::select("location.long", "location.lat", id)


clean_sf <- st_as_sf(all_dat, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "pt_john_20240123.gpkg"), append = F)






















saveRDS(jgps , file = file.path("output", "rekn_johnson_raw_20240101.rds"))


#########################################################################
# 3)  (Newstead)

raw_dat <- file.path("data", "other_dataset")

filesoi <- list.files(raw_dat)

ndatr <- read_xlsx(file.path(raw_dat, "Newstead","CBBEP_Newstead_Red Knot Gulf to Arctic.xlsx"), 
                  .name_repair = "universal")

nref<- read_xlsx(file.path(raw_dat, "Newstead","CBBEP_Newstead_Red Knot Gulf to Arctic.xlsx"), 
                   sheet = 'Capture Data', .name_repair = "universal") %>% 
  mutate(animal.id = individual.local.identifier) %>%
  rename("animal.sex" = Sex, 
         "animal.life.stage" = Age, 
         "animal.ring.id" = Band.number) 


nref <- nref %>%
  mutate(deploy.on.date = Capture.date,
         deploy.on.measurements = str_c("{culmenInMillimeters:",Culmen, ",theadInMilimeters:", TotalHead,",wingInMillimeters:",Wing,"}"),
         animal.mass = Weight,
         study.site = case_when(
           Site == "Elmers Island" ~ "elmer",
           Site == "Grand Isle" ~ "grandis",
           Site == "Padre Island National Seashore - Southbeach" ~ "padre",
           Site == "Fourchon Beach/Caminada" ~ "fourc",
            ))%>% 
  dplyr::mutate(deploy.on.latitude = case_when(
    study.site == "elmer" ~ 29.1774,
    study.site == "grandis" ~ 29.2451,
    study.site == "padre" ~ 27.063,
    study.site == "fourc" ~ 29.1)) %>%
  dplyr::mutate(deploy.on.longitude = case_when(
    study.site == "elmer" ~ -90.0724,
    study.site == "grandis" ~ -89.9767,
    study.site == "padre" ~ -97.415,
    study.site == "fourc" ~ -90.226))%>%
  dplyr::select("study.site", "animal.id", "deploy.on.date", "deploy.on.measurements",  "deploy.on.latitude",      
                 "deploy.on.longitude" ,"animal.sex" , "animal.ring.id",  "animal.life.stage"      )



ndat <- ndatr %>%
  #filter(`lotek.crc.status.text` != "OK(corrected)")  %>%
  rename("animal.id" = individual.local.identifier,
         "location.lat" = location.lat,
         "location.long" = location.long)%>%
  mutate(proj ="Newstead",
         data_type = "GPS", 
         import.marked.outlier = as.character(import.marked.outlier),
         visible = as.character(visible)) %>%
  mutate(arrive = ymd_hms(Date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(month = month(arrive)) %>%
  mutate(day = day(arrive)) %>%
  mutate(hour = hour(arrive),
        minute = minute(arrive)) %>% 
  dplyr::select(-Date) %>%
  mutate(timestamp = as.character(timestamp)) %>% 
  mutate(date_time = arrive, 
         tag.id = tag.local.identifier)%>% 
  dplyr::select( -event.id, -arrive, -study.name, -individual.taxon.canonical.name,-lotek.crc.status.text) %>%
  mutate(tag.id = as.character(tag.id))



ndat <- left_join(ndat, nref)




ndat3v <- read.csv(file.path(raw_dat, "Newstead","3 V.csv"))

ndat3v  <- ndat3v   %>%
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time))%>% 
  dplyr::select(-utm.northing, -utm.easting ,  -study.timezone,  -mortality.status, tag.voltage,
            -individual.taxon.canonical.name,   -event.id,   -study.local.timestamp,
            -external.temperature, -study.name,  -tag.voltage, -utm.zone, -lotek.crc.status.text ) %>%
  mutate(proj = "Newstead") %>%
  rename("animal.id" = individual.local.identifier)%>%
  mutate(tag.id = tag.local.identifier) %>%
  mutate(tag.id = as.character(tag.id))%>%
  #mutate("date_time" = timestamp)%>%  
  mutate(timestamp = as.character(timestamp))%>%
  mutate(deploy.on.latitude = 27.063, 
         deploy.on.longitude = -97.415 , 
         study.site = "Padre") 

aa <- ndat3v %>%
  dplyr::select(date_time, "animal.id" )%>%
  slice_min(date_time) %>% 
  distinct(date_time)%>%
  pull()

ndat3v  <- ndat3v   %>%
  mutate(deploy.on.date = aa)

#head(ndat)
#head(ndat3v) 

ndat_out <- bind_rows(ndat, ndat3v) %>%
  dplyr::select(-height.above.ellipsoid, -data_type, -tag.local.identifier, -import.marked.outlier, 
                -year, -month, -day, -hour, -minute)%>%
  mutate(tag.model = "Lotek PinPoint GPS-Argos 75", 
         tag.manufacturer.name = "Lotek", 
         deploy.on.date = as.character(deploy.on.date),
         animal.ring.id = as.character(animal.ring.id))


saveRDS(ndat_out, file = file.path("output", "rekn_newstead_20240101.rds"))




