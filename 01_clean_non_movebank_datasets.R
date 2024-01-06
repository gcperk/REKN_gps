# Compile al the other pieces of data - not curretnly on movebank format 

## 


library(leaflet)
library(RColorBrewer)
library(dplyr)
library(lubridate)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


raw_dat <- file.path("data", "other_dataset")

filesoi <- list.files(raw_dat)


#####################################################
# 1) Yves_Aubres 

qbirds <- read.csv(file.path(raw_dat, "aurey_yves", "redknotcan_6687_QuebecRawLotek.csv"))

qb <- qbirds |> 
  dplyr::select(Tag_ID, UTC_Date,  UTC_Time,  Latitude,  Longitude,  Location.Quality)
  

# calculate time differences
qb <- qb %>%
  mutate(date_time = ymd(UTC_Date))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         time = hms(UTC_Time),
         hour = hour(time),
         minute = minute(time)) %>% 
  dplyr::select(-time, -UTC_Date) %>%
  mutate(Tag_ID = gsub("6687:", "",Tag_ID)) %>%
  rename("animal.id" = Tag_ID,
       "location.lat" = Latitude,
       "location.long" = Longitude ,
      "argos.lc" = Location.Quality)


clean_save = qb %>% mutate(proj = "Mingnon")
saveRDS(clean_save, file = file.path("output", "rekn_mignon_raw_20231219.rds"))



#####################################################
# 2)  Johnson 2017 and 2018 

j17 <- read.csv(file.path(raw_dat, "johnson", "REKN_GPSprocessed_2017.csv")) %>% 
  mutate(Lat = as.numeric(Lat), 
         Long = as.numeric(as.character(Long)),
         Long_new = as.numeric(Long_new))

j18 <- read.csv(file.path(raw_dat, "johnson", "REKN_GPSprocessed_2018.csv"))


jd <- bind_rows(j17, j18)%>%
#   filter(CRC %in% c(NA , "OK")) %>%
#   filter(LocType_new != "Bad") %>%
  rename("animal.id" = FlagID,
          "location.lat" = Lat,
          "location.long" = Long) %>%
   mutate(proj ="Johnson_GPS",
          data_type = "GPS")

# Basic summary of individuals

jgps <- jd %>%
  mutate(arrive = mdy(Date)) %>%
  mutate(year = year(arrive), 
         month = month(arrive), 
         day = day(arrive)) |> 
  rename("animal.ring.id" = Band,
           "animal.mass" = Mass) %>%
  dplyr::select(-Cohort, -Sequence, -Site, -Period, -Event,  )
  




# TO do : generate a date_time






saveRDS(jgps , file = file.path("output", "rekn_johnson_raw_20231219.rds"))


#########################################################################
# 3)  (Newstead)

ndat <- read_xlsx(file.path(raw_dat, "Newstead","CBBEP_Newstead_Red Knot Gulf to Arctic.xlsx"), 
                  .name_repair = "universal")

ndat <- ndat %>%
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
  dplyr::select(-Date, -arrive) %>%
  dplyr::select( -event.id, -study.name, -individual.taxon.canonical.name) %>%
  mutate(timestamp = as.character(timestamp))


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
            -external.temperature, -study.name, -date_time, -tag.voltage, -utm.zone ) %>%
  mutate(proj = "Newstead")




# TO do : generate a date_time








ndat_out <- bind_rows(ndat, ndat3v)

saveRDS(ndat_out, file = file.path("output", "rekn_newstead_20231219.rds"))



##### Felicia 's data is already in movebank 




