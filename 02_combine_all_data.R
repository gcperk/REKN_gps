
#library(leaflet)
#library(RColorBrewer)

library(lubridate)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)


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


# write out: 
write.csv(all, file.path("output", "xall_rekn_20240203.csv"), row.names = F)
clean_sf <- st_as_sf(all, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("output", "xall_rekn_20240203.gpkg"), append = F)


# extract reference information 
ref <- all %>% 
  dplyr::select(animal.comments, animal.id, animal.life.stage, animal.marker.id, 
                animal.mass,animal.ring.id, animal.sex, animal.taxon, "attachment.type", 
                "capture.timestamp",  "deploy.on.date" ,"deploy.on.latitude", "deploy.on.longitude" , 
                "deploy.on.measurements", "deploy.on.person" , "deployment.comment", 
                "duty.cycle", "sensor.type" , "tag.beacon.frequency"   ,   "tag.manufacturer.name",
                "tag.mass", "tag.id", "tag.serial.no" ,  "tag.model", "study.site", proj)   %>%
  distinct()
                                   
                
loc <- all %>% dplyr::select("proj", id, "tag.id", tag.model, "location.lat" ,"location.long" ,  
                         "date_time", "timestamp", # "year",   "month",    "day" ,   "hour" ,    "minute" , 
                        "argos.lc" , "lotek.crc.status", "gps.fix.type.raw" ,  
                        #"bearing" , "diff", "gcd_m",   "speed_mhr",  "breeding" ,"direction" ,
                        #"stopover", "location.lat_prior" , "location.long_prior",                     
                        "height.above.ellipsoid"  , "argos.altitude") %>%
  unique()
                                 
 # clean up the spatial resolution # can remove these cols. 
unique(loc$argos.lc)
unique(loc$lotek.crc.status)
unique(loc$gps.fix.type.raw)

## How many tags and types of tage 
unique(ref$tag.model)

#length(unique(ref$tag.id))
#29+66+1+5+22+43+42+10+15+15+2+36+34+3




### SUMMARY OF ANIMALS 

# tags per project 
tag.proj <-  ref %>% 
  dplyr::select(tag.id, proj) %>%
  group_by( proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))

# mingnon tags not matching ##


# tag number per tag type
tag.types <- ref %>% 
  dplyr::select(tag.id, tag.model, proj) %>%
  group_by(tag.model, proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))

# type of fix per tag model (accuracy per tag type - only relevant for pinpoint)
tag.types.model <- loc %>% 
  dplyr::select(tag.id, tag.model, proj, gps.fix.type.raw) %>%
  group_by(gps.fix.type.raw, tag.model) |> 
  summarise(no.of.tags = length(unique(tag.id)))
  

### Capture location 

study.site <- ref |> 
  dplyr::select(tag.id, study.site, deploy.on.date, deploy.on.latitude,  deploy.on.longitude ) |> 
  group_by(study.site) |> 
  summarise(no.of.tags = length(unique(tag.id)))

capture_loc <- ref |> 
  dplyr::select(tag.id, study.site,  animal.life.stage, animal.sex, deploy.on.date, deploy.on.latitude,  deploy.on.longitude ) %>%
  distinct() %>%
  st_as_sf( coords = c("deploy.on.longitude", "deploy.on.latitude"), crs = 4326)

# write out capture locations 
write_sf(capture_loc, file.path(raw_dat , "maps", "capture_locations.gpkg"))





### Summary statistics

## Age / sex of individuals 

## capture age and sex 
sex_sum <- ref |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage, animal.sex) |> 
  group_by(animal.sex,proj) |> 
   summarise(no.of.tags = length(unique(tag.id)))

age_sum <- ref |> 
  dplyr::select(tag.id, study.site, proj, animal.life.stage) |> 
  group_by(animal.life.stage , study.site) |> 
  #group_by(animal.life.stage ) |> 
    summarise(no.of.tags = length(unique(tag.id)))


# ring-id for subpop information
rids <- ref %>%
  dplyr::select(animal.ring.id, tag.id, proj) %>% 
  group_by(animal.ring.id, tag.id ) %>%
  count()






### Duration statistics 


# calculate the duration of the tags total 

dloc <- loc  %>%
  mutate(date_time = ymd_hms(timestamp))%>%
  mutate(year = year(date_time )) %>%
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time)) %>%
  filter(year<=2023)

# # missing tag information 
# nas <- all %>%
#   filter(is.na(tag.model)) %>%
#   dplyr::select(proj) |>
#   distinct()
# 
# unique(all$tag.model)

range(dloc$year)

table_max <- dloc %>%  
  dplyr::select(tag.id, date_time) %>% 
  # group_by(tag.local.identifier)%>%  -->
  slice_max(date_time, by = tag.id) |> 
  distinct()
  colnames(table_max)<- c("tag.id","max") 

table_min <-  dloc %>%   
    dplyr::select(tag.id, date_time) %>%  
   # group_by(tag.local.identifier)%>%   
    slice_min(date_time, by = tag.id)  |> 
  distinct()

  colnames(table_min)<- c("tag.id", "min")  

dur <- left_join(table_max, table_min, by = join_by(tag.id)) %>%  
    distinct() %>%  
    dplyr::mutate(duration = max- min ) %>%  
    mutate(dur_min = round(as.numeric(duration)/60,1))%>%  
    mutate(dur_hrs = round(as.numeric(dur_min)/60,1))%>%  
    mutate(dur_days = round( dur_hrs/24,1))%>%  
    mutate(year = year(min))  

dur_plot <- ggplot(dur, aes(y=factor(tag.id))) +  
    geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), size=1)+  
    xlab("Date") + ylab("Tag")   

dur_plot  

dur_hist <- ggplot(dur, aes(x= dur_days))+  
    geom_histogram() + #fill="white", position="dodge") +  
    #facet_wrap(~year)+  
    xlab("duration (days)")   

  dur_hist  

p_duration <- ggplot(dloc, aes(factor(month), fill = factor(year)))+  
    geom_bar(position = "dodge") +  
    #xlim(1,12)+  
    #facet_wrap(~tag.local.identifier)+  
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  




# duration of tag per tag type 


dur_type <- dur %>% 
  left_join(ref) 

dur_hist <- ggplot(dur_type, aes(x= dur_days)) +  
    geom_histogram(position = "dodge") + #fill="white", position="dodge") +  
    facet_wrap(~tag.model)+  
    xlab("duration (days)")  
    

dur_hist

  
