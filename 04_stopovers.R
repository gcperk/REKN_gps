
# generate base data for stop-overs

library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(dplyr)
library(lubridate)
#library(geosphere)
#require(scales)
#library(adehabitatLT)

raw_dat <- file.path("output")
out_dat <- file.path("output", "report")

# read in the final data 
# 
# all <- read.csv(file.path(raw_dat ,"xall_rekn_20240207.csv")) %>% 
#   mutate(tag.id = as.character(tag.id), 
#          date_time = ymd_hms(date_time))
# 
# unique(all$proj)
# generate a direction subset to merge edited file 
# 
# sub_dir <- all |> 
#   dplyr::select(tag.id, location.long, location.lat, date_time,  "gps.fix.type.raw", 
#                 "lotek.crc.status", "argos.lc",               
#                 "diff" , "location.long_prior","location.lat_prior", proj)            


# join the manual edits together and merge to the main dataset 

manual_edits <- list.files(file.path(raw_dat, "manual_edited_complete", "final"))

man1 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_dom_mig_20240123.gpkg"))
#man2 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_john_mig_20240123.gpkg"))%>% 
#  mutate(proj = "johnson")
man3 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_ma_mig_20240123.gpkg"))
man4 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_mils_mig_20240123.gpkg"))
man5 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_newstead_mig_20240123.gpkg"))
man6 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_oceanwinds_20240123.gpkg"))
man7 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_qu_20240128_2.gpkg" ))%>% 
  mutate(proj = "mingan")
# 
# head(man7)
# 
# man7 <- man7 |> 
#   dplyr::select(id, tag.id, animal.id, date_time,minute, hour, day, month, year,  gcd_m,speed_mhr, bearing, diff, stopover,breeding,  direction, geom)
#              
# st_write(man7, file.path(raw_dat, "manual_edited_complete", "final", "rekn_qu_20240128_2.gpkg"))

man8 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_sthcarolina_20240113.gpkg" ))
man9 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_eccc_20240207.gpkg" )) %>% 
  mutate(proj = "ECCC")
man10 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_atlantic_20240207.gpkg" ))%>% 
  mutate(proj = "atlantic")

man11 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_spring_USFW_20230214.gpkg" ))%>% 
  mutate(proj = "spring", date_time = ddate)

man_out <- bind_rows(man1, man3, man4, man5, man6, man7, man8, man9, man10, man11) 

man_out <- man_out %>%
#man_out <- man11 %>%
    cbind(st_coordinates(.))%>%
  rename(location.lat = Y, 
         location.long = X) %>%
  st_drop_geometry()



## checks 
#length(sub_dir$tag.id)
#length(man_out$tag.id)
#length(st$tag.id)

# stsf <- st_as_sf(man_out, coords = c("location.long", "location.lat"), crs = 4326)
# 
# write_sf(stsf, file.path(raw_dat, "test_edited_compiled2.gpkg"))
# 
# unique(stsf$proj)
# 
# 
# 



## calculate stop over locations and dates 

# convert migration and stop-over to catergories 
stops <- man_out |> 
     mutate(stop_code = case_when(
        stopover == "stop-over" ~ 0, 
         stopover == "migration" ~ 1,
         TRUE ~ NA))

out <- stops %>%
  #filter(tag.id == "201135") %>%
  group_by(tag.id) |> 
  arrange(date_time) |> 
 # mutate(movement = ifelse(stop_code != lead(stop_code), 1, 0)) %>% 
  mutate(move_event = cumsum(stop_code != lag(stop_code) | row_number() == 1))
         

out <- out |> 
  select( c(`tag.id`,"date_time", "location.long", "location.lat" , "stopover" ,
            "breeding", "direction" ,"stop_code" ,"move_event" ))



stop_summaries <- out %>%
  #filter(tag.id == "201135") %>%
  group_by(tag.id, move_event ) %>%
  summarise(start = min(date_time),
            end = max(date_time),
            move_type = unique(stopover),
            breed_type = unique(breeding),
            direction = unique(direction)[1],
            ave_lat = mean(location.lat),
            ave_long = mean(location.long))

stop_summaries <- stop_summaries %>% 
  filter(!is.na(ave_long))


stsf <- st_as_sf(stop_summaries, coords = c("ave_long", "ave_lat"), crs = 4326)

write_sf(stsf, file.path(raw_dat, "stopover_summaries3.gpkg"))
#write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))






## Note this file has been manually edited for stop-over locations 



# Edits and calculations 

sts <- st_read(file.path(out_dat, "stopover_locations.gpkg")) %>%
  dplyr::mutate(stop_dur = round(difftime( end, start,  units = c("days")),1)) #%>% 
 # filter(move_type == "stop-over")

hist(as.numeric(sts$stop_dur), breaks = 100)

# 
# man_out <- man_out %>%
#   #man_out <- man11 %>%
#   cbind(st_coordinates(.))%>%
#   rename(location.lat = Y, 
#          location.long = X) %>%
#   st_drop_geometry()






######################################################################
## Filter tags which cant be used 

bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv")) %>%
    mutate(tag.id = as.character(tag.id))

head(sts)

so <- left_join(sts, bcat, by = "tag.id")

#length(sts$tag.id)
#length(so$tag.id)


### Write out for review 


write_sf(so, file.path(raw_dat, "stopover_summaries_testing.gpkg"))
#write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))



