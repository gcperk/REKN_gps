
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
            ave_lat = mean(location.lat),
            ave_long = mean(location.long))

stop_summaries <- stop_summaries %>% 
  filter(!is.na(ave_long))


stsf <- st_as_sf(stop_summaries, coords = c("ave_long", "ave_lat"), crs = 4326)

write_sf(stsf, file.path(raw_dat, "stopover_summaries2.gpkg"))



# Edits and calculations 

sts <- st_read(file.path(raw_dat, "stopover_summaries2.gpkg")) %>%
  dplyr::mutate(stop_dur = round(difftime( end, start,  units = c("days")),1)) %>% 
  filter(move_type == "stop-over")


hist(as.numeric(sts$stop_dur), breaks = 100)



man_out <- man_out %>%
  #man_out <- man11 %>%
  cbind(st_coordinates(.))%>%
  rename(location.lat = Y, 
         location.long = X) %>%
  st_drop_geometry()










######################################################################
## Filter tags which cant be used 

bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv"))

# filter the tags which cant be used 
cant_use <- bcat |> 
  filter(Catergory == "Not usable") %>%
  dplyr::select(tag.id) %>%
  pull()

bd <- bdat %>% 
  filter(!tag.id %in% cant_use)

bdd <- bd |> 
  dplyr::select(location.long, location.lat, gps.fix.type.raw, lotek.crc.status,proj,
                argos.lc,  tag.model, date_time, year, month, day, hour, minute, tag.id)



bdd <- bdd %>% dplyr::select(location.long, location.lat, date_time, tag.id)


plot(bdd$location.long, bdd$location.lat, col = viridis_pal()(nrow(bdd)), pch = 20, 
     xlab = "x", ylab = "y", asp = 1)

bddvisit = getRecursions(bdd, 25) 

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bddvisit, bdd, legendPos = c(13, -10))






hist(bddvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 2)")
summary(bddvisit$revisits)

head(bddvisit$revisitStats)


par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(martinvisit, martin, legendPos = c(13, -10))

















# Geographic distributon of tags

rf_sf <- sf::st_as_sf(clean, coords = c("location.long","location.lat"), crs = 4326, agr = "constant")
#st_write(rf_sf, file.path("output", "all_rekn_20230918.gpkg"))

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 1, colour = "dark blue") +
  #facet_wrap(~tag.local.identifier)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global








 
 
 
# Stopover density
 #https://partnersinflight.org/stopover-to-passage-ratio-links-airspace-and-stopover-habitat-use-to-identify-high-priority-conservation-areas-for-migrating-birds/
 
 
#https://jamesepaterson.github.io/jamespatersonblog/02_trackingworkshop_trajectories

# trajectory Analysis 

#https://cran.r-project.org/web/packages/move2/vignettes/trajectory_analysis.html

### Using move2 package 
# https://cran.r-project.org/web/packages/move2/vignettes/programming_move2_object.html

#https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/5_MovementSummaries.html

#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/file:///C:/Users/genev/Downloads/KnightEtAl_Ecography_CONInetwork.pdf

# https://rspatial.org/raster/sphere/index.html



