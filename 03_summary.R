# summary of data 

library(lubridate)
library(sp)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(viridis)
library(dplyr)


raw_dat <- file.path("output")
out_dat <- file.path("output", "report")

all <- read.csv(file.path(raw_dat ,"xall_rekn_20240207.csv"))

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
                             "date_time", "timestamp", "year",   "month",    "day" ,   "hour" ,    "minute" , 
                             "argos.lc" , "lotek.crc.status", "gps.fix.type.raw" ,  
                             "bearing" , "diff", "gcd_m",   "speed_mhr",  #"breeding" ,"direction" ,
                             #"stopover", 
                             "location.lat_prior" , "location.long_prior",                     
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


study.site.proj <- ref |> 
  dplyr::select(tag.id, study.site, proj ) |> 
  group_by(study.site, proj) |> 
  summarise(no.of.tags = length(unique(tag.id)))


capture_loc <- ref |> 
  dplyr::select(tag.id, study.site,  animal.life.stage, animal.sex, deploy.on.date, deploy.on.latitude,  deploy.on.longitude ) %>%
  distinct() %>%
  st_as_sf( coords = c("deploy.on.longitude", "deploy.on.latitude"), crs = 4326)

# write out capture locations 
#write_sf(capture_loc, file.path(raw_dat , "maps", "capture_locations.gpkg"))





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

range(loc$year)

table_max <- loc %>% 
  dplyr::mutate(date_time = ymd_hms(date_time)) |> 
  dplyr::select(tag.id, date_time) %>% 
  # group_by(tag.local.identifier)%>%  -->
  slice_max(date_time, by = tag.id) |> 
  distinct()
colnames(table_max)<- c("tag.id","max") 

table_min <-  loc %>% 
  dplyr::mutate(date_time = ymd_hms(date_time)) |>   
  dplyr::select(tag.id, date_time) %>%  
  # group_by(tag.local.identifier)%>%   
  slice_min(date_time, by = tag.id)  |> 
  distinct()

colnames(table_min)<- c("tag.id", "min")  

dur <- left_join(table_max, table_min, by = join_by(tag.id)) %>%  
  distinct() %>%  
  dplyr::mutate(duration = max - min ) %>%  
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
  scale_color_viridis_d()+
  #facet_wrap(~year)+  
  xlab("duration (days)")   

dur_hist  

p_duration <- ggplot(dloc, aes(factor(month), fill = factor(year)))+  
  geom_bar(position = "dodge") +
  scale_color_viridis_d()+
  #xlim(1,12)+  
  #facet_wrap(~tag.local.identifier)+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  






# duration of tag per tag type 

dur_type <- dur %>% 
  left_join(ref) 

dur_hist <- ggplot(dur_type, aes(x= dur_days, fill = tag.model)) +  
  geom_histogram(position = "dodge") + #fill="white", position="dodge") +  
  scale_fill_viridis_d()+
  xlab("duration (days)")  

dur_hist

aa <- dur_type |> select(tag.id , dur_days, tag.model, proj)%>%
  distinct()



p1 <- ggplot(aa, aes(x= dur_days)) +
  geom_histogram()


dur_type_sum <- dur_type |> select(tag.id , dur_days, tag.model)%>%
  distinct()%>% 
  filter(dur_days>0) |> 
  group_by( tag.model) |> 
  summarise(count = n(),
            min = min(dur_days), 
            max = max(dur_days),
            mean_dur = mean(dur_days), 
            sd = sd(dur_days))

ggplot(dur_type_sum) +
  geom_bar( aes(x=tag.model , y=mean_dur), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=tag.model, ymin=mean_dur -sd, ymax=mean_dur+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)


ggplot(dur_type, aes(x=tag.model, y=dur_days)) + 
  geom_bar(stat = "identity")



# add the group by
ggplot(dur_type, aes(tag.id, dur_days,)) +
  geom_col(aes(fill = proj))#, position = position_stack(reverse = TRUE))

ggplot(dur_type, aes(x = forcats::fct_reorder(as.factor(tag.id), dur_days), y = dur_days)) +
  geom_col(aes(fill = proj))+
  facet_wrap(~proj, scales = "free_x")#, position = position_stack(reverse = TRUE))


# 
# 
# # duration looking at dates 
# 
# # plot total duration of collar data 
# ggplot(dur, aes(y=factor(tag.id))) +
#   geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), linewidth = 1)+
#   xlab("Date") + ylab("Tag") 
# 
# 
# # plot total duration of collar data 
# ggplot(dur, aes(y=factor(tag.id))) +
#   geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id),colour = proj), linewidth = 1)+
#   scale_color_discrete()+
#   xlab("Date") + ylab("Tag") 
# 
# 
# # plot total duration of collar data 
# ggplot(dur, aes(y=factor(proj))) +
#   geom_segment(aes(x=min, xend=max, y=factor(proj), yend=factor(proj), colour = proj), linewidth = 3)+
#   scale_color_discrete()+
#   xlab("Date") + ylab("Tag") 
# 
# 
# # plot total duration of collar data 
# ggplot(dur, aes(y=factor(tag.id))) +
#   geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), linewidth = 1)+
#   xlab("Date") + ylab("Tag") +
#   facet_wrap(~proj, scales = "free")
# 



#####################################################################################

# seasonality by tag 

bs <- all |> 
  dplyr::select(month, year, tag.id) %>% 
  distinct() %>%
  group_by(month, year) %>%
  count()
# mutate(sdate = ymd(ddate))

# no of tags per month (all years)

ggplot(bs, aes(x = as.factor(month), y = n)) +
  geom_col() 


# By year 
ggplot(bs, aes(x = as.factor(month), y = n, fill = as.factor(year))) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  scale_fill_viridis_d()+
  #geom_bar(stat = "identity", position = "dodge") #+
  facet_wrap(~year)+
  theme_bw()+
  xlab("month")+ ylab("count")+
  theme(legend.position = "none")


####################################################################################










  
  
#   
#   
# 
# ######################################################################
# ## Filter tags which cant be used 
# 
# bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv"))
# 
# edit_tags <- unique(bcat$tag.id)
# 
# 
# xx <- setdiff(edit_tags, dat_tags)
# # these tags already 
# #213948 229367 232341 232342 232344 239414 242699
# 
# 
# bcat <- left_join(bcat, model.id)
# 
# # filter the tags which cant be used 
# cant_use <- bcat |> 
#   filter(Catergory == "Not usable") %>%
#   dplyr::select(tag.id) %>%
#   pull()
# 
# 
# bd <- bdat %>% 
#   filter(! tag.id %in% cant_use)
# 
# # no of tags per project - note the duplicate atlantic and spring migration is still in this data set so 
# # need to manually remove the duplicates
# 
# no_ids_proj <- bd %>% 
#   group_by(proj)%>%
#   summarise(n_tags = length(unique(tag.id)))
# 
# 
# bdd <- bd |> 
#   dplyr::select(location.long, location.lat, gps.fix.type.raw, lotek.crc.status, sensor.type,
#                 argos.lc,  tag.model, date_time, year, month, day, hour, minute, tag.id)
# 


# Types of data : GPS vs argos

#The data split between gps and argos data records. Large proportion are argos doppler shift.   These represent two different methods of calculating the locations and all types can be used. 

tag_type <- bdd %>%
  dplyr::select(tag.id, sensor.type) %>%
  group_by(tag.id, sensor.type)%>%
  summarise(n = n())

#tag_type

tag_type_date <- bdd %>%
  dplyr::select(tag.id, sensor.type, year, month) %>%
  group_by(tag.id, sensor.type, year,month)%>%
  summarise(n = n())

#tag_type_date

p_alldat <- ggplot( bdd, aes(year, fill = sensor.type))+
  geom_bar(position = "dodge") 


p_alldat 


#####################################################################################

# Duration between pings/ 
bdd <- all |> 
  mutate(ddate = ymd_hms(date_time)) |> 
  arrange(tag.id, ddate,tag.model)

ggplot(bdd, aes(tag.id, diff)) +
  geom_col(aes(fill = tag.model))#, position = position_stack(reverse = TRUE))


# average and min max between gps 

 bdd_dur_sum <- bdd_dur |> 
   group_by(tag.id) |> 
   filter(diff != 0) |> 
   summarise(mean = mean(diff, na.rm = T), 
             min = min(diff, na.rm = T), 
             max = max(diff, na.rm = T))

bdd_dur_sum <- left_join(bdd_dur_sum, bcat)%>%
  select(-X, -Subspecies, -Subpopulations, -Catergory, -To, -From, -Directon, -Duplicates, -animal.id)


# Checked these tags and correct 

# 232985 - sunbird 
# 232986 - sunbird 

# 229370 - sunbird? Mingan
# 240157 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 240155 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 240162 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 240160 - deply april 2023 gap til Oct - sunbird . Might be fixed by Movebank issue 
# 242570 - sunbird , gap in timing but seems legit
# 242573 - sunbird , gap in timing but seems legit
# 240156 - sunbird - legit


# Number of ppoints and average frequency. 

ggplot(bdd_dur_sum, aes(mean, fill = tag.model)) +
  geom_bar(stat = "identity")#, position = position_stack(reverse = TRUE))

 
 
 
 
#  
# ###############################################
# # map data
# 
# ru <- ru[complete.cases(ru), ]
# rf_sf <- st_as_sf(ru, coords = c("lng","lat"), crs = 4326, agr = "constant")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# Americas <- world %>% 
#   dplyr::filter(region_un == "Americas")
# 
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = rf_sf, size = 1, colour = "dark blue") +
#   #facet_wrap(~common_name)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global
# 
# #######################
# 
# rekn <- ru %>% 
#   filter(!is.na(lat)) %>%
#   filter(arr_month %in% c(4,5,6,7,8,9,10,11)) 
# 
# 
# rf_sf <- st_as_sf(rekn, coords = c("lng","lat"), crs = 4326, agr = "constant")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# Americas <- world %>% 
#   dplyr::filter(region_un == "Americas")
# 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = rf_sf , aes(color = arr_month), alpha = 0.5, size = 1) +
#   #facet_wrap(~arr_month, nrow = 3)+
#   scale_color_viridis_c() +
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80),, expand = FALSE)+
#   theme_minimal() +
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         legend.text=element_text(size=10))+
#   guides(color = guide_legend(override.aes = list(size = 3,alpha = 1)))
# 
# global 
# 
# 
# # map data
# 
# 
# bdat_sp <- ru 
# 
# 
# 
# #month_col = sort(unique(bdat_sp$arr_month))
# palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)
# 
# pal <- colorFactor(
#   palette = "viridis",
#   domain = unique(bdat_sp$tag.local.identifier))
# 
# tags <- unique(bdat_sp$tag.local.identifier)
# 
# 
# birdmapall <- leaflet(bdat_sp) %>%
#   # add a dark basemap
#   #addProviderTiles("CartoDB.DarkMatter") %>%
#   addProviderTiles("CartoDB") %>%
#   addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
#                    weight = 3, color = ~palette1(bdat_sp$arr_month), 
#                    fill = TRUE,
#                    label = ~arr_month,
#                    #radius = ~dur_days/10,
#                    fillColor = ~palette1(bdat_sp$arr_month)) %>%
#   #popup = ~animal.id) %>%
#   addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
#                color = "grey",  opacity = 0.1, stroke = TRUE) %>%
#   addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
#             title = "Arrival Month",
#             opacity = 1) 
# 
# birdmapall
# 
# 
