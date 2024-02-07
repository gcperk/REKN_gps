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

bdat <- read.csv(file.path(raw_dat ,"xall_rekn_20240203.csv"))

# bb <- bdat %>% 
#   filter(proj == "Newstead") %>% 
#   select(tag.id,animal.id) |> 
#   distinct()


model.id <- bdat |> 
  dplyr::select(tag.id, proj) |> 
  distinct()

dat_tags <- unique(bdat$tag.id)


######################################################################
## Filter tags which cant be used 

bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv"))

edit_tags <- unique(bcat$tag.id)


xx <- setdiff(edit_tags, dat_tags)
# these tags already 
#213948 229367 232341 232342 232344 239414 242699


bcat <- left_join(bcat, model.id)

# filter the tags which cant be used 
cant_use <- bcat |> 
  filter(Catergory == "Not usable") %>%
  dplyr::select(tag.id) %>%
  pull()


bd <- bdat %>% 
  filter(! tag.id %in% cant_use)

# no of tags per project - note the duplicate atlantic and spring migration is still in this data set so 
# need to manually remove the duplicates

no_ids_proj <- bd %>% 
  group_by(proj)%>%
  summarise(n_tags = length(unique(tag.id)))


bdd <- bd |> 
  dplyr::select(location.long, location.lat, gps.fix.type.raw, lotek.crc.status, sensor.type,
                argos.lc,  tag.model, date_time, year, month, day, hour, minute, tag.id)



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



# tag model
tag_model <- bdd %>%
  dplyr::select(tag.id, tag.model) %>%
  group_by(tag.id, tag.model)%>%
  summarise(n = n())

#tag_model

p_model <- ggplot( tag_model, aes(tag.model)) +
  geom_bar(position = "dodge") 



##############################################################
## duration of tag 
#############################################################

## 1. Min and Max - date - duration 

library(dplyr)
library(lubridate)

bdd <- bdd |> 
  mutate(ddate = ymd_hms(date_time))

# calculate the minimum and max dates per collar 
table_max <- bdd |> 
  dplyr::select(tag.id, ddate) |> # select the columns of interest
  slice_max(ddate, by = tag.id)%>%
  distinct()  # select the max value of date_time, per animal.id
colnames(table_max)<- c("tag.id","max") 

table_min <- bdd |> 
  dplyr::select(tag.id, ddate) |> 
  slice_min(ddate, by = tag.id) 

colnames(table_min)<- c("tag.id","min")

# merge the two tables together and calculate the duration 
dur <- left_join(table_max, table_min, by = join_by(tag.id)) |> 
  distinct() |> 
  mutate(duration = max - min) |> # calculate duration (default is days)
  mutate(dur_raw = round(duration, 1)) |> # format the duration 
  mutate(dur_hrs = round(as.numeric(dur_raw),1)) |> # convert to hours
  mutate(dur_days = round(as.numeric(dur_hrs)/24,1)) |> # convert to hours
  mutate(year_start = year(min), 
         year_end = year(max))

# add the group by 

bids <- bcat %>% 
  dplyr::select(tag.id, proj, tag.model)

dur <- left_join(dur, bids)

# plot total duration of collar data 
#ggplot(dur, aes(dur_hrs)) +
#  geom_point(aes(fill = tag.id) )

ggplot(dur, aes(tag.id, dur_hrs,)) +
  geom_col(aes(fill = proj))#, position = position_stack(reverse = TRUE))

ggplot(dur, aes(x = forcats::fct_reorder(tag.id, dur_days), y = dur_days)) +
  geom_col(aes(fill = proj))+
  facet_wrap(~proj, scales = "free_x")#, position = position_stack(reverse = TRUE))




# average duration by project 

ggplot(dur, aes(tag.id, dur_hrs,)) +
  geom_col(aes(fill =tag.model))#, position = position_stack(reverse = TRUE))



# total duration by tag 
dur_tag <- dur |> 
  group_by(tag.model) |> 
  summarise(min_days = min(dur_days),
            max_days = max(dur_days),
            mean_days = mean(dur_days))

dur_tag 



# duration looking at dates 


# plot total duration of collar data 
ggplot(dur, aes(y=factor(tag.id))) +
  geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), linewidth = 1)+
  xlab("Date") + ylab("Tag") 


# plot total duration of collar data 
ggplot(dur, aes(y=factor(tag.id))) +
  geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id),colour = proj), linewidth = 1)+
  scale_color_discrete()+
  xlab("Date") + ylab("Tag") 


# plot total duration of collar data 
ggplot(dur, aes(y=factor(proj))) +
  geom_segment(aes(x=min, xend=max, y=factor(proj), yend=factor(proj), colour = proj), linewidth = 3)+
  scale_color_discrete()+
  xlab("Date") + ylab("Tag") 


# plot total duration of collar data 
ggplot(dur, aes(y=factor(tag.id))) +
  geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), linewidth = 1)+
  xlab("Date") + ylab("Tag") +
  facet_wrap(~proj, scales = "free")



#####################################################################################

# seasonality 

bs <- bdd |> 
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
  #geom_bar(stat = "identity", position = "dodge") #+
  facet_wrap(~year)#, position = position_stack(reverse = TRUE))


####################################################################################

#





















#####################################################################################

# Duration between pings/ 
bdd <- bdd |> 
  mutate(ddate = ymd_hms(date_time)) |> 
  arrange(tag.id, ddate,tag.model)

bdd_dur <- bdd  |> 
  #mutate(time = as.POSIXct(date_time, format = "%y/%d/%m %H:%M:%S")) |> 
  group_by(tag.id) |> 
  mutate(diff = difftime(ddate, lag(ddate),  units = c("hours")), 
         diff = as.numeric(diff))

# we can see a big range in the time intervals for the fixes
range(bdd_dur$diff, na.rm = TRUE)


ggplot(bdd_dur, aes(tag.id, diff)) +
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

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
###############################################
# map data

ru <- ru[complete.cases(ru), ]
rf_sf <- st_as_sf(ru, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 1, colour = "dark blue") +
  #facet_wrap(~common_name)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

#######################

rekn <- ru %>% 
  filter(!is.na(lat)) %>%
  filter(arr_month %in% c(4,5,6,7,8,9,10,11)) 


rf_sf <- st_as_sf(rekn, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf , aes(color = arr_month), alpha = 0.5, size = 1) +
  #facet_wrap(~arr_month, nrow = 3)+
  scale_color_viridis_c() +
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-170.15, -30), ylim = c(-60, 80),, expand = FALSE)+
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.text=element_text(size=10))+
  guides(color = guide_legend(override.aes = list(size = 3,alpha = 1)))

global 


# map data


bdat_sp <- ru 



#month_col = sort(unique(bdat_sp$arr_month))
palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$tag.local.identifier))

tags <- unique(bdat_sp$tag.local.identifier)


birdmapall <- leaflet(bdat_sp) %>%
  # add a dark basemap
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
                   weight = 3, color = ~palette1(bdat_sp$arr_month), 
                   fill = TRUE,
                   label = ~arr_month,
                   #radius = ~dur_days/10,
                   fillColor = ~palette1(bdat_sp$arr_month)) %>%
  #popup = ~animal.id) %>%
  addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
               color = "grey",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
            title = "Arrival Month",
            opacity = 1) 

birdmapall


