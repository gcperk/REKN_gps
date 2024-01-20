# summary of data 

#library(leaflet)
#library(RColorBrewer)
library(lubridate)
library(sp)
library(sf)
#library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(viridis)
library(dplyr)
#library(mapview)
#library("rnaturalearth")
#library("rnaturalearthdata")#

#ibrary(tmap)    # for static and interactive maps
#devtools::install_github("ropensci/rnaturalearthhires")
#library("rnaturalearthhires")#


raw_dat <- file.path("output")
out_dat <- file.path("output", "report")

bdat <- read.csv(file.path(raw_dat ,"xsubset_rekn_20240119.csv"))




# basic summaries 

# no of dataset

proj <- unique(bdat$proj)


no_ids_proj <- bdat %>% 
  group_by(proj, tag.id, animal.id)%>%
  count()

write.csv(no_ids_proj, file.path(out_dat, "proj_animalid.csv"), row.names = T)




######################################################################
##Basic plotting 



bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv"))


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
                argos.lc, date_time, year, month, day, hour, minute, tag.id)



# add the duration time since 

# 
# 
# If we remove all the algorithm marked outliers (lotek = error, argos accuracy value = A, B, Z). We reduce the number of observations in the data set from `r length(bout$tag.local.identifier)` to `r length(clean$tag.local.identifier)`.
# Argos accuracy codes are: 
#   
#   - Class 0: over 1500 m radius (not currently filtered)
# 
# - Class A: Argos no accuracy estimation (3 messages received)
# - Class B: Argos no accuracy estimation (1 or 2 messages received)
# - Class Z: Argos Invalid Location
# 
# 
# The cleaned date range varies from 
# `r min(range(clean$year))` to `r max(range(clean$year))`.
# 

# GPS vs argos

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


## duration of tag 
library(dplyr)
library(lubridate)

bdd <- bdd |> 
  mutate(ddate = ymd_hms(date_time)) #|> 
 # mutate(sdate = ymd(ddate))

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
  dplyr::select(tag.id, proj)

dur <- left_join(dur, bids)


dur <- dur %>% 
  arrange(dur_days, group.by = proj)


# total length of tag duration 


# plot total duration of collar data 
#ggplot(dur, aes(dur_hrs)) +
#  geom_point(aes(fill = tag.id) )

ggplot(dur, aes(tag.id, dur_hrs,)) +
  geom_col(aes(fill = proj))#, position = position_stack(reverse = TRUE))


ggplot(dur, aes(x = forcats::fct_reorder(tag.id, dur_days), y = dur_days)) +
  geom_col(aes(fill = proj))+
  facet_wrap(~proj, scales = "free_x")#, position = position_stack(reverse = TRUE))


# plot total duration of collar data 
ggplot(dur, aes(y=factor(tag.id))) +
  geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), linewidth = 1)+
  xlab("Date") + ylab("Tag") 


# plot total duration of collar data 
ggplot(dur, aes(y=factor(tag.id))) +
  geom_segment(aes(x=min, xend=max, y=factor(tag.id), yend=factor(tag.id)), linewidth = 1)+
  xlab("Date") + ylab("Tag") +
  facet_wrap(~proj, scales = "free")



#####################################################################################

# get basic summaries 

no.birds <- bdat %>%
  
  count(tag.id)









sbou <- sbou |> 
  arrange(animal.id, date_time)

sbou_dur <- sbou |> 
  mutate(time = as.POSIXct(date_time, format = "%y/%d/%m %H:%M:%S")) |> 
  group_by(animal.id) |> 
  mutate(diff = difftime(time, lag(time),  units = c("hours")), 
         diff = as.numeric(diff))

# we can see a big range in the time intervals for the fixes
range(sbou_dur$diff, na.rm = TRUE)









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


