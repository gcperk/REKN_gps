# summary of data 

library(leaflet)
library(RColorBrewer)
library(lubridate)
library(sp)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(viridis)
library(dplyr)
library(mapview)
library("rnaturalearth")
library("rnaturalearthdata")#

#library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
#devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearthhires")#


# define folder structire
data.dir <- file.path ("temp")


bdat<- readRDS(file.path(data.dir, "rekn_gps_locations.rds"))

ru <- bdat %>% dplyr::select( tag.local.identifier,lng, lat, arrive, 
                               year,arr_month, sensor.type)


######################################################################
##Basic plotting 











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


