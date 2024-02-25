#Breeding Grounds 


library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(dplyr)

#install.packages("gganimate")
library(gganimate)
library(ggspatial)

raw_dat <- file.path("output")
out_dat <- file.path("output", "report")


# Edits and calculations 

sts <- st_read(file.path(out_dat, "stopover_locations.gpkg")) %>%
  dplyr::mutate(stop_dur = round(difftime( end, start,  units = c("days")),1)) %>% 
  dplyr::mutate(dur_days = as.numeric(stop_dur)) 

sts <- sts |> 
  mutate(DayMonth_s = format(as.Date(start ), "%d-%m"),
         DayMonth_e = format(as.Date(end), "%d-%m"))

######################################################################
## Filter tags which cant be used 

bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv")) %>%
  mutate(tag.id = as.character(tag.id))

head(sts)

so <- left_join(sts, bcat, by = "tag.id")

so <- so |> 
  rowwise() |> 
  mutate(movement_dir = direction) |> 
  mutate(movement_dir = ifelse(breed_type %in% c("wintering", "breeding"), breed_type, movement_dir))

## Get summary form min and max dates 
breeding_sum <- so |> 
  st_drop_geometry() |> 
  group_by(movement_dir, Subpopulations) |> 
  summarise(arrive_mindate = min(DayMonth_s), 
            arrive_maxdate = max(DayMonth_s))


# 
# # read in locations 
# 
# locals <- st_read(file.path(out_dat,"stopover_locations_named.gpkg")) %>% 
#   st_drop_geometry() |> 
#   select(name, country, state, region)
# 
# solo <- left_join(so, locals) |> 
#   select(tag.id, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations, name, region, type)


solo_breed <- so %>% filter(movement_dir == "breeding") %>%
     select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations)


solo_breed<- cbind(st_coordinates(solo_breed),solo_breed)

sb <- solo_breed |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end)) |> 
  select(X, Y, tag.id, Subpopulations, doy_start, doy_end)%>%
  st_drop_geometry()%>% 
  mutate(dur = doy_end - doy_start)


sb <- sb |> 
  filter(Subpopulations %in% c( "WGWP" ,  "nth_sthAm" , "SE",  "TDF")) #         "nth_sthAm_TDF" "SE_nth_sthAm"))
  
  
tags <- as.factor(unique(sb$tag.id))

acc_out <- foreach::foreach(xx = levels(tags), .combine = rbind)%do% {
  
  #xx <- tags[40]
  
  alldat = sb |> 
    filter(tag.id %in% xx) %>% 
    mutate(dur = doy_end - doy_start)

  outdat = alldat %>% 
    slice(rep(1:n(), times = dur))
  
  outdat$doy = seq(outdat$doy_start[1], outdat$doy_end[1]-1, 1)
  
  outdat
}
  
  



## Plots 

world <- ne_countries(scale = "medium", returnclass = "sf")

#Americas <- world %>% dplyr::filter(region_un == "Americas")
Americas <- world %>% dplyr::filter(continent == "North America")


# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out, aes(x = X, y = Y, colour = Subpopulations)) +#colour = "dark blue") +
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
 # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-130, -60), ylim = c(50, 80), expand = FALSE)+
  theme_bw()+
  labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global

# 
# p <- ggplot(acc_out) + 
#   geom_point(aes(x = X, y = Y, colour = Subpopulations)) + 
#  # geom_path(aes(x = X, y = Y, colour = Subpopulations)) + 
#   #facet_grid(~tag.id) +
#   scale_color_viridis_d() + 
#   coord_sf() + 
#   labs(colour = "Day of Year") + 
#   theme_bw() + 
#   theme(
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank(),
#     legend.position = "bottom",
#     legend.key.width = unit(3, "lines")
#   )


p_animate <- global + 
  transition_reveal(along = doy) + 
  labs(title = "Day of Year: {round(frame_along)}")

animate(
  p_animate, 
  width = 10, 
  height = 6, 
  units = "in", 
  res = 72, 
  fps = 10, 
  nframes = 300
)




rf_sf <- wgwp %>%
  filter(Catergory != "static")

world <- ne_countries(scale = "medium", returnclass = "sf")

#Americas <- world %>% dplyr::filter(region_un == "Americas")
Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global











write_sf(solo_breed, file.path(raw_dat, "stopover_breed_testing.gpkg"), append = FALSE)


# start dates 

begin_breed <- solo_breed |> 
  st_drop_geometry() |> 
  select(DayMonth_s,DayMonth_e, Subpopulations, dur_days) |> 
  group_by(Subpopulations) |>  
  summarise(first_breed_commence = min(DayMonth_s),
            last_breed_comment = max(DayMonth_s),
            ave_dur = mean(dur_days),
            first_depart = min(DayMonth_e),
            last_depart = max(DayMonth_e))








# Geographic distributon of tags

# rf_sf <- sf::st_as_sf(clean, coords = c("location.long","location.lat"), crs = 4326, agr = "constant")
#st_write(rf_sf, file.path("output", "all_rekn_20230918.gpkg"))

rf_sf <- wgwp %>%
  filter(Catergory != "static")

world <- ne_countries(scale = "medium", returnclass = "sf")

#Americas <- world %>% dplyr::filter(region_un == "Americas")
Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

