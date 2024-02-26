
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

# Read in edits document 

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

#head(sts)

so <- left_join(sts, bcat, by = "tag.id")


# convert to single column with breeding/wintering/direction
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


###############################
# # read in locations 

locals <- st_read(file.path(out_dat,"stopover_locations_named.gpkg")) %>%
  st_drop_geometry() |>
  select(name, country, state, region)

solo <- left_join(so, locals) |>
  select(tag.id, DayMonth_s,DayMonth_e, start, end, movement_dir, dur_days, Subpopulations, name, region, type)


rf_sf <- solo %>%
  filter(!type %in% c("static", "Not usable")) |> 
  mutate(Subpop_class = case_when(
    Subpopulations %in% c("SE_nth_sthAm_TDF", "unsure" ,"Not usable" , "NA" ,NA,"unsure(se?)", "SE_nth_sthAm" ) ~ "unknown", 
    .default = as.character(Subpopulations)))
    
  

# Geographic distributon of tags
world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(region_un == "Americas")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 2, colour = "dark blue") +
  facet_wrap(~movement_dir)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global


# jpeg(file.path(out.plots,"rose_density.jpg"), width = 30, height = 30,units = "cm", res = 210)

# entire north America 
global_sub <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 1, colour = "dark blue") +
  facet_wrap(~Subpop_class)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global_sub








# Detailed geographic locations vs sub populations

We can estimate the subpopulation level for those individuals with longer duration of tag timing?
  Using the estimated ranges
- South East US
- Tierra Del Fuego
- Northern South America
- Western Gulf and western Pacific. 

As many of these individuals were banded at Delaware bay, this limits the subpops to SE_US, TDF, N_SA, while the WGWP group are less likely to be detected this far east. 



# # Breeding/extent locations: 
# 
# 
# ```{r}
# 
# a <- sort(unique(clean$tag.local.identifier ))
# 
# pvic <- c(213827, 213830, 213834 ,213836, 213838,  213841)
# 
# coats <- c(213831, 213835)
# 
# usa <- c(204359, 204362 ,213829, 213839, 213842, 224072, 224076, 224077 ,224078, 224079,224081,
#          224086 ,224087,224091 ,224092, 224094 ,224095,  224096,224098, 224100, 224101 , 224102,
#          224103)
# 
# cari <- c(204361, 224080, 224082, 224088,224097)
# 
# sth_am <- c(204351, 204352, 204357, 204364, 204369,  204370,  204375, 224073,  224075, 224083 ,
#             224085, 224089, 224093, 224099, 233781)
# 
# hudson <- c(213828, 213832, 213833, 213837)
# 
# odd <- c(204371, 213840)
# 
# 
# 
# # breeding local 
# 
# breedsf <- arf_sf %>% 
#   mutate(breed_class = case_when(
#     tag.local.identifier %in%  usa ~ "usa",
#     tag.local.identifier %in%  cari ~ "caribbean",
#     tag.local.identifier %in% sth_am ~ "sth_america",
#     tag.local.identifier %in%  hudson ~ "hudson",
#     tag.local.identifier %in%  coats ~ "coats_is",
#     tag.local.identifier %in%  pvic ~ "prince_vic",
#     tag.local.identifier %in%  odd  ~ "unknown",
#     .default = NA
#   ))
# 
# 
# # entire north America 
# breed_loc <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = breedsf, size = 1, colour = "dark blue") +
#   facet_wrap(~breed_class)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# breed_loc
# 
# 
# ```
# 
# 
# 

