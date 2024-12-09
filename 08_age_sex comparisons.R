# Age comparisons 



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



#library(geosphere)
#require(scales)
#library(adehabitatLT)

raw_dat <- file.path("output")
out_dat <- file.path("output", "report")


# Edits and calculations 

sts <- st_read(file.path(out_dat, "stopover_locations.gpkg")) %>%
  dplyr::mutate(stop_dur = round(difftime( end, start,  units = c("days")),1)) %>% 
  dplyr::mutate(dur_days = as.numeric(stop_dur)) 

sts <- sts |> 
  mutate(DayMonth_s = format(as.Date(start ), "%d-%m"),
         DayMonth_e = format(as.Date(end), "%d-%m"))

# capture locations 
cap <- st_read(file.path(raw_dat,"maps", "capture_locations.gpkg")) %>%
  st_drop_geometry()%>%
  select(tag.id, animal.life.stage, animal.sex) %>% 
  group_by(tag.id) |>
  slice_head( n = 1) |> 
  ungroup()


######################################################################
## Filter tags which cant be used 

bcat <- read.csv(file.path(out_dat,"proj_animalid_edited.csv")) %>%
  mutate(tag.id = as.character(tag.id)) 

bcat <- left_join(bcat, cap, by = "tag.id")

so <- left_join(sts, bcat, by = "tag.id")

so <- so |> 
  rowwise() |> 
  mutate(movement_dir = direction) |> 
  mutate(movement_dir = ifelse(breed_type %in% c("wintering", "breeding"), breed_type, movement_dir))

#######################
# read in locals 

locals <- st_read(file.path(out_dat,"stopover_locations_named.gpkg")) %>%
  st_drop_geometry() |>
  select(name, country, state, region)

solo <- left_join(so, locals)|>
  select(tag.id, DayMonth_s,DayMonth_e, start, end, movement_dir, dur_days, Subpopulations, name, region, type,  "animal.life.stage" ,"animal.sex" )|> 
  mutate(Subpop_class = case_when(
    Subpopulations %in% c("SE_nth_sthAm_TDF", "unsure" ,"Not usable" , "NA" ,NA,"unsure(se?)", "SE_nth_sthAm" ) ~ "unknown", 
    .default = as.character(Subpopulations)))




######################

## Age distribution 

######################

hy <- solo |> 
  filter(animal.life.stage =="HY") |> 
  filter(type != "static")


sum_hy <-solo %>% 
  filter(animal.life.stage =="HY") |> 
  select(tag.id, type) %>% 
  st_drop_geometry() |> 
  distinct()


sum_hy$tag.id
# 
# Delaware bay 
# "221847" - NOv banded - ocean winds - local movement USA
# "221860" - NOv banded - ocean winds - local movement USA
# "224096" - August - atlantic - static
# "233921" - Sept - ocean winds -- static
# "233927" - Sept - ocean winds -- static
# "233930" -  Sept - ocean winds -- local movement USA
# "234185"-  Sept - ocean winds -- local movement USA
# 
# MOnomoy 
# "234370" - August -Oceanwinds _ to bahamas - late departure
# 
# MIngagn 
# "229364" - August band  - depart Nov - Nthstham 
# "229365"  - August band  - depart Nov - NthSth
# "229368" - August band - depart Nov - NthSth
# "229366" - August band  - static
# "229369" - August band - static
# "229370" - August band - static
# "232345" - August band - depart Nov - NthSth
# "232348" - August band - depart Nov - NthSth
# "232349"- August band - depart Nov - NthSth
# "232350" - August band - static
# "232351" - August band - depart Nov - NthSth
# "232353" - August band - depart Nov - NthSth
# "239408" - August bank - depart Nov - NthSth
# "239409" - August bank - depart Nov - NthSth
# "239410" - August band - depart Nov - NthSth
# "239411" - August band - depart Nov - NthSth
# "239412" - August band - depart Nov - NthSth
# "239413" - August band - depart Nov - NthSth
# "239415"  - August band - static
# "239416" - August band - depart Nov - NthSth
# "239417"  - August band - static
# "239418" - August band - static
# "239419" - August band - depart Nov - NthSth
# "239420" - August band - depart Nov - NthSth
# "239421" - August band - depart Nov - NthSth
# "239422" - August band - depart Nov - NthSth
# "239423" - August band - depart Nov - NthSth
# "239424"- August band - depart Nov - NthSth
# "239425"- August band - depart Nov - NthSth
# "242698" - August band - depart Nov - NthSth
# "242702" - August band - depart Nov - NthSth
# "242703" - August band - static
# 
# 
# ## map out the HY 
# 
# rf_sf <- hy
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# Americas <- world %>% dplyr::filter(region_un == "Americas")
# #Americas <- world %>% dplyr::filter(continent == "North America")
# # entire north America 
# global <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
#   scale_fill_viridis_d(option = "magma",begin = 0.1)+
#   facet_wrap(~tag.id)+
#   # geom_point(ru, aes(x = lng, y = lat), size = 4) +
#   xlab("Longitude") + ylab("Latitude") +
#   coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
#   #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global



######################

## Sex distribution 

######################

mf <- solo |> 
  filter(animal.sex %in% c("M", "F"))

mfs <- mf |> 
  select(tag.id, animal.sex) |> 
  st_drop_geometry() |> 
  distinct() 


unique(mf$tag.id)

"229363" - AHY - diff - (M) 2022-09-01
"229364" - HY - (F)
"229365" - HY
"229368" - HY
"229366"  - diff (M) static quebec
"229369"  - diff (M) static quebec
"229370" - diff (M) static quebec
"232345" - HY
"232348" - HY (F)
"232349" - HY (F)
"232350" - diff (M) static quebec
"232351" - HY (F)
"232352" - diff (M) depart 2022-09-018
"232353" - HY (F)



allsf <- st_read(file.path(raw_dat ,"xall_rekn_20240207.gpkg")) %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time)) |> 
  filter(tag.id == 232352) %>%
  select(tag.id, date_time)

## map out odd individual 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = allsf, size = 2, colour = "dark blue") +
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-90, -40), ylim = c(0, 60), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

