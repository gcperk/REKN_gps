# subpopulation detailed analysis and figures: per subpopulation 

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

# ggplot(sts_sum, aes(y=factor(tag.id))) +
#   geom_segment(aes(x=DayMonth_s , xend=DayMonth_e, y=factor(tag.id), yend=factor(tag.id)), linewidth = 3)+
#   xlab("Date") + ylab("Tag") 
# 
# 
# ggplot(sts_sum, aes(x=tag.id, y=stop_dur, fill=breed_type )) +
#   geom_bar(stat="identity")+
#   theme_bw(base_size=12)+
#   coord_flip()
# 

allsf <- st_read(file.path(raw_dat ,"xall_rekn_20240207.gpkg")) %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time))

all <- read.csv(file.path(raw_dat ,"xall_rekn_20240207.csv")) %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time))

tag_type <- all |> 
  dplyr::select(tag.id, tag.manufacturer.name, tag.model) |> 
  distinct()



###############################################################
# create a basic plot # figure 1 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = allsf, size = 1, colour = "darkblue" ) + #aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_color_viridis_d(option = "magma",begin = 0.1)+
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-180, -20), ylim = c(-60, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global

# create a subspecies plot 
susp <- allsf %>%
  dplyr::select(proj, date_time ) |> 
  mutate(subspecies = case_when(
    proj ==  "Johnson_GPS"  ~ "roselaari",
    .default = "rufa"
  ))


world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = susp, size = 1, colour = "darkblue" ) + #aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_color_viridis_d(option = "magma",begin = 0.1)+
  facet_wrap(~subspecies)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-180, -20), ylim = c(-60, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global













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

### Write out for review 

#write_sf(so, file.path(raw_dat, "stopover_summaries_testing.gpkg"), append = FALSE)
#write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))

## Get summary form min and max dates 
breeding_sum <- so |> 
  st_drop_geometry() |> 
  group_by(movement_dir, Subpopulations) |> 
  summarise(arrive_mindate = min(DayMonth_s), 
            arrive_maxdate = max(DayMonth_s))

# read in locations 

locals <- st_read(file.path(out_dat,"stopover_locations_named.gpkg")) %>% 
  st_drop_geometry() |> 
  select(name, country, state, region)

solo <- left_join(so, locals) |> 
  select(tag.id, move_event, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations, name, region, type)


#solo_breed <- solo %>% filter(movement_dir == "breeding")

#write_sf(solo_breed, file.path(raw_dat, "stopover_breed_testing.gpkg"), append = FALSE)


#####################################################

# create a plot of Stopover loctions

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = so, size = 2, aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_color_viridis_d()+
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-180, -20), ylim = c(-60, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global



#############################################################
# western pacific population 
#############################################################

wgwp <- so %>% filter(Subpopulations == "WGWP")

#length(unique(wgwp$tag.id))

#write_sf(wgwp , file.path(raw_dat, "stopover_summaries_wgwp_test.gpkg"))
#write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))

#length(unique(wgwp$tag.id))

move_type <- wgwp |> 
  st_drop_geometry() |> 
  select(tag.id, Catergory) |> 
  group_by(Catergory) |> 
  summarise(count = length(unique(tag.id)))

move_dur <- wgwp |> 
  select(tag.id, move_event, start , dur_days, movement_dir) %>%
  st_drop_geometry() |> 
  mutate(movement_dir_rc = fct_relevel(as.factor(movement_dir), 
                            "northward", "breeding", "southward")) 

# ggplot(move_dur, aes(move_event, tag.id, fill = move_event), colour = movement_dir) +
#   geom_col(aes(dur_days))

# Figure 1 - stopovers

ggplot(move_dur) + 
  geom_bar(aes(y=tag.id, x=dur_days, fill=movement_dir_rc ), colour="black", stat="identity")+
  scale_fill_viridis_d()+
  xlab("Duration (days)") + ylab("Tag id.")+
  theme(
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines"),
    legend.title=element_blank()
  )

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


## Breeding locations 
rf_sf_breed <- rf_sf#%>%
  #filter( movement_dir== "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf_breed, size = 3, aes(colour= movement_dir)) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(55, 79), expand = FALSE)+
  theme_bw()+
  labs(colour = "Type") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend.title = "", 
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global












# # date depart for spring migration 
# Texax
# 2021 May 18 - 201135 - via Prairies + hudson Bay 
# 2020 May 15 - 201139 - via Prairies returned 2020 July 19 
# 2023 May 24 - 228166 - via Prairies - returned august 9th
# 
# New Orleans
# 2021 May 18 - 201151 - via prairies
# 2021 May 19 - 201140 - via prairies
# 2021 May 21 - 201137 - short
# 
# 2021 May 20 - 201146 - via Hudson Bay - return 08 28th
# 2021 May 26 - 201150 - via Hudson Bay 
# 2021 May 28 - 201160 - via Hudson Bay 
# 2021 May 30 - 201159 - via Hudson Bay 
# 2021 May 30 - 201163 - via Hudson Bay 
# 2021 May 31 - 201143 - via Hudson Bay 
# 2021 May 30 - 201145 - via Hudson Bay 
# 2021 June 05 - 201165 - via Hudson Bay 
# 
# 
# 
# # date departing breeding area 
# 201146 - via Hudson Bay depart august 11th
# 228166 - via hudson Bay July 19th (return via hudson bay - arrive via prairies) 
# 201139 - via Prairies returned 2020 July 19 
# 
# 201143 - via Hudson Bay Sept 6th (might be dislodged tag as last ping)
# 201165 - via hudson bay Sept 10th (might be dislodged tag as last ping)


# # filter the tags which cant be used 
# cant_use <- bcat |> 
#   filter(Catergory == "Not usable") %>%
#   dplyr::select(tag.id) %>%
#   pull()
# 
# bd <- bdat %>% 
#   filter(!tag.id %in% cant_use)
# 
# bdd <- bd |> 
#   dplyr::select(location.long, location.lat, gps.fix.type.raw, lotek.crc.status,proj,
#                 argos.lc,  tag.model, date_time, year, month, day, hour, minute, tag.id)
# 
# bdd <- bdd %>% dplyr::select(location.long, location.lat, date_time, tag.id)
# 
# plot(bdd$location.long, bdd$location.lat, col = viridis_pal()(nrow(bdd)), pch = 20, 
#      xlab = "x", ylab = "y", asp = 1)
# 
# bddvisit = getRecursions(bdd, 25) 
# 
# par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
# plot(bddvisit, bdd, legendPos = c(13, -10))
# 







#############################################################
# tdf
#############################################################

tdf <- so %>% 
  filter(Subpopulations == "TDF")

#length(unique(tdf$tag.id))

# move_type <- tdf |> 
#   st_drop_geometry() |> 
#   select(tag.id, Catergory) |> 
#   group_by(Catergory) |> 
#   summarise(count = length(unique(tag.id)))

#write_sf(tdf, file.path(raw_dat, "stopover_summaries_tdf_test.gpkg"))

tdf_tag_type <- tag_type |> 
  filter(tag.id %in% tdf$tag.id)

tdf <- left_join(tdf, tdf_tag_type)

move_type <- tdf  |> 
  st_drop_geometry() |> 
  select(tag.id, Catergory) |> 
  group_by(Catergory) |> 
  summarise(count = length(unique(tag.id)))

move_dur <- tdf  |> 
  select(tag.id, move_event, start , dur_days, movement_dir, tag.model) %>%
  st_drop_geometry() |> 
  mutate(movement_dir_rc = fct_relevel(as.factor(movement_dir), 
                                       "northward", "breeding", "southward")) |> 
  filter(dur_days>1)

# ggplot(move_dur, aes(move_event, tag.id, fill = move_event), colour = movement_dir) +
#   geom_col(aes(dur_days))

ggplot(move_dur) + 
  geom_bar(aes(y=tag.id, x=dur_days, fill=movement_dir_rc ), colour="black", stat="identity")+
  scale_fill_viridis_d()+
  xlab("Duration (days)") + ylab("Tag id.")+
  theme(
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines"),
    legend.title=element_blank()
  )





# rf_sf <- sf::st_as_sf(clean, coords = c("location.long","location.lat"), crs = 4326, agr = "constant")
#st_write(rf_sf, file.path("output", "all_rekn_20230918.gpkg"))

rf_sf <- tdf %>%
  filter(Catergory != "static")

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")
# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
  scale_fill_viridis_d(option = "magma",begin = 0.1)+
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  #coord_sf(xlim = c(-130, -60), ylim = c(15, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global


## Breeding locations 
rf_sf_breed <- rf_sf#%>%
#filter( movement_dir== "breeding")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf_breed, size = 3, aes(colour= movement_dir)) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
  theme_bw()+
  labs(colour = "Type") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend.title = "", 
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global


## Southward 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf, size = 3,  aes( colour = movement_dir))+#colour = "dark blue") +
  scale_color_viridis_d()+
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  labs(colour = "Type") + 
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 80), expand = FALSE)+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend.title = "", 
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global














# 
# [1] "213830" "213835" "213841" "232982" "234375" "240155" "240157" "240158" "240159" "240160"
# [11] "240162" "240163" "240164" "240165" "240166" "240167" "241166" "241167"
# 
# 
# # fall migration - depart date 
# 240167 - depart Lago de Piexe - April 10 th and depart sth sth Am may 29th 2023
# 240158 - depart Lago de Piexe/ sth sth Am - April 13th 
# 240159 - depart Lago de Piexe/ sth sth Am - May 8th 2023
# 240166 - depart Lago de Piexe/ sth sth Am - May 8th 2023
# 241166 - depart Lago de Piexe/ sth sth Am - May 1st 2023
# 240164 - depart Lago de Piexe - April 11 th and depart sth sth Am April 27th 2023
# 241167 - depart Lago de Piexe - April 26 th and depart sth sth Am may 5th 2023
# 
# #Depart Nth Brazil 
# 240158 - depart Marahao - may 15th 
# 240159 - depart Marahao - may 24th
# 241166 - depart Marahao - may 27th
# 240164 - depart Marahao = may 27th 
# 241167 - depart New Amsterdam - May 25th
# 240165 - depart New Amsterdam = may 27th 
# 241167 - depart Amapa - May 30th 
# 
# #Depart US mainland 
# 213830 - depart del bay May 24th (2021)
# 213835 - Depart del Bay - june 1 (2021) - tagged in delbay (unknown arrival)
# 213841 - Depart del Bay - june 1 (2021) - tagged in delbay (unknown arrival)
# 232982 - Depart del Bay - May 30th (2023) - tagged in delbay (unknown arrival)
# 240156  - depart del Bay May 29 2023 - tagged in delbay (unknown arrival)
# 240159  - depart del Bay May 31 2023) (arrive 29th May) 
# 240158  - depart sthport (nth carolina)  june 7th 2023 (arrive 20th May) 
# 240161  - depart del bay - May 29 2023 - tagged in delbay (unknown arrival)
# 240164  - depart del bay - June 20th 2023 - multiple stops on mainland(arrive 2nd june)
# 240165  - depart del bay - June 15th 2023 - multiple stops on mainland (arrive 6th june)
# 240167  - depart del bay - June 11th 2023 - arrive june 4th
# 240166  - depart del bay - June 11th 2023 - arrive june 1st
# 240167  - depart del bay - June 8th 2023 - arrive May 27th 
# 
# 
# #Arrive/depart Hudson bay - noth bound 
# 213830 arrive HB May 26th - depart June 4th - 2021 9 days
# 240156 arrive HB May 31 - depart June 3th  (4 days)
# 240161 arrive HB may 31 and depart june 6th (mutliple stops)- 2023 (7 days)
# 240158 arrive HB June 1 and depart June 6th - 2023 (6 days)
# 232982 arrive HB June 3rd - depart June 7th (5 days)
# 213835 arrive HB June 04 - depart June 8th  (4 days)
# 213841 arrive HB June 6th - depart june 12 (multiple stops) - 2021 - 6 days
# 240159 arrive HB june 10 and depart June 15 (multiple stops)- 2023 - 5 days
# 240167 arrive HB june 14th and depart june 21 (multiple stops)- 2023 - 7 days
# 240166 arrive HB june 14th and depart june 19 (multiple stops) - 2023 - 5 days 
# 241167 arrive HB june 15th and depart june 18 (multiple stops) - 2023 - 4 days
# 240164 arrive HB june 21 and depart June 25 *multiple stops) - 2023 - 4 days 
# 
# # arrive/depart breeding grounds 
# 232982 arrive Victoria is June 9th and July 16 th depart is 
# 240158 arrive Victoria is June 12th - til ?
#   213841 arrive Victoria is june 14th - Sept 19 (tag died ?())
# 240159 arrive Victoria is June 17th and july 11 (last transmit)
# 213830 arrive Victoria is June 20th to Sept 8th (tag died ?())
# 240167 arrive Victoria is june 22 and depart July 26th (full record)
# 240166 arrive Victoria is june 22 and depart July 4th (tag died)
# 
# 213835 arrive Coasts Is june 14th - july 2nd (tag died?) 
# 240156 arrive qikiqtarjuaq Island June 6th - til August 19 (tag died) 
# 240161 arrive Prince Charles Is june 13th - depart is July 21st (full record)
# 240164 arrive Prince of wales island june 29th - depart july 21 (full record)
# 
# 
# # arrival stopover HB
# 232982 - arrive HBay (July 21st - August 10 th) - direct to Sth Am 
# 240161 - from Prince charles arrive (july 26th - August 15th)- via sth hamp, hb (multiple stops) - different stops on retunr than towards                                   
# 240164 -  from Vic Is = (August 18 - Sept 23)  -multple stops in HB - ends here 
# 240167 - from Vic is = single stop in hudson By (July 27th - August 8th)
# 
# # stopover delaware bay                   
# 234375 -  depart delaware bay August 31 - banded here - direct to sth am ()                     
# 240167 - from hudson bay (August 12 - 31th) the depart to Guyana 
# 
# 
# 
# 232982 - VicIs - Hudson Bay -                  - Marahoa - sth Brazil (multiple stops)
# 234375 -                      DEL BAY (multiple) - GUYANA - sth Brazil
# 240167 - VicIS - hudson Bay - DELBAY (multiple) -  GUYANA(multiple stops) - sth Brazil(multiple stops)
# 241167 - VicIS -hudson Bay (multiple) -       - GUYANA
# 
# 
# # arrive nth sth americs                      
# 232982 - arrive  stheast maranhao (August 17th - Sept 28th) - diredct from HB         
# 240167 - arrive October 8th - stayed to at least Dec 19th (tag ran out) - Reserva natural bahia San Blas (
#   234375 - arrive Guyana Sept 4th - Sept 26th direct from nth america                   
#   
#   
#   # arrive wintering grouds 
#   232982 - arrive  15th Oct (mutliple stop sth brazil/ argentina) - tag died
#   232982  - arrive Mar del plata October 1th - from Nth Sth Am.    
#   
#   
#   
#   # 
#   # hist(bddvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 2)")
#   # summary(bddvisit$revisits)
#   # 
#   # head(bddvisit$revisitStats)
#   # 
#   # 
#   # par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
#   # plot(martinvisit, martin, legendPos = c(13, -10))
#   # 
#   # 
#   # 
#   # 
#   # 
#   # 
#   # 
#   # 


  
  #############################################################
  # South East and Caribean 
  #############################################################
  
  se <- so %>% 
    filter(Subpopulations == "SE")
  
  length(unique(tdf$tag.id))
  # 
  # move_type <- se |> 
  #   st_drop_geometry() |> 
  #   select(tag.id, Catergory) |> 
  #   group_by(Catergory) |> 
  #   summarise(count = length(unique(tag.id)))
  
  #write_sf(se, file.path(raw_dat, "stopover_summaries_se_test.gpkg"))
  #write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))
  
  se_tag_type <- tag_type |> 
    filter(tag.id %in% se$tag.id)
  
  se <- left_join(se, se_tag_type)
  
  move_dur <- se  |> 
    select(tag.id, move_event, start , dur_days, movement_dir, tag.model) %>%
    st_drop_geometry() |> 
    mutate(movement_dir_rc = fct_relevel(as.factor(movement_dir), 
                                         "northward", "breeding", "southward")) 
  
##Stopping plots 
  
  ggplot(move_dur) + 
       geom_bar(aes(y=tag.id, x=dur_days, fill=movement_dir_rc ), colour="black", stat="identity")+
       scale_fill_viridis_d()+
       xlab("Duration (days)") + ylab("Tag id.")+
       theme(
         axis.ticks = element_blank(),
           legend.position = "bottom",
           legend.key.width = unit(3, "lines"),
           legend.title=element_blank()
         )

 # general plot 

  rf_sf <- se %>%
    filter(Catergory != "static")
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  Americas <- world %>% dplyr::filter(region_un == "Americas")
  global <- ggplot(data = Americas) +
    geom_sf(color = "grey") +
    geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
    scale_color_viridis_d()+
    #facet_wrap(~tag.id)+
    # geom_point(ru, aes(x = lng, y = lat), size = 4) +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(-130, -40), ylim = c(5, 80), expand = FALSE)+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      #axis.title = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(3, "lines")
    )
  
  global
  
  
  ## Breeding locations 
  rf_sf_breed <- rf_sf
  
  # entire north America 
  global <- ggplot(data = Americas) +
    geom_sf(color = "grey") +
    geom_sf(data = rf_sf_breed, size = 3, aes(colour= movement_dir)) +#colour = "dark blue") +
    scale_color_viridis_d() + 
    coord_sf(xlim = c(-125, -60), ylim = c(50, 79), expand = FALSE)+
    theme_bw()+
    labs(colour = "Type") + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      #legend.title = "", 
      legend.position = "bottom",
      legend.key.width = unit(3, "lines")
    )
  
  global
  
  
  ######################################################################
  
  ## Nth Bound 
  ###  Depart US 
#213831 - delaware bay = May 28th 
#213834- delaware bay = June 1sy 
#213834- delaware bay = June 1sy  
#242656 - Kiawah Bay May 10th to 21st 
  
#213831 - Hudson Bay East June 1 to June 6th 
#213834 - Hudson Bay East June 6th to June 10th 
#242656 - Hudson Bay June 3rd - June 6th
  

# breeding
#213834 - Vic Is June 20th July 12th
#242656 - Vic Is June 10 - August 3rd 
  
  
#Arrive Hudson Bay 
#242656 - August 8th - August 16th 

  
#Retunr sth from Hbay to US coastline
#242656 - US Coast August 18th 
  
  
  # sth bound  caribean islands 
# 224080 - Spet 14th - 20th 
# 224082 - Cuba - Sept 13th - 24th  
# 224088 - Cuba - Sept 1st- 26th
  
  
  
  
  
  
  
  #############################################################
  # Northern Sth America 
  #############################################################

  sth <- so %>% filter(Subpopulations == "nth_sthAm")
  
  write_sf(sth , file.path(raw_dat, "stopover_summaries_nthstham_test.gpkg"))
  #write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))
  
  #length(unique(wgwp$tag.id))
  
  move_type <- sth |> 
    st_drop_geometry() |> 
    select(tag.id, Catergory) |> 
    group_by(Catergory) |> 
    summarise(count = length(unique(tag.id)))
  
  move_dur <- sth |> 
    select(tag.id, move_event, start , dur_days, movement_dir) %>%
    st_drop_geometry() |> 
    mutate(movement_dir_rc = fct_relevel(as.factor(movement_dir), 
                                         "northward", "breeding", "southward")) 
  ##Stopping plots 
  
  
  ggplot(move_dur) + 
    geom_bar(aes(y=tag.id, x=dur_days, fill=movement_dir_rc ), colour="black", stat="identity")+
    scale_fill_viridis_d()+
    xlab("Duration (days)") + ylab("Tag id.")+
    theme(
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(3, "lines"),
      legend.title=element_blank()
    )
  
  # general plot 
  
  rf_sf <- sth %>%
    filter(Catergory != "static")
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  Americas <- world %>% dplyr::filter(region_un == "Americas")
  global <- ggplot(data = Americas) +
    geom_sf(color = "grey") +
    geom_sf(data = rf_sf, size = 3,  aes(fill = movement_dir, colour = movement_dir))+#colour = "dark blue") +
    scale_color_viridis_d()+
    #facet_wrap(~tag.id)+
    # geom_point(ru, aes(x = lng, y = lat), size = 4) +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(-130, -20), ylim = c(-20, 80), expand = FALSE)+
    theme_bw()+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      #axis.title = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(3, "lines")
    )
  
  global
  
  
  ## Breeding locations 
  rf_sf_breed <- rf_sf
  
  # entire north America 
  global <- ggplot(data = Americas) +
    geom_sf(color = "grey") +
    geom_sf(data = rf_sf_breed, size = 3, aes(colour= movement_dir)) +#colour = "dark blue") +
    scale_color_viridis_d() + 
    coord_sf(xlim = c(-125, -60), ylim = c(45, 79), expand = FALSE)+
    theme_bw()+
    labs(colour = "Type") + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      #legend.title = "", 
      legend.position = "bottom",
      legend.key.width = unit(3, "lines")
    )
  
  global
  
  
  ## Nth Bound 
  ###  Depart US 
  #232981- delaware bay = May 29th 
  #242583 delaware bay = May 27th 
  #248544 delaware bay = May 28th 
  
  
  #Arrive Hudson Bay 
  
# 232981 - May 30 - June 6th 
# 238544 - May 30 - June 6th 
# 242580 - May 29th June 3rd 
# 242583 - May 30 - June 11th 
  
  
# breeding
# Vic is - June 13th - Jult 27th 
# matty Is - June 5th - July 30th 
# Sthhamnpt - June 7th to August 6th 
# baffin - June th - July 19th 
  

# Arrive Hudson Bay 
# 242580 -  August 3rd - 16th 
#  242583 - July 27th  - August 15th 
  
#Retunr To delaware bay  
#232981- delaware bay  = august 15 - 27th 
#242583 delaware bay = August 17 - 27th 
#248544 delaware bay = August 18 - 28th 
  
  
#Sth from mingang 
# 229363 - August 30 to Sept 1th 
#232352 - August 30th to Sept 19th 
# 23544 - August 13th to Sept 19th  
# juveniles depart Early November between 2nd and 9th. 
  

# 
#arrive in Nth Sth Am 
#Arrive August 18th, 22nd, 30, 
# Sept 7, 7th 30th, 

  
  
#############
# Still unknown
#########
  
  
  
  un <- so %>% 
    filter(Subpopulations %in% c("unsure", "SE_nth_sthAm", "nth_sthAm_TDF","unsure(se?)", "SE_nth_sthAm_TDF"))
  
  write_sf(un, file.path(raw_dat, "stopover_summaries_unknown_test.gpkg"))
  #write_sf(stsf, file.path(raw_dat, "stopover_summaries_dirs.gpkg"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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





##########################################################

# Subpopulations

###########################################################



# Geographic distribution of all tracks with estimated groupings: 


- South_america n(22)
- Caribbean n(8)
- usa (70)
- west_arctic (37)
- east arctic (26)
- tdf (7)


These are estimateed groupings based on potential analysis questions. 



a <- sort(unique(clean$tag.local.identifier ))

sth_am  <- c(204351, 204352, 204357, 204359,204364
             ,204369 ,204370, 204371, 204375, 224073, 224075,224083, 224085,
             224089, 224093, 224099, 233781, 233926  ,233928 ,236445,236450 ,236451)

#length(sth_am)

cari <- c(204361, 224080, 224082,224088 ,224097 ,233919,  236444, 236447)

#length(cari)

usa <- c(204362, 213839, 213842,221841 ,221843, 221844 ,221845, 221846 ,221847, 221850, 221852,
         221854 ,221856, 221858, 221860 ,221863, 221866, 221867 ,224072,
         224076 ,224077, 224078, 224079, 224081, 224086, 224087, 224091, 224092, 224094 ,224095, 224096, 224098, 224100, 224101, 224102,224103, 
         230306 ,230319, 232980 ,233918, 233920, 233921, 233922, 233923 ,233924, 233925,
         233929 ,233930, 233931 ,233932, 234177, 234178, 234179, 234180 ,234181, 234182, 234183 ,234184,
         234185 ,234187, 234189 ,234190, 234191,
         234235 ,236446, 236452, 238542, 238543, 240169, 242571)

#length(usa)

west_arctic <- c(213827, 213830,213834, 213836,213838, 213841, 230303, 230304,
                 230310, 230311, 230314, 230317, 230318, 230320, 232982, 232985, 232986, 233927,
                 234233, 234234, 234237,
                 234239, 234240, 236453, 238546 ,240156, 240158, 240159 ,240167, 240168 ,240164, 241166 ,241167, 242573, 242574, 242577,230301)

#length(west_arctic)

east_arctic <- c(213828, 213829, 213831, 213832, 213833 ,213835, 213837, 213840, 230299, 230300, 230302,
                 230307,230308 ,230309, 230312 ,230315 ,230316,
                 232981,
                 234236 , 234238 ,236448, 236449 ,238544 ,240161 ,242570 ,242580) 

#length(east_arctic)

tdf <- c(240155,  240157, 240160, 240162 ,240163, 240165 ,240166)

#length(tdf) 

# breeding local 

breedsf <- rf_sf %>% 
  mutate(breed_class = case_when(
    tag.local.identifier %in%  tdf ~ "tdf",
    tag.local.identifier %in%  west_arctic ~ "west_arctic",
    tag.local.identifier %in% usa ~ "usa",
    tag.local.identifier %in%  sth_am ~ "South_america",
    tag.local.identifier %in%  east_arctic ~ "east_arctic",
    tag.local.identifier %in%  cari ~ "caribbean",
    .default = NA
  ))


# entire north America 
breed_loc <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = breedsf, size = 1, colour = "dark blue") +
  facet_wrap(~breed_class)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

breed_loc


```









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



