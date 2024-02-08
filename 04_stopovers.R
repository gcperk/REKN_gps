library(sp)
library(sf)
library(ggplot2)
library(stringr)
library(readxl)
library(foreach)
library(tidyverse)
library(dplyr)
library(geosphere)
require(recurse)
require(scales)
library(adehabitatLT)

raw_dat <- file.path("output")
out_dat <- file.path("output", "report")

# read in the final data 

all <- read.csv(file.path(raw_dat ,"xall_rekn_20240207.csv"))

# generate a direction subset to merge edited file 

sub_dir <- all |> 
  dplyr::select(tag.id, location.long, location.lat, date_time,  "gps.fix.type.raw", 
                "lotek.crc.status", "argos.lc","year", "month", "day" , "hour", "minute",                
                "diff" , "location.long_prior","location.lat_prior","gcd_m",                  
                "bearing" , "speed_mhr")            






# join the manual edits together and merge to the main dataset 

manual_edits <- list.files(file.path(raw_dat, "manual_edited_complete", "final"))

man1 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_dom_mig_20240123.gpkg"))
man2 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_john_mig_20240123.gpkg"))
man3 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_ma_mig_20240123.gpkg"))
man4 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_mils_mig_20240123.gpkg"))
man5 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_newstead_mig_20240123.gpkg"))
man6 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_oceanwinds_20240123.gpkg"))
man7 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_qu_20240128.gpkg" ))
man8 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_sthcarolina_20240113.gpkg" ))
man9 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_eccc_20240207.gpkg" ))
man10 <- st_read(file.path(raw_dat, "manual_edited_complete", "final", "rekn_atlantic_20240207.gpkg" ))

man_out <- bind_rows(man1, man2, man3, man4, man5, man6, man7, man8, man9, man10) %>% 
  cbind(st_coordinates(.))%>%
  rename(location.lat = Y, 
         location.long = X)

man_out <- man_out |> 
  dplyr::select(tag.id, location.long, location.lat, date_time,  "gps.fix.type.raw", 
                "lotek.crc.status", "argos.lc","year", "month", "day" , "hour", "minute",                
                "diff" , "location.long_prior","location.lat_prior","gcd_m",                  
                "bearing" , "speed_mhr")            












#read in the raw data and match to the catergorised data sets




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













# subset with carolina data 

bdd <- bdd %>% filter(proj == "sthcarolina_arctic") %>%
  dplyr::select(-proj, -tag.model, -gps.fix.type.raw, -lotek.crc.status)

# note keeping as many points as possible here 
# can be filtered later on the values of A/B/3 etc..

#ggplot(bdd, aes(argos.lc)) +
#  geom_bar()#, position = position_stack(reverse = TRUE))



# Duration between pings/ 
bdd <- bdd |> 
  mutate(ddate = ymd_hms(date_time)) |> 
  arrange(tag.id, ddate)

bdd_dur <- bdd  |> 
  #mutate(time = as.POSIXct(date_time, format = "%y/%d/%m %H:%M:%S")) |> 
  group_by(tag.id) |> 
  mutate(diff = difftime(ddate, lag(ddate),  units = c("mins")), 
         diff = as.numeric(diff))%>%
  mutate(diff >0)


type_dur <- bdd_dur |> 
  dplyr::select(tag.id, argos.lc, diff) |> 
  group_by(argos.lc, tag.id) |> 
  summarise( n =  n(), 
             min = min(diff, na.rm= T),
             max =max(diff, na.rm = T), 
             mean = mean(diff, na.rm = T))










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



