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

bdat <- read.csv(file.path(raw_dat ,"xsubset_rekn_20240123.csv"))

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






############################################################################
## Calculate distance between points and bearing

bdd_det <- bdd_dur  |> 
  #filter(tag.id == 230318) |> 
  group_by(tag.id) |> 
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L)) %>%
  rowwise() %>%
  mutate(gcd_km = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
         speed_kmhr = round((gcd_km/diff)/1000,1))






#  boxplot((speed_kmhr) ~ month, data = bdd_det, 
#          ylim = c(0, 1000), 
#          ylab = "km/day", xlab="day of year", pch = 19, cex = 0.5, col = rgb(0, 0, 0, 0.5))
 
#aa <- aa[1:500,]
# 
# ggplot(subset(aa, speed_kmhr>0), aes(x = month, y = speed_kmhr)) + 
#   facet_grid(~tag.id) + geom_point(alpha = 0.3) + 
#   stat_smooth() + scale_y_log10() +
#   ylab("Speed (km/hr)") +  xlab("Hour of day")



# Assign preliminary breeding and migration routes 

bdd_det <- bdd_det %>% 
  group_by()

















# Extra notes 

# alternatively using the adehabitat analysis - howver this seems to drop some points? 

 aa <- bdd_dur  %>%
   filter(diff > 0) %>%
   #filter(tag.id == 230318) %>%
   dplyr::select(location.long, location.lat,ddate, tag.id) #%>% 
  # group_by(date_time)%>%
  # slice_max(date_time) %>%
  # distinct()  # select the max value of date_time, per animal.id
 
 aa$x = aa$location.long 
 aa$y = aa$location.lat
 
 turtles.ltraj <- as.ltraj(xy = aa[,c("x","y")], 
                           date = aa$ddate,
                           id = aa$tag.id)
 
 head( turtles.ltraj[[1]]) 
 
 plot(turtles.ltraj)
 
 #i<- fpt( turtles.ltraj, seq(300,1000,length=30))
 i<- fpt( turtles.ltraj,
          seq(0.01,1,length=30))
 
 
 varlogfpt(i,graph=TRUE)

 
 #distance,between successive locations (meters),
 #dt (the difference in seconds between relocations),
 #R2n (the squared distance between the first relocation of the trajectory and the current relocation), and
 #two angle measurements (absolute and relative). relative and absolute turning angles (in radians),
 # These are relative turn angles, with values between -180 and 180 degrees, 
 # meaning that they are calculated from the previous bearing of the animals. 
 # These are different than absolute turning angles, which are not related to the previous bearing.
 # It automatically calculates the distance between successive locations (meters), 
 # relative and absolute turning angles (in radians), and time interval between successive locations (in seconds)
 
 
 
# # generate track data 
# data(capreochiz) 
# head(capreochiz) 
# ##Createanobjectofclass"ltraj" 
# cap<-as.ltraj(xy=capreochiz[,c("x","y")],
#               date=capreochiz$date, 
#               id="Roe.Deer",
#               typeII=TRUE, 
#               infolocs=capreochiz[,4:8]) 
# 
# data(puechcirc) 
# i<-fpt(puechcirc,seq(300,1000,length=30))
# plot(i,scale=500, warn=FALSE) 
# toto<-meanfpt(i) 
# toto 


attr(toto,"radii") 
toto<-varlogfpt(i) 
toto 
attr(toto,"radii")
 
 




# Geographic distributon of tags

```{r, echo=FALSE}

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


```


# Geographic distribution of all tracks with estimated groupings: 


- South_america n(22)
- Caribbean n(8)
- usa (70)
- west_arctic (37)
- east arctic (26)
- tdf (7)


These are estimateed groupings based on potential analysis questions. 


```{r}

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



