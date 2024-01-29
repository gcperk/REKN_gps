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



