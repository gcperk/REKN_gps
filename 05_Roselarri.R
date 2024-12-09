library(lubridate)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)
library(stringr)
library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("magrittr")
library(leaflet)
library(RColorBrewer)
library(readxl)
library(foreach)
library(tidyverse)
library(viridis)
library(dplyr)
library(mapview)
library("rnaturalearth")
library("rnaturalearthdata")



#library(geosphere)
#require(scales)
#library(adehabitatLT)

raw_dat <- file.path("output")
out_dat <- file.path("output", "report")


## read in all data 

allsf <- st_read(file.path(raw_dat ,"xall_rekn_20240207.gpkg")) %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time))

all <- read.csv(file.path(raw_dat ,"xall_rekn_20240207.csv")) %>%
  mutate(tag.id = as.character(tag.id),
         date_time = ymd_hms(date_time))

tag_type <- all |> 
  dplyr::select(tag.id, tag.manufacturer.name, tag.model) |> 
  distinct()
















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
# create a basic plot 

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

source("01_load_data.R")

# define folder structire
data.dir <- file.path ("data", "location_estimates")
out.dir <- file.path("outputs")
out.plots <- file.path("outputs", "final")

# Rufa data sets
rekn <- readRDS(file.path(out.dir, "BNP_rekn_summary.rds")) 
#rekngps <- readRDS(file.path(out.dir, "newstead_rekn_gps.RDS" ))

# Rose datasets
rose <- readRDS(file.path(out.dir, "johnson_rose_daily.RDS"))
#rosegps <- readRDS(file.path(out.dir, "johnson_rose_gps.RDS"))

rose <- rose %>% dplyr::select(animal.id, Subpop,lng, lat, arrive, depart, year,arr_month,dep_month, dur_no, data_type)
#rosegps <- rosegps %>% dplyr::select(animal.id, Subpop,lng, lat, arrive, depart, year,arr_month,dep_month, dur_no,data_type)
#rose_extra <- rose_extra %>% dplyr::select(animal.id, Subpop,lng, lat, arrive, depart, year,arr_month,dep_month, dur_no,data_type)


#rr <- st_as_sf(rose, coords = c("lng", "lat"), crs = 4326)

#xx <- st_distance(rr)

rkall <- rose %>%
  dplyr::mutate(subspecies = "roselaari",
                Subpop1 = "roselaari")




######################################################################
##Basic plotting 

rkall <- rkall[complete.cases(rkall), ]
rf_sf <- st_as_sf(rkall, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(region_un == "Americas")
#Americas <- world %>% dplyr::filter(continent == "North America")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  # geom_sf(data = rf_sf, size = 1, colour = "blue", alpha = 0.5)+
  #ggplot(rkall, aes(x=lng, y=lat) ) +
  geom_bin2d(data = rkall, aes(x=lng, y=lat), bins = 90) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-170.15, -80), ylim = c(0, 80), expand = FALSE)+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

global 

jpeg(file.path(out.plots,"rose_density.jpg"), width = 30, height = 30,units = "cm", res = 210)
# 2. Create the plot
global 
# 3. Close the file
dev.off()


# ggplot(rf_sf, aes(x=lng, y=lat) ) +
#   geom_hex(bins = 70) +
#   scale_fill_continuous(type = "viridis") +
#   theme_bw()
# 
# ggplot(rkall, aes(x=lng, y=lat) ) +
#   geom_bin2d(bins = 70) +
#   scale_fill_continuous(type = "viridis") +
#   theme_bw()


##################################################################
# plot per month: 


rose <- rkall %>% 
  filter(!is.na(lat)) %>%
  #filter(arr_month %in% c(4,5,6,7,8,9,10,11)) %>%
  dplyr::rename("subpopulation" = Subpop)


rf_sf <- st_as_sf(rose, coords = c("lng","lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% 
  dplyr::filter(region_un == "Americas")

global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_sf(data = rf_sf,colour = "dark blue", alpha = 0.5, size = 0.7) +
  #geom_bin2d(data = rkall, aes(x=lng, y=lat), bins = 50) +
  facet_wrap(~arr_month, nrow = 3)+
  #scale_color_viridis_d() +
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim =  c(-170.15, 0), ylim = c(0, 80), expand = FALSE)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position= 'none' )
#legend.text=element_text(size=12))+
#guides(color = guide_legend(override.aes = list(size = 6,alpha = 1)))

global 

jpeg(file.path(out.plots,"rose_subpop_month_facet.jpg"), width = 30, height = 35,units = "cm", res = 210)
# 2. Create the plot
global 
# 3. Close the file
dev.off()
#ggsave(file.path(out.dir, "rufa_subpop_month_facet.jpg"))

################################################################


global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  #geom_sf(data = rf_sf,colour = "dark blue", alpha = 0.5, size = 0.7) +
  geom_bin2d(data = rkall, aes(x=lng, y=lat), bins = 50) +
  facet_wrap(~arr_month, nrow = 3)+
  scale_color_viridis_d() +
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim =  c(-170.15, 0), ylim = c(0, 80), expand = FALSE)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position= 'none' )
#legend.text=element_text(size=12))+
#guides(color = guide_legend(override.aes = list(size = 6,alpha = 1)))

global 

jpeg(file.path(out.plots,"rose_density_month_facet.jpg"), width = 30, height = 25,units = "cm", res = 210)
# 2. Create the plot
global 
# 3. Close the file
dev.off()
#ggsave(file.path(out.dir, "rufa_subpop_month_facet.jpg"))





#####################################################################


# leaflet mapping 


source("01_load_data.R")

# define folder structire
data.dir <- file.path ("data", "location_estimates")
out.dir <- file.path("outputs")
out.plots <- file.path("outputs", "final")


# Rose datasets
rose <- readRDS(file.path(out.dir, "johnson_rose_daily.RDS"))

rose <- rose %>% dplyr::select(animal.id, Subpop,lng, lat, arrive, depart, year,arr_month,dep_month, dur_no, data_type)

rkall <- rose %>%
  dplyr::mutate(subspecies = "roselaari",
                Subpop1 = "roselaari")

rkall <- rkall[complete.cases(rkall), ]
rf_sf <- st_as_sf(rkall, coords = c("lng","lat"), crs = 4326, agr = "constant")



bdat_sp <- rkall

palette1 <- colorNumeric(palette = 'viridis', bdat_sp$arr_month, reverse = TRUE)

pal <- colorFactor(
  palette = "viridis",
  domain = unique(bdat_sp$animal.id))

tags <- unique(bdat_sp$animal.id)

birdmapall <- leaflet(bdat_sp) %>%
  # add a dark basemap
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = bdat_sp$lng, lat = bdat_sp$lat, 
                   weight = 3, color = ~palette1(bdat_sp$arr_month), 
                   fill = TRUE,
                   label = ~arr_month,
                   #radius = ~dur_no/10,
                   fillColor = ~palette1(bdat_sp$arr_month)) %>%
  #popup = ~animal.id) %>%
  addPolylines(data = bdat_sp, lng = bdat_sp$lng, lat = bdat_sp$lat, 
               color = "grey",  opacity = 0.1, stroke = TRUE) %>%
  addLegend("bottomleft", pal = palette1, values = ~bdat_sp$arr_month,
            title = "Arrival Month",
            opacity = 1)

birdmapall












############################################################################################

## Roselarri

############################################################################################

arose <- rose %>% dplyr::select("animal.id","data_type", "arrive" ,"year" , "arr_month",            
                                "lat", "lng", "dur_no" ,"Subpop")

rosegps <- rosegps %>% dplyr::select("animal.id","data_type", "arrive" ,"year" , "arr_month",            
                                     "lat", "lng", "dur_no" ,"Subpop")

arose <- bind_rows(arose, rosegps)

jdatxy <- arose[c("lng","lat")] %>% filter(!is.na(lat)) 

# run kde

tdata <- data.frame(jdatxy)

kde <- bkde2D(tdata,
              bandwidth=c(.5, .5), gridsize = c(1000,1000)) # need to adjust this
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)       


## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])               




# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(tdata) <- c("lng", "lat")
proj4string(tdata) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
tdfgeo <- spTransform(tdata, CRS("+init=epsg:4087")) # Transform to UTM


kdehref500 <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)
kderef1000 <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 1000, extent = 2)

p = 95
kde <- kdehref1000 
ver <- getverticeshr(kde, p)
ver.sf <- st_as_sf(ver)

kdehref500_95 <- ver.sf
kdehref1000_95 <- ver.sf
mapview::mapview(kdehref1000_95 )
mapview::mapview(kdehref500_95 )
# doesnt work very well - issue with end of map/ leaflet 



kde <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)
kde1 <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 1000, extent = 2)
kde2 <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 10000, extent = 100)


lscv_1000_10 <- kde2
lscv_500_2  <- kde
lscv_1000_100 <- kde2

p = 50
kde <- lscv_1000_100 
ver <- getverticeshr(kde, p)
ver.sf <- st_as_sf(ver)

kdehref500_95 <- ver.sf
kdehref1000_95 <- ver.sf
mapview::mapview(ver.sf )
mapview::mapview(kdehref500_95 )
# doesnt work very well - issue with end of map/ leaflet 





saveRDS(kde, file = file.path(out.dir, paste0("rose_lscv_model.rds")))

hrpc = c(95)

for (p in hrpc){
  tryCatch({
    ver <- getverticeshr(kde, p)
    ver.sf <- st_as_sf(ver)
    st_write(ver.sf, file.path(out.dir, paste0( p, "_", s, "_lscv.gpkg")), delete_dsn = TRUE)
    st_write(ver.sf, file.path(out.dir, paste0( p, "_", s, "_lsvc.shp")))
    
  },
  error = function(e){
    print( paste0("unable to generate vertices for ", p, "% vertices for ", s))
  })
  
} # end of hrpc loop


















kde2 <- bkde2D(jdatxy,
               bandwidth=c(.045, .068), gridsize = c(1000,1000)) # need to adjust this
CL2 <- contourLines(kde2$x1 , kde2$x2 , kde2$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS2 <- as.factor(sapply(CL2, `[[`, "level"))
NLEV2 <- length(levels(LEVS2))

## CONVERT CONTOUR LINES TO POLYGONS
pgons2 <- lapply(1:length(CL2), function(i)
  Polygons(list(Polygon(cbind(CL2[[i]]$x, CL2[[i]]$y))), ID=i))
spgons2 = SpatialPolygons(pgons2)

## Leaflet map with polygons
leaflet(spgons2) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV2, NULL)[LEVS2])               

##############



kde3 <- bkde2D(jdatxy,
               bandwidth=c(.45, .68), gridsize = c(1000,1000)) # need to adjust this
CL3 <- contourLines(kde3$x1 , kde3$x2 , kde3$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS3 <- as.factor(sapply(CL3, `[[`, "level"))
NLEV3 <- length(levels(LEVS3))

## CONVERT CONTOUR LINES TO POLYGONS
pgons3 <- lapply(1:length(CL3), function(i)
  Polygons(list(Polygon(cbind(CL3[[i]]$x, CL3[[i]]$y))), ID=i))
spgons3 = SpatialPolygons(pgons3)

## Leaflet map with polygons
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])               


# filter data to remove NA values
jdat <- jdat %>% dplyr::select(c(Date, Median.long, Median.lat, Birdid))



######################################


kde4 <- bkde2D(jdatxy,
               bandwidth=c(.5, .5), gridsize = c(1000,1000)) # need to adjust this
CL4 <- contourLines(kde4$x1 , kde4$x2 , kde4$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS4 <- as.factor(sapply(CL4, `[[`, "level"))
NLEV4 <- length(levels(LEVS4))

## CONVERT CONTOUR LINES TO POLYGONS
pgons4 <- lapply(1:length(CL4), function(i)
  Polygons(list(Polygon(cbind(CL4[[i]]$x, CL4[[i]]$y))), ID=i))
spgons4 = SpatialPolygons(pgons4)

## Leaflet map with polygons
leaflet(spgons4) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV4, NULL)[LEVS4])               



###################################################


kde5 <- bkde2D(jdatxy,
               bandwidth=c(.05, .05), gridsize = c(10000,10000)) # need to adjust this
CL5 <- contourLines(kde5$x1 , kde5$x2 , kde5$fhat)       

## EXTRACT CONTOUR LINE LEVELS
LEVS5 <- as.factor(sapply(CL5, `[[`, "level"))
NLEV5 <- length(levels(LEVS5))

## CONVERT CONTOUR LINES TO POLYGONS
pgons5 <- lapply(1:length(CL5), function(i)
  Polygons(list(Polygon(cbind(CL5[[i]]$x, CL5[[i]]$y))), ID=i))
spgons5 = SpatialPolygons(pgons5)

## Leaflet map with polygons
leaflet(spgons5) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV5, NULL)[LEVS4])               


# finer grain size = less generalise i.e focus on the points and not areas. 10000 is too fine. 



## Leaflet map with polygons
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])     

leaflet() %>%
  addTiles %>%
  addPolygons(spgons3)


Refs

https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package




### COMPARE THE OUTPUTS 



## bandwidth=c(.0045, .0068), gridsize = c(10000,10000))
## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])   
# a bit too fine 



##  bandwidth=c(.045, .068), gridsize = c(1000,1000))
## Leaflet map with polygons
leaflet(spgons2) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV2, NULL)[LEVS2])               
# filters out the 


## bandwidth=c(.45, .68), gridsize = c(1000,1000)) 
leaflet(spgons3) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV3, NULL)[LEVS3])               

## bandwidth=c(.5, .5), gridsize = c(1000,1000))
leaflet(spgons4) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV4, NULL)[LEVS4])               


#bandwidth=c(.05, .05), gridsize = c(10000,10000))
leaflet(spgons5) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV5, NULL)[LEVS4])   