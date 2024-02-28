# graphics for common areas for all subpopulations

# ie breeding area
# hudson bay
# delaware bay 
# nth Sth america. 


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


###############################
# # read in locations 

locals <- st_read(file.path(out_dat,"stopover_locations_named.gpkg")) %>%
  st_drop_geometry() |>
  select(name, country, state, region)

solo <- left_join(so, locals) |>
  select(tag.id, DayMonth_s,DayMonth_e, start, end, movement_dir, dur_days, Subpopulations, name, region, type) %>%
  filter(!type %in% c("static", "Not usable")) |> 
  mutate(Subpop_class = case_when(
    Subpopulations %in% c("SE_nth_sthAm_TDF", "unsure" ,"Not usable" , "NA" ,NA,"unsure(se?)", "SE_nth_sthAm" ) ~ "unknown", 
    .default = as.character(Subpopulations)))


saveRDS(solo, file.path(out_dat , "REKN_stopovers_final.rds"))



##################################

# 1) review the breeding grounds

##################################

# keep only the full breeding duration tags

solo_breed <- solo %>% filter(movement_dir == "breeding") %>%
     select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations, Subpop_class,type, region)|> 
  filter(type %in% c( "partial spring/breeding"  ,"partial spring/breeding/partial fall" ,"spring migration/breeding/fall migration/partial wintering",
                      "partial spring/breeding/partial fall " ))  |> 
  filter(Subpop_class %in% c("WGWP", "TDF", "nth_sthAm", "SE"))

solo_breed<- cbind(st_coordinates(solo_breed),solo_breed)

sb <- solo_breed |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end)) |> 
  select(X, Y, tag.id, Subpopulations,Subpop_class, doy_start, doy_end, region)%>%
  st_drop_geometry()%>% 
  mutate(dur = doy_end - doy_start)

tags <- as.factor(unique(sb$tag.id)) %>% droplevels()

# extend the data table for the figure: 

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

# filter to shorten the length of 
acc_out <- acc_out |> 
  mutate(date_label = parse_date_time(x = paste(2021,doy), orders = "yj")) %>%
  mutate(date_label = ymd(date_label))  %>%
  filter(doy < 233)
  
 # aa <- acc_out |> 
 #  group_by(doy) |> 
 #  summarise(counts = n())

## Generate the plots: 

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out, aes(x = X, y = Y, colour = Subpop_class), size = 4) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
 # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-125, -60), ylim = c(55, 79), expand = FALSE)+
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

p_animate <- global + 
  transition_time(date_label) + 
  labs(title = "Date: {round(frame_time)}") +
  enter_grow() + #enter_drift(x_mod = -1) + 
  exit_shrink()

animate(
  p_animate, 
  width = 10, 
  height = 8, 
  units = "in", 
  res = 72, 
  fps = 2, #10 default  
  nframes = 50
)


# 
# 
# ## Generate by week 
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
#     legend.key.width = unit(3, "lines")) 
# 
# p_animate <- p + 
#   transition_time(day) + 
#   labs(title = "Day of Year: {round(frame_along)}")
# 
# # p_animate <- global + 
# #   transition_reveal(along = doy) + 
# #   labs(title = "Day of Year: {round(frame_along)}")
# 
# animate(
#   p_animate, 
#   width = 10, 
#   height = 6, 
#   units = "in", 
#   res = 72, 
#   fps = 10, 
#   nframes = 300
# )



## Get summary form min and max dates 
breeding_sum <- sb |> 
  st_drop_geometry() |> 
  select( Subpop_class, region, dur,doy_start, doy_end ) |> 
  group_by(region, Subpop_class) |> 
  summarise(arrive_mindate = min(doy_start),
            arrive_maxdate = max(doy_start),
            arrive_ave = mean(doy_start),
            depart_mindate = min(doy_end),
            depart_maxdate = max(doy_end),
            arrive_dep = mean(doy_end),
            count = n())
            #ave_duration = mean(as.numeric(duration)))

# 
# ggplot(breeding_sum, aes(y=factor(region))) +
#   geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class)), linewidth = 3)+
#   facet_wrap(~region)+
#    xlab("Date") + ylab("Tag") 
# 
# 
# breeding_sum_final <- breeding_sum |> 
#   filter(Subpop_class %in% c("WGWP", "TDF", "nth_sthAm", "SE"))

ggplot(breeding_sum, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class)), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_wrap(~region) +
  ggtitle("Breeding Locations") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")





##################################

# 2) Hudson Bay  - northward

##################################

solo_hb_n <- solo %>% filter(movement_dir == "northward") %>%
  select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations,Subpop_class,type, region)|> 
  #filter(type %in% c( "partial spring/breeding"  ,"partial spring/breeding/partial fall" ,"spring migration/breeding/fall migration/partial wintering",
  #                    "partial spring/breeding/partial fall " )) |> 
  filter(region %in% c( "james_bay", "hudson_bay_east", "hudson_bay_west","hudson_bay_north")) |> 
  filter(dur_days >= 2)


solo_hb_n<- cbind(st_coordinates(solo_hb_n),solo_hb_n)

shb <- solo_hb_n |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end)) |> 
  select(X, Y, tag.id, Subpopulations, doy_start, doy_end,Subpop_class, region)%>%
  st_drop_geometry()%>% 
  mutate(dur = doy_end - doy_start)


# filter to only the known locations 
shb  <- shb  |> 
  filter(Subpopulations %in% c( "WGWP" ,  "nth_sthAm" , "SE",  "TDF")) #         "nth_sthAm_TDF" "SE_nth_sthAm"))

tags <- as.factor(unique(shb$tag.id)) %>% droplevels()

# extend the data table for the figure: 

acc_out_sbh <- foreach::foreach(xx = levels(tags), .combine = rbind)%do% {
  #xx <- tags[9]
  xx  <- as.character(xx)
  
  alldat = shb |> 
    filter(tag.id == xx) %>% 
    mutate(dur = doy_end - doy_start) %>%
    filter(dur >0)
  
  alldat = alldat %>%
    mutate(no_stops = seq(1, length(alldat$X), 1))
  
  if(length(alldat$X > 1)){
    
    stops <- as.factor(unique(alldat$no_stops)) %>% droplevels()
    
    out_single <- foreach::foreach(ss = levels(stops), .combine = rbind)%do% {
      
      #ss <- stops[1]
      ss <- as.numeric(ss)
      
      sdat = alldat  |> 
        filter(no_stops == ss) 
      
      sdat = sdat  %>% 
        slice(rep(1:n(), times = sdat$dur))
      
      sdat$doy = seq(sdat$doy_start[1], sdat$doy_end[1]-1, 1)
      
      sdat
    
      } 
    
    out_single 
    
    } else {
     
   out_single <-  alldat %>% 
     slice(rep(1:n(), times = alldat$dur))
  
   out_single$doy = seq(out_single$doy_start[1], out_single$doy_end[1]-1, 1)
  
   out_single
   
   }
  
}


# filter to shorten the length of 
acc_out_sbh <- acc_out_sbh |> 
  mutate(date_label = parse_date_time(x = paste(2021,doy), orders = "yj")) %>%
  mutate(date_label = ymd(date_label))  #%>%
  #filter(doy < 233)
# 
# aa <- acc_out_sbh  |>
#  group_by(doy) |>
#  summarise(counts = n())
# 

## Generate the plots: 

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global_north <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out_sbh, aes(x = X, y = Y, colour = Subpopulations), size = 4) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-110, -65), ylim = c(45, 70), expand = FALSE)+
  theme_bw()+
  labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global_north

p_animate_north <- global_north + 
  transition_time(date_label) + 
  labs(title = "Date: {round(frame_time)}") +
  enter_grow() + #enter_drift(x_mod = -1) + 
  exit_shrink()

animate(
  p_animate_north, 
  width = 10, 
  height = 8, 
  units = "in", 
  res = 72, 
  fps = 2, #10 default  
  nframes = 50
)


## Get summary form min and max dates 
hbn_sum <- shb |> 
  st_drop_geometry() |> 
  select( Subpop_class, region, dur,doy_start, doy_end ) |> 
  group_by(region, Subpop_class) |> 
  summarise(arrive_mindate = min(doy_start),
            arrive_maxdate = max(doy_start),
            arrive_ave = mean(doy_start),
            depart_mindate = min(doy_end),
            depart_maxdate = max(doy_end),
            arrive_dep = mean(doy_end),
            count = n())
#ave_duration = mean(as.numeric(duration)))

ggplot(hbn_sum, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class)), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_wrap(~region) +
  ggtitle("Hudson Bay and James Bay Spring Migration") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")







##################################

# 3) Hudson Bay  - southward

##################################

solo_hb_s <- solo %>% filter(movement_dir == "southward") %>%
  select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations,Subpop_class, type, region)|> 
  #filter(type %in% c( "partial spring/breeding"  ,"partial spring/breeding/partial fall" ,"spring migration/breeding/fall migration/partial wintering",
  ##                    "partial spring/breeding/partial fall " )) |> 
  filter(region %in% c( "james_bay", "hudson_bay_east", "hudson_bay_west","hudson_bay_north")) |> 
  filter(dur_days >= 2)

solo_hb_s<- cbind(st_coordinates(solo_hb_s),solo_hb_s)

shb <- solo_hb_s |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end)) |> 
  select(X, Y, tag.id, Subpop_class, Subpopulations,region, doy_start, doy_end)%>%
  st_drop_geometry()%>% 
  mutate(dur = doy_end - doy_start)


# filter to only the known locations 
shb  <- shb  |> 
  filter(Subpopulations %in% c( "WGWP" ,  "nth_sthAm" , "SE",  "TDF")) #         "nth_sthAm_TDF" "SE_nth_sthAm"))

tags <- as.factor(unique(shb$tag.id)) %>% droplevels()

# extend the data table for the figure: 

acc_out_sbh_sth <- foreach::foreach(xx = levels(tags), .combine = rbind)%do% {
  #xx <- tags[9]
  xx  <- as.character(xx)
  
  alldat = shb |> 
    filter(tag.id == xx) %>% 
    mutate(dur = doy_end - doy_start) %>%
    filter(dur >0)
  
  alldat = alldat %>%
    mutate(no_stops = seq(1, length(alldat$X), 1))
  
  if(length(alldat$X > 1)){
    
    stops <- as.factor(unique(alldat$no_stops)) %>% droplevels()
    
    out_single <- foreach::foreach(ss = levels(stops), .combine = rbind)%do% {
      
      #ss <- stops[1]
      ss <- as.numeric(ss)
      
      sdat = alldat  |> 
        filter(no_stops == ss) 
      
      sdat = sdat  %>% 
        slice(rep(1:n(), times = sdat$dur))
      
      sdat$doy = seq(sdat$doy_start[1], sdat$doy_end[1]-1, 1)
      
      sdat
      
    } 
    
    out_single 
    
  } else {
    
    
    out_single <-  alldat %>% 
      slice(rep(1:n(), times = alldat$dur))
    
    out_single$doy = seq(out_single$doy_start[1], out_single$doy_end[1]-1, 1)
    
    out_single
    
  }
  
}


# filter to shorten the length of 
acc_out_sbh_sth <- acc_out_sbh_sth |> 
  mutate(date_label = parse_date_time(x = paste(2021,doy), orders = "yj")) %>%
  mutate(date_label = ymd(date_label))  #%>%

#filter(doy < 233)
# 
# aa <- acc_out_sbh  |>
#  group_by(doy) |>
#  summarise(counts = n())
# 

## Generate the plots: 

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 

global_sth <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out_sbh_sth, aes(x = X, y = Y, colour = Subpopulations), size = 4) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-110, -65), ylim = c(45, 70), expand = FALSE)+
  theme_bw()+
  labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global_sth

p_animate_sth <- global_sth + 
  transition_time(date_label) + 
  labs(title = "Date: {round(frame_time)}") +
  enter_grow() + #enter_drift(x_mod = -1) + 
  exit_shrink()

animate(
  p_animate_sth , 
  width = 10, 
  height = 8, 
  units = "in", 
  res = 72, 
  #fps = 10, #10 default  
  nframes = 50
)






## Get summary form min and max dates 
hbs_sum <- shb |> 
  st_drop_geometry() |> 
  select( Subpop_class, region, dur,doy_start, doy_end ) |> 
  group_by(region, Subpop_class) |> 
  summarise(arrive_mindate = min(doy_start),
            arrive_maxdate = max(doy_start),
            arrive_ave = mean(doy_start),
            depart_mindate = min(doy_end),
            depart_maxdate = max(doy_end),
            arrive_dep = mean(doy_end),
            count = n())
#ave_duration = mean(as.numeric(duration)))


hbs_sum$direction = "southward"
hbn_sum$direction = "northward"


ggplot(hbn_sum, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class)), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_wrap(~region) +
  ggtitle("Hudson Bay and James Bay Spring Migration") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")



ggplot(hbs_sum, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class)), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_wrap(~region) +
  ggtitle("Hudson Bay and James Bay Fall Migration") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")



all_hb <- bind_rows(hbs_sum, hbn_sum)



ggplot(all_hb, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class),colour = direction), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_grid( rows = vars(region))+
  ggtitle("Hudson Bay and James Bay Spring and Fall Migration") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")











##################################

# 4) Delaware Bay - northward

##################################

solo_db <- solo %>% filter(movement_dir == "northward") %>%
  select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations, Subpop_class, type, region)|> 
# filter(type %in% c( "partial spring/breeding"  ,"partial spring/breeding/partial fall" ,"spring migration/breeding/fall migration/partial wintering",
#                      "partial spring/breeding/partial fall " )) |> 
  filter(region %in% c("delbay_region"))

solo_db <- cbind(st_coordinates(solo_db), solo_db)

shb <- solo_db |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end)) |> 
  select(X, Y, tag.id, Subpopulations,Subpop_class, region, doy_start, doy_end)%>%
  st_drop_geometry()%>% 
  mutate(dur = doy_end - doy_start)


# filter to only the known locations 
shb  <- shb  |> 
  filter(Subpopulations %in% c( "WGWP" ,  "nth_sthAm" , "SE",  "TDF")) #         "nth_sthAm_TDF" "SE_nth_sthAm"))

tags <- as.factor(unique(shb$tag.id)) %>% droplevels()

# extend the data table for the figure: 

acc_out_db <- foreach::foreach(xx = levels(tags), .combine = rbind)%do% {
  #xx <- tags[9]
  xx  <- as.character(xx)
  
  alldat = shb |> 
    filter(tag.id == xx) %>% 
    mutate(dur = doy_end - doy_start) %>%
    filter(dur >0)
  
  alldat = alldat %>%
    mutate(no_stops = seq(1, length(alldat$X), 1))
  
  if(length(alldat$X > 1)){
    
    stops <- as.factor(unique(alldat$no_stops)) %>% droplevels()
    
    out_single <- foreach::foreach(ss = levels(stops), .combine = rbind)%do% {
      
      #ss <- stops[1]
      ss <- as.numeric(ss)
      
      sdat = alldat  |> 
        filter(no_stops == ss) 
      
      sdat = sdat  %>% 
        slice(rep(1:n(), times = sdat$dur))
      
      sdat$doy = seq(sdat$doy_start[1], sdat$doy_end[1]-1, 1)
      
      sdat
      
    } 
    
    out_single 
    
  } else {
    
    
    out_single <-  alldat %>% 
      slice(rep(1:n(), times = alldat$dur))
    
    out_single$doy = seq(out_single$doy_start[1], out_single$doy_end[1]-1, 1)
    
    out_single
    
  }
  
}


# filter to shorten the length of 
acc_out_db <- acc_out_db |> 
  mutate(date_label = parse_date_time(x = paste(2021,doy), orders = "yj")) %>%
  mutate(date_label = ymd(date_label))  #%>%

#filter(doy < 233)
# 
# aa <- acc_out_sbh  |>
#  group_by(doy) |>
#  summarise(counts = n())
# 

## Generate the plots: 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global_db <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out_db, aes(x = X, y = Y, colour = Subpopulations), size = 4) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-77, -73), ylim = c(37, 40), expand = FALSE)+
  theme_bw()+
  labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global_db

p_animate_db <- global_db + 
  transition_time(date_label) + 
  labs(title = "Date: {round(frame_time)}") +
  enter_grow() + #enter_drift(x_mod = -1) + 
  exit_shrink()

xx <- animate(
  p_animate_db  , 
  width = 10, 
  height = 8, 
  units = "in", 
  res = 72, 
  fps = 2, #10 default  
  nframes = 50
)

xx



## Get summary form min and max dates 
dbn_sum <- shb |> 
  st_drop_geometry() |> 
  select( Subpop_class, region, dur,doy_start, doy_end ) |> 
  group_by(region, Subpop_class) |> 
  summarise(arrive_mindate = min(doy_start),
            arrive_maxdate = max(doy_start),
            arrive_ave = mean(doy_start),
            depart_mindate = min(doy_end),
            depart_maxdate = max(doy_end),
            arrive_dep = mean(doy_end),
            count = n()) %>%
  mutate(direction = "northward")
#ave_duration = mean(as.numeric(duration)))








##################################

# 4) Delaware Bay - southward

##################################

solo_db <- solo %>% filter(movement_dir == "southward") %>%
  select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations, Subpop_class, type, region)|> 
  # filter(type %in% c( "partial spring/breeding"  ,"partial spring/breeding/partial fall" ,"spring migration/breeding/fall migration/partial wintering",
  #                      "partial spring/breeding/partial fall " )) |> 
  filter(region %in% c("delbay_region"))

solo_db <- cbind(st_coordinates(solo_db), solo_db)

shb <- solo_db |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end)) |> 
  select(X, Y, tag.id, Subpopulations,Subpop_class, region, doy_start, doy_end)%>%
  st_drop_geometry()%>% 
  mutate(dur = doy_end - doy_start) |> 
  filter(dur >=1)


# filter to only the known locations 
shb  <- shb  |> 
  filter(Subpopulations %in% c( "WGWP" ,  "nth_sthAm" , "SE",  "TDF")) #         "nth_sthAm_TDF" "SE_nth_sthAm"))

tags <- as.factor(unique(shb$tag.id)) %>% droplevels()

# extend the data table for the figure: 

acc_out_db <- foreach::foreach(xx = levels(tags), .combine = rbind)%do% {
  #xx <- tags[9]
  xx  <- as.character(xx)
  
  alldat = shb |> 
    filter(tag.id == xx) %>% 
    mutate(dur = doy_end - doy_start) %>%
    filter(dur >0)
  
  alldat = alldat %>%
    mutate(no_stops = seq(1, length(alldat$X), 1))
  
  if(length(alldat$X > 1)){
    
    stops <- as.factor(unique(alldat$no_stops)) %>% droplevels()
    
    out_single <- foreach::foreach(ss = levels(stops), .combine = rbind)%do% {
      
      #ss <- stops[1]
      ss <- as.numeric(ss)
      
      sdat = alldat  |> 
        filter(no_stops == ss) 
      
      sdat = sdat  %>% 
        slice(rep(1:n(), times = sdat$dur))
      
      sdat$doy = seq(sdat$doy_start[1], sdat$doy_end[1]-1, 1)
      
      sdat
      
    } 
    
    out_single 
    
  } else {
    
    
    out_single <-  alldat %>% 
      slice(rep(1:n(), times = alldat$dur))
    
    out_single$doy = seq(out_single$doy_start[1], out_single$doy_end[1]-1, 1)
    
    out_single
    
  }
  
}

# filter to shorten the length of 
acc_out_db <- acc_out_db |> 
  mutate(date_label = parse_date_time(x = paste(2021,doy), orders = "yj")) %>%
  mutate(date_label = ymd(date_label))  #%>%


## Generate the plots: 

world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 

global_db <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out_db, aes(x = X, y = Y, colour = Subpopulations), size = 4) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-77, -73), ylim = c(37, 40), expand = FALSE)+
  theme_bw()+
  labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global_db
# 
# p_animate_db <- global_db + 
#   transition_time(date_label) + 
#   labs(title = "Date: {round(frame_time)}") +
#   enter_grow() + #enter_drift(x_mod = -1) + 
#   exit_shrink()
# 
# xx <- animate(
#   p_animate_db  , 
#   width = 10, 
#   height = 8, 
#   units = "in", 
#   res = 72, 
#   fps = 2, #10 default  
#   nframes = 300
# )
# 
# xx



## Get summary form min and max dates 
dbs_sum <- shb |> 
  st_drop_geometry() |> 
  select( Subpop_class, region, dur,doy_start, doy_end ) |> 
  group_by(region, Subpop_class) |> 
  summarise(arrive_mindate = min(doy_start),
            arrive_maxdate = max(doy_start),
            arrive_ave = mean(doy_start),
            depart_mindate = min(doy_end),
            depart_maxdate = max(doy_end),
            arrive_dep = mean(doy_end),
            count = n()) |> 
  mutate(direction= "southward")
#ave_duration = mean(as.numeric(duration)))

all_db <- bind_rows(dbs_sum, dbn_sum)

ggplot(all_db, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class),colour = direction), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_wrap( ~region)+
  ggtitle("Delaware Bay Spring and Fall Migration") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")


##################################

# 5)  Northern Nth Brazil 

##################################

solo_stham <- solo %>% filter(movement_dir == "southward") %>%
  select(tag.id,  start, end, DayMonth_s,DayMonth_e, movement_dir, dur_days, Subpopulations, Subpop_class, type, region)|> 
  # filter(type %in% c( "partial spring/breeding"  ,"partial spring/breeding/partial fall" ,"spring migration/breeding/fall migration/partial wintering",
  #                      "partial spring/breeding/partial fall " )) |> 
  filter(region %in% c("nth_venezuela", "guyana" , "northern_brazil")) 

solo_stham <- cbind(st_coordinates(solo_stham ), solo_stham )

shb <- solo_stham  |> 
  mutate(doy_start = yday(start), 
         doy_end = yday(end),
         week_start = lubridate::isoweek(start),
          week_end = lubridate::isoweek(end)) |> 
  select(X, Y, tag.id, Subpopulations, Subpop_class,doy_start, doy_end, week_start, week_end, region)%>%
  st_drop_geometry() %>% 
  mutate(dur = doy_end - doy_start,
         dur_week = week_end - week_start) |> 
  filter(dur>=2)


# filter to only the known locations 
shb  <- shb  |> 
  filter(Subpopulations %in% c( "nth_sthAm" , "SE",  "TDF")) #         "nth_sthAm_TDF" "SE_nth_sthAm"))

tags <- as.factor(unique(shb$tag.id)) %>% droplevels()

# extend the data table for the figure: 

acc_out_stham <- foreach::foreach(xx = levels(tags), .combine = rbind)%do% {
  
  #xx <- tags[2]
  #xx  <- as.numeric(xx)
  
  alldat = shb |> 
    filter(tag.id == xx) %>% 
    mutate(dur = doy_end - doy_start) %>%
    filter(dur >0)
  
  alldat = alldat %>%
    mutate(no_stops = seq(1, length(alldat$X), 1))
  
  if(length(alldat$X > 1)){
    
    stops <- as.factor(unique(alldat$no_stops)) %>% droplevels()
    
    out_single <- foreach::foreach(ss = levels(stops), .combine = rbind)%do% {
      #ss <- stops[1]
      ss <- as.numeric(ss)
      
      sdat = alldat  |> 
        filter(no_stops == ss) 
      
      sdat = sdat  %>% 
        slice(rep(1:n(), times = sdat$dur))
      
      sdat$doy = seq(sdat$doy_start[1], sdat$doy_end[1]-1, 1)
      
      sdat
    } 
    
    out_single 
    
  } else {
    
    out_single <-  alldat %>% 
      slice(rep(1:n(), times = alldat$dur))
    
    out_single$doy = seq(out_single$doy_start[1], out_single$doy_end[1]-1, 1)
    
    out_single
    
  }
  
}

# filter to shorten the length of 
acc_out_stham  <- acc_out_stham  |> 
  mutate(date_label = parse_date_time(x = paste(2021,doy), orders = "yj")) %>%
  mutate(date_label = ymd(date_label)) %>%
  mutate(week_label = lubridate::isoweek(date_label))

#filter(doy < 233)
# 
# aa <- acc_out_sbh  |>
#  group_by(doy) |>
#  summarise(counts = n())
# 


## Generate the plots: 

world <- ne_countries(scale = "medium", returnclass = "sf")
Americas <- world %>% dplyr::filter(region_un == "Americas")

# entire north America 
global_nthsm <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = acc_out_stham , aes(x = X, y = Y, colour = Subpopulations), size = 4) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  #xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-80, -25), ylim = c(-15, 20), expand = FALSE)+
  theme_bw()+
  labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global_nthsm 

p_animate_nthsm  <- global_nthsm + 
  transition_time(date_label) + 
  labs(title = "Date: {round(frame_time)}") +
  enter_grow() + #enter_drift(x_mod = -1) + 
  exit_shrink()

animate(
  p_animate_nthsm  , 
  width = 10, 
  height = 8, 
  units = "in", 
  res = 72, 
  fps = 2, #10 default  
  nframes = 50
)





## Get summary form min and max dates 
nth_sum <- shb |> 
  st_drop_geometry() |> 
  select( Subpop_class, region, dur,doy_start, doy_end ) |> 
  group_by(region, Subpop_class) |> 
  summarise(arrive_mindate = min(doy_start),
            arrive_maxdate = max(doy_start),
            arrive_ave = mean(doy_start),
            depart_mindate = min(doy_end),
            depart_maxdate = max(doy_end),
            arrive_dep = mean(doy_end),
            count = n()) |> 
  mutate(direction= "southward")
#ave_duration = mean(as.numeric(duration)))


ggplot(nth_sum, aes(y=factor(region))) +
  geom_segment(aes(x=arrive_mindate, xend=depart_maxdate, y=factor(Subpop_class), yend=factor(Subpop_class),colour = direction), linewidth = 3)+
  geom_point(aes(x=arrive_ave, y=factor(Subpop_class), size = 2), colour = "blue") +
  geom_point(aes(x=arrive_dep, y=factor(Subpop_class), size = 2), colour = "red") +
  facet_grid( rows = vars(region))+
  ggtitle("Northern Brazil Fall Migration") +
  #geom_text( aes(y = factor(region), x = depart_maxdate, label = count))+
  xlab("Day of Year") + ylab("sub-population") +
  theme(legend.position = "none")

















# ## Generate the 
# 
# p <- ggplot(acc_out_sbh) +
#   geom_point(aes(x = X, y = Y, colour = Subpopulations)) +
#   # geom_path(aes(x = X, y = Y, colour = Subpopulations)) +
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
#     legend.key.width = unit(3, "lines")) 
# 
# p_animate <- p + 
#   transition_time(doy) + 
#   labs(title = "Day of Year: {round(frame_along)}")
# 
# # p_animate <- global + 
# #   transition_reveal(along = doy) + 
# #   labs(title = "Day of Year: {round(frame_along)}")
# 
# animate(
#   p_animate, 
#   width = 10, 
#   height = 6, 
#   units = "in", 
#   res = 72, 
#   fps = 10, 
#   nframes = 300
# )


###################################################








































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

