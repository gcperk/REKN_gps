# load data 

library(leaflet)
library(RColorBrewer)
library(lubridate)
library(sp)
library(sf)
library(adehabitatHR)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)
library(foreach)


# read in previous project data 

raw_dat <- file.path("data", "raw_reference_data")
filesoi <- list.files(raw_dat)

# read in the previous NWRS proj files
key = "ReferenceDataNWRC_2021solar_proj.xlsx"

old <- list.files(raw_dat, pattern = key, full.names = T)
oldf = read_excel(old)

old <- oldf %>%
  dplyr::select(Subpop, 'Original dataset',"animal-ring-id") 
colnames(old) = c("subpop", "dataset","ringid")

oo <- old %>%
  distinct() %>%
  dplyr::filter(!is.na('ringid'))


# read in the key for the latest datasets 

raw_dat <- file.path("data", "movebank_reference", "movebank_ref_all_deployments.xlsx")
newf <- list.files(raw_dat, full.names = T)
newf = read_excel(raw_dat)


newf<- newf %>%
  dplyr::select(movebank_name ,`Band Number`)%>%
  distinct() 
  
colnames(newf) = c("movebank_name","ringid" )


tags <- dplyr::full_join(old, newf)
tt <- tags <- tags %>%filter(!is.na(subpop))
