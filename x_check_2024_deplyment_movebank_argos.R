# check the ids of the 2024 GPS tags with the deployment tags. 

# read in the deployment information 

library(readxl)
library(dplyr)
library(stringr)

dep_file <- fs::path("data", "movebank_reference","from_steph_2024_REKN_Sat_tag_deployments.xlsx" )

del <- read_excel(dep_file, sheet = 1) |> 
  dplyr::select("Band Number", "Code", "OBSERVATIONS" ) |> 
  dplyr::mutate(tagid = gsub("Sat tag #", "", OBSERVATIONS)) |> 
  dplyr::mutate(tagid = as.numeric(str_sub(tagid, 1,6))) |> 
  select(-OBSERVATIONS)
  
nj <- read_excel(dep_file, sheet = 2) |> 
  dplyr::select("Band Number", "Code", "OBSERVATIONS" ) |> 
  dplyr::mutate(tagid = gsub("Sat tag: ", "", OBSERVATIONS)) |> 
  dplyr::mutate(tagid = as.numeric(str_sub(tagid, 1,6))) |> 
  select(-OBSERVATIONS) |> 
  filter(!is.na(tagid))

lp <- read_excel(dep_file, sheet = 3, skip = 1) |> 
  dplyr::select("Bird ring", "Tags ID" ) |> 
  dplyr::mutate(tagidall = gsub("ID", "", `Tags ID`)) |> 
  mutate(tagidall = stringr::str_trim(tagidall, "left")) |> 
  dplyr::mutate(tagid = as.numeric(str_sub(tagidall, 1,6))) |> 
  dplyr::mutate(tagid2 = str_sub(tagidall, 8,13))

  
deltags <- del$tagid
njtags <- nj$tagid
lptags <- lp$tagid

#####################################
# Pull movebank data 

# check with steph which of the movebank projects are deployed in 2024


raw_dat <- file.path("data", "movebank_locations_20241208")
filesoi <- list.files(raw_dat)

# data_set3 : spring migration 
key = "Atlantic"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data 
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep %>% 
  filter(animal.id == "")



all_tags <- unique(brep$tag.id)


