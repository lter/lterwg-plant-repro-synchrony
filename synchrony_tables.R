## ------------------------------------------ ##
          # Synchrony Summary Tables
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Create summary tables for inclusion in the paper

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, supportR)

# Clear environment
rm(list = ls())

# Identify names of files this script requires
sync_file <- "synchrony_pcoa_climate_combination.csv" # synchrony + climate data
trait_file <- "pre_ordination_trait_data.csv" # trait data
perm_file <- "pairwise_corr_perm.csv" # correlation permutation data
mrm_file <- "MRM_not_averaged_results_2023-06-14_10000perm.csv" # MRM results

# Identify links of relevant Drive folders
sync_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")
stats_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")

# Identify relevant data from those folders
## List out all CSVs in all folders
(wanted_files <- googledrive::drive_ls(path = sync_folder, type = "csv") %>%
    dplyr::bind_rows(googledrive::drive_ls(path = stats_folder, type = "csv")) %>%
    ## Filter to only desired files
    dplyr::filter(name %in% c(sync_file, trait_file, perm_file, mrm_file)))

# Create folder to download files into
dir.create(path = file.path("figure_data"), showWarnings = F)

# Download files into that folder
purrr::walk2(.x = wanted_files$id, 
             .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("figure_data", .y),
                                                overwrite = T))

## ------------------------------------------ ##
              # Data Wrangling ----
## ------------------------------------------ ##

# Read in synchrony data
sync_df <- read.csv(file = file.path("figure_data", sync_file)) %>%
  # Make a species pair column quickly
  dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
                .before = Species1) %>%
  # Drop needleleaf vs broadleaf
  dplyr::select(-dplyr::starts_with("Needleleaf_Broadleaf_")) %>%
  # Add in solid shape values
  dplyr::mutate(solid_shapes = dplyr::case_when(lter %in% c("AND", "HBR") ~ 15,
                                                lter %in% c("BNZ", "LUQ") ~ 16,
                                                lter %in% c("CDR", "SEV") ~ 17,
                                                lter %in% c("CWT") ~ 18))

# Glimpse it
dplyr::glimpse(sync_df)







# End ----
