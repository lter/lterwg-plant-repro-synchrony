## ------------------------------------------ ##
  # Plant Repro. - Google Drive Interactions
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## This script is ONLY for members of the LTER Plant Reproduction working group
## It _requires_ access to their Shared Google Drive and is meant as a convenience for down/uploading data from/to the various online data storage locations

## ------------------------------------------ ##
                # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse)

# Authenticate Google Drive
googledrive::drive_auth()

# Clear environment
rm(list = ls())

# Create needed local folders
dir.create(path = file.path("source_data"), showWarnings = F)
dir.create(path = file.path("tidy_data"), showWarnings = F)

## ------------------------------------------ ##
          # 1 - Statistics Prep ----
## ------------------------------------------ ##

## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_stats_prep.R"

# Identify *tidy* file(s) to download
tidy_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1aPdQBNlrmyWKtVkcCzY0jBGnYNHnwpeE"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("pairwise_corr.csv", "masting_summary_stats.csv"))

# Identify *trait* file(s) to download
trait_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1PGaPAkNz1lmvZQMwwthmS-ZjQ97BS2Am"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("LTER_integrated_attributes_USDA_2022-12-14.csv"))

# Identify *phylogenetic distance* file(s) to download
phylo_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IVY6i79REaF59kZEBbrJCRNOlcxE7Tel"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("phylo distance matrix.csv"))

# Identify PCoA axis values in Drive
pc_file <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
  # Filter to desired file
  dplyr::filter(name == "trait_space_pcoa_axes.csv")

# Identify climate files
clim_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1tPM28pofJbpKXod3rXq-QEqjxGcU8xgt"), type = "csv") %>%
  dplyr::filter(name %in% c("climateSites_tidy_ANDupdate.csv", "plot_data_ClimateSites.csv"))

# Combine these file sets
wanted_files <- tidy_files %>%
  dplyr::bind_rows(trait_files) %>%
  dplyr::bind_rows(phylo_files) %>% 
  dplyr::bind_rows(pc_file) %>% 
  dplyr::bind_rows(clim_files)

# Check this out to make sure it includes all files that we need
wanted_files

# Download these files into that folder
purrr::walk2(.x = wanted_files$id, .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x),
                                                path = file.path("source_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
            # Upload
## -------------------------- ##
## Run *after* "synchrony_stats_prep.R"

# Identify produced files
stats_prep_files <- c("synchrony_data.csv", "synchrony_pcoa_climate_combination.csv", 
                      "synchrony_pcoa_combination.csv")

# Upload each file
for(file in stats_prep_files){
  googledrive::drive_upload(media = file.path("tidy_data", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) }


## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"

## ------------------------------------------ ##
# 2 - x ----
## ------------------------------------------ ##

## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_...R"

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_...R"


# End ----

