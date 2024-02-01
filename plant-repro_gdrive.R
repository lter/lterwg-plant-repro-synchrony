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
dir.create(path = file.path("stats_results"), showWarnings = F)

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
(wanted_files <- tidy_files %>%
  dplyr::bind_rows(trait_files) %>%
  dplyr::bind_rows(phylo_files) %>% 
  dplyr::bind_rows(pc_file) %>% 
  dplyr::bind_rows(clim_files) )

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

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
          # 2 - MRM Analyses ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_mrm.R"

# Identify needed data files
data_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) %>% 
  dplyr::filter(name %in% c("synchrony_data.csv"))

# Combine all needed files into one object
mrm_needs <- data_files

# Download them
purrr::walk2(.x = mrm_needs$id, .y = mrm_needs$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tidy_data", .y)))

# Clear environment
rm(list = ls())

## -------------------------- ##
          # Upload
## -------------------------- ##
## Run *after* "synchrony_mrm.R"

# Upload the species pair averages data
googledrive::drive_upload(media = file.path("tidy_data", "synchrony_data_spp_averages.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"))

# Identify all statistical result files
stat_outs <- dir(path = file.path("stats_results"))

# Winnow to only the MRM results
mrm_outs <- stat_outs[stringr::str_detect(string = stat_outs, pattern = "MRM_")]

# Upload each to the Drive
for(file in mrm_outs){
  googledrive::drive_upload(media = file.path("stats_results", file), overwrite = T,
                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
   # 3 - Permuted Correlation Analyses ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_perm-stats.R"

# Identify needed data files
data_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
    dplyr::filter(name %in% c("synchrony_pcoa_climate_combination.csv"))

# Identify permuted correlation files
perm_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/folders/1aPdQBNlrmyWKtVkcCzY0jBGnYNHnwpeE"), type = "csv") %>% 
  dplyr::filter(name %in% c("permutation_corr_unsummarized.csv"))

# Combine all needed files into one object
(perm_stats_needs <- data_files %>% 
  dplyr::bind_rows(perm_files) )

# Download them
purrr::walk2(.x = perm_stats_needs$id, .y = perm_stats_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
            # Upload
## -------------------------- ##
## Run *after* "synchrony_perm-stats.R"

# Identify all statistical result files
stat_outs <- dir(path = file.path("stats_results"))

# Winnow to only the MRM results
perm_stats_outs <- stat_outs[stringr::str_detect(string = stat_outs, 
                                                 pattern = "perm-vs-actual-results_")]

# Upload each to the Drive
for(file in perm_stats_outs){
  googledrive::drive_upload(media = file.path("stats_results", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
          # 4 - perANOVA Analyses ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_anova.R"

# Identify needed data files
sync_file <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) %>%
  dplyr::filter(name == "synchrony_data.csv")

# Combine all needed files into one object
(aov_needs <- sync_file )

# Download them
purrr::walk2(.x = aov_needs$id, .y = aov_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
            # Upload
## -------------------------- ##
## Run *after* "synchrony_anova.R"

# Identify all statistical result files
stat_outs <- dir(path = file.path("stats_results"))

# Winnow to only the MRM results
aov_outs <- stat_outs[stringr::str_detect(string = stat_outs, pattern = "ANOVA_trait_")]

# Upload each to the Drive
for(file in aov_outs){
  googledrive::drive_upload(media = file.path("stats_results", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
# 4 - x ----
## ------------------------------------------ ##
## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_.R"

# Identify needed data files


# Combine all needed files into one object


# Download them

# Clear environment
rm(list = ls())

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_.R"

# Identify produced files

# Upload each file

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
# 4 - x ----
## ------------------------------------------ ##
## -------------------------- ##
# Download
## -------------------------- ##
## Run *before* "synchrony_.R"

# Identify needed data files


# Combine all needed files into one object


# Download them

# Clear environment
rm(list = ls())

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_.R"

# Identify produced files

# Upload each file

# Clear environment
rm(list = ls())






# End ----

