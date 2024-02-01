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
dir.create(path = file.path("figure_data"), showWarnings = F)
dir.create(path = file.path("table_data"), showWarnings = F)
dir.create(path = file.path("map_data"), showWarnings = F)

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

# Winnow to only the ANOVA results
aov_outs <- stat_outs[stringr::str_detect(string = stat_outs, pattern = "ANOVA_trait_")]

# Upload each to the Drive
for(file in aov_outs){
  googledrive::drive_upload(media = file.path("stats_results", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
      # 5 - Trait Similarity Analyses ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_similarity-stats.R"

# Identify needed data files
data_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
  dplyr::filter(name %in% c("synchrony_pcoa_climate_combination.csv"))

# Combine all needed files into one object
(sim_needs <- data_files )

# Download them
purrr::walk2(.x = sim_needs$id, .y = sim_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
            # Upload
## -------------------------- ##
## Run *after* "synchrony_similarity-stats.R"

# Identify all statistical result files
stat_outs <- dir(path = file.path("stats_results"))

# Winnow to only the similarity results
sim_outs <- stat_outs[stringr::str_detect(string = stat_outs, pattern = "trait-sim-results_")]

# Upload each to the Drive
for(file in sim_outs){
  googledrive::drive_upload(media = file.path("stats_results", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
    # 6 - Climate/Phylogeny Analyses ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_climate_trait_phylog-stats.R"

# Identify needed data files
data_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
  dplyr::filter(name %in% c("synchrony_pcoa_climate_combination.csv"))

# Combine all needed files into one object
(phylog_needs <- data_files )

# Download them
purrr::walk2(.x = phylog_needs$id, .y = phylog_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
          # Upload
## -------------------------- ##
## Run *after* "synchrony_climate_trait_phylog-stats.R"

# Identify all statistical result files
stat_outs <- dir(path = file.path("stats_results"))

# Winnow to only the mixed-effect results
mix_outs <- stat_outs[stringr::str_detect(string = stat_outs, pattern = "mixed-effect-results_")]

# Upload each to the Drive
for(file in mix_outs){
  googledrive::drive_upload(media = file.path("stats_results", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
            # 7 - Summary Tables ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_tables.R"

# Identify needed data files
data_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
  dplyr::filter(name %in% c("synchrony_pcoa_climate_combination.csv", "pre_ordination_trait_data.csv"))

# Identify full trait file
full_trait <-  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1PGaPAkNz1lmvZQMwwthmS-ZjQ97BS2Am"), type = "csv") %>%
  dplyr::filter(name %in% c("LTER_integrated_attributes_USDA_2022-12-14.csv"))

# Combine all needed files into one object
(table_needs <- data_files %>% 
  dplyr::bind_rows(full_trait) )

# Download them
purrr::walk2(.x = table_needs$id,  .y = table_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
# Upload
## -------------------------- ##
## Run *after* "synchrony_tables.R"

# Identify produced files
(table_outs <- dir(path = file.path("table_data")))

# Upload each to the Drive (skipping the map if it's there)
for(file in table_outs){
  googledrive::drive_upload(media = file.path("table_data", file),
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ReUBqmiZK2gwGePNt9VN0qcfcLiwZ_MZ"), overwrite = T) }
# Upload each file

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
                # 8 - Site Map ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_map.R"

# Identify pre-requisite files to map creation
(map_needs <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1wo2xocmHx0isWwp3siX85rTFB9TvfnNx")) %>%
   # Filter to only desired files
   dplyr::filter(name %in% c("lter_site_coordinates.csv", "gblulcgeo20.tif")))

# Download them
## Note this takes longer than other downloads because the .tif is a big file
purrr::walk2(.x = map_needs$id, .y = map_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("map_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
          # Upload
## -------------------------- ##
## Run *after* "synchrony_map.R"

# Identify all figure outputs
fig_outs <- dir(path = file.path("synchrony_figure_files"))

# Winnow to just the map
map_out <- fig_outs[stringr::str_detect(string = fig_outs, pattern = "_map.png")]

# Upload to Google Drive
for(map in map_out){
  googledrive::drive_upload(media = file.path("synchrony_figure_files", map),
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1wZqCP-axj9KUfAaiPJsTamc03zsgngCY"),
                            overwrite = T) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
          # 9 - Visualization Prep ----
## ------------------------------------------ ##
## -------------------------- ##
          # Download
## -------------------------- ##
## Run *before* "synchrony_vis_prep.R"

# Identify needed data files
vis_files <- c("synchrony_pcoa_climate_combination.csv", "pre_ordination_trait_data.csv",
               "permutation_corr_unsummarized.csv", "series_andrews.csv", 
               "series_bonanza.csv")

# Find all needed files in Drive
(vis_needs <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
  dplyr::bind_rows(googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/folders/1aPdQBNlrmyWKtVkcCzY0jBGnYNHnwpeE"), type = "csv")) %>%
  dplyr::bind_rows(googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH"), type = "csv")) %>%
  ## Filter to only desired files
  dplyr::filter(name %in% c(vis_files) |
                stringr::str_detect(string = name, pattern = "MRM_not_averaged_results_") |
                  stringr::str_detect(string = name, pattern = "ANOVA_trait_")) )

# Download them
purrr::walk2(.x = vis_needs$id, .y = vis_needs$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Clear environment
rm(list = ls())

## -------------------------- ##
          # Upload
## -------------------------- ##
## Run *after* "synchrony_vis_prep.R"

# Identify produced files
vis_outs <- dir(path = file.path("figure_data"))

# Upload each file
for(file in vis_outs){
  googledrive::drive_upload(media = file.path("figure_data", file), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1tN5-GhlIWuEG7NKlaoVrUWAiLMG7agY2")) }

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

