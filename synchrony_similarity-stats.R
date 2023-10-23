## ------------------------------------------ ##
    # Synchrony Trait Similarity Statistics
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Performs statistical analysis on trait similarity
## As of 10/23/23 this corresponds to the data presented in figure 4B

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, RRPP)

# Clear environment
rm(list = ls())

# Identify names of files this script requires
sync_file <- "synchrony_pcoa_climate_combination.csv" # synchrony + climate data

# Identify links of relevant Drive folders
sync_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")

# Identify relevant data from those folders
## List out all CSVs in all folders
(wanted_files <- googledrive::drive_ls(path = sync_folder, type = "csv") %>%
    ## Filter to only desired files
    dplyr::filter(name %in% c(sync_file)))

# Create folder to download files into
dir.create(path = file.path("tidy_data"), showWarnings = F)
dir.create(path = file.path("stats_results"), showWarnings = F)

# Download files into that folder
purrr::walk2(.x = wanted_files$id, .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

# Read in synchrony data
sync_df <- read.csv(file = file.path("figure_data", sync_file))

# Glimpse it
dplyr::glimpse(sync_df)

# Clean up environment
rm(list = setdiff(x = ls(), y = "sync_df"))

## ------------------------------------------ ##
# "Global" Analysis ----
## ------------------------------------------ ##

# Fit model
glob_fit <- RRPP::lm.rrpp(r.spearman ~ corr.type, iter = 999, data = combo_df)

# Extract ANOVA table
glob_aov <- as.data.frame(anova(glob_fit)$table) %>%
  dplyr::mutate(lter = "All",
                term = row.names(.),
                .before = dplyr::everything())

# Check that out
glob_aov

## ------------------------------------------ ##
# Per-Site Analysis ----
## ------------------------------------------ ##

# Empty list for storing results
site_list <- list()

# Loop across sites
for(site in unique(combo_df$lter)){
  
  # Processing message
  message("Beginning analysis for LTER: ", site)
  
  # Subset the data
  sub_df <- combo_df %>%
    dplyr::filter(lter == site)
  
  # Fit model
  sub_fit <- RRPP::lm.rrpp(r.spearman ~ corr.type, iter = 999, data = sub_df)
  
  # Extract ANOVA table and add to list
  site_list[[site]] <- as.data.frame(anova(sub_fit)$table) %>%
    dplyr::mutate(lter = site,
                  term = row.names(.),
                  .before = dplyr::everything())
}

## ------------------------------------------ ##
# Process Outputs & Export ----
## ------------------------------------------ ##

# Unlist site information
site_aov <- purrr::list_rbind(x = site_list)

# Glimpse it
dplyr::glimpse(site_aov)

# Glimpse global AOV table too
dplyr::glimpse(glob_aov)

# Combine the two
stat_out <- dplyr::bind_rows(glob_aov, site_aov) %>%
  # Rename the P value column
  dplyr::rename(P = `Pr(>F)`)

# Generate time-stamped filename
(file_name <- paste0("perm-vs-actual-results_", Sys.Date(), ".csv"))

# Save locally
write.csv(x = stat_out, file = file.path("stats_results", file_name), row.names = F, na = '')

# Identify Drive destination
stats_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")

# And upload to Drive
googledrive::drive_upload(media = file.path("stats_results", file_name), path = stats_drive, overwrite = T)

# End ----
