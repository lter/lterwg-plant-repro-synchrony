## ------------------------------------------ ##
    # Synchrony Trait Similarity Statistics
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Performs statistical analysis on trait similarity
## As of 10/23/23 this corresponds to the data presented in figure 4B

# PRE-REQUISITES
## The following script(s) must be run--in order--for this script to work as intended
## 1. Run `synchrony_stats_prep.R`

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP)

# Clear environment
rm(list = ls())

# Create needed local folder(s)
dir.create(path = file.path("stats_results"), showWarnings = F)

# Read in synchrony data
sync_df <- read.csv(file = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv"))

# Glimpse it
dplyr::glimpse(sync_df)

## ------------------------------------------ ##
            # "Global" Analysis ----
## ------------------------------------------ ##

# Fit model
glob_fit <- RRPP::lm.rrpp(r.spearman ~ TraitSimilarityJaccardVariant, 
                          iter = 999, data = sync_df)

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
for(site in unique(sync_df$lter)){
  
  # Processing message
  message("Beginning analysis for LTER: ", site)
  
  # Subset the data
  sub_df <- sync_df %>%
    dplyr::filter(lter == site)
  
  # Fit model
  sub_fit <- RRPP::lm.rrpp(r.spearman ~ TraitSimilarityJaccardVariant, iter = 999, data = sub_df)
  
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
(file_name <- paste0("trait-sim-results_", Sys.Date(), ".csv"))

# Save locally
write.csv(x = stat_out, file = file.path("stats_results", file_name), row.names = F, na = '')

# End ----
