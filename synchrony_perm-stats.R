## ------------------------------------------ ##
      # Synchrony Permutation Statistics
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Performs statistical analysis on permuted vs. actual correlations
## As of 10/23/23 this corresponds to the data presented in figures 2 and 3A

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

## ------------------------------------------ ##
              # Data Wrangling ----
## ------------------------------------------ ##

# Read in synchrony data
sync_df <- read.csv(file = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv")) %>%
  # Pare down to needed columns
  dplyr::select(lter, Plot.ID, Species1, Species2, r.spearman) %>%
  # Drop non-unique rows (shouldn't be any but better safe than sorry)
  dplyr::distinct() %>%
  # Add a column indicating the type of correlation this is
  dplyr::mutate(corr.type = "actual", .before = r.spearman)

# Glimpse it
dplyr::glimpse(sync_df)

# Read in permutations of correlations
perm_df <- read.csv(file = file.path("tidy_data", "permutation_corr_unsummarized.csv")) %>%
  # Cut off below overlap threshold
  dplyr::filter(overlap > 9) %>%
  # Filter to only desired LTERs
  dplyr::filter(lter %in% c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV")) %>%
  # Pare down to desired columns
  dplyr::select(lter, Plot.ID, Species1, Species2, perm_r.spearman) %>%
  # Drop non-unique rows
  dplyr::distinct() %>%
  # Rename the correlation column
  dplyr::rename(r.spearman = perm_r.spearman) %>% 
  # Add a column for correlation type
  dplyr::mutate(corr.type = "permuted", .before = r.spearman)

# Check out structure
dplyr::glimpse(perm_df)

# Combine the two data objects
combo_df <- dplyr::bind_rows(sync_df, perm_df)

# Check structure
dplyr::glimpse(combo_df)

# Clean up environment
rm(list = setdiff(x = ls(), y = "combo_df"))

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
write.csv(x = stat_out, row.names = F, na = '',
          file = file.path("stats_results", file_name))

# End ----
