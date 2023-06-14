## ------------------------------------------ ##
              # Synchrony MRMs
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Using multiple regression on distance matrices (MRM),
## analyze effect(s) of various traits on cross-species synchrony
## Also does sensitivity analyses by excluding one site and re-running all MRMs

## ------------------------------------------ ##
                # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, ecodist)

# Clear environment
rm(list = ls())

# Create tidy data folder if it doesn't exist
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Identify prepared data
## See "synchrony_data_prep.R" for how this file is created
sync_file <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) %>%
  dplyr::filter(name == "synchrony_data.csv")

# Download it
googledrive::drive_download(file = sync_file$id, path = file.path("tidy_data", sync_file$name), overwrite = T)

# Read in that file
sync_df <- read.csv(file = file.path("tidy_data", "synchrony_data.csv")) %>%
  # Make a species pair column quickly
  dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
                .before = Species1)

# Glimpse it
dplyr::glimpse(sync_df)

# Also prepare a dataframe that is this but with average trait values within species pairs
# Summarize within species pairs / LTER sites
pair_avg_df <- sync_df %>%
  dplyr::group_by(lter, Species_Pair, Species1, Species2) %>%
  # Average if numeric, pick first if categorical
  ## "First" doesn't matter because all species pairs would have same value
  dplyr::summarize(r.spearman = mean(r.spearman, na.rm = T),
                   r.pearson = mean(r.pearson, na.rm = T),
                   r.spearman.detrend = mean(r.spearman.detrend, na.rm = T),
                   r.pearson.detrend = mean(r.pearson.detrend, na.rm = T),
                   overlap = mean(overlap, na.rm = T),
                   Pollinator_code_shared = dplyr::first(Pollinator_code_shared),
                   Seed_development_shared = dplyr::first(Seed_development_shared),
                   Mycorrhiza_shared = dplyr::first(Mycorrhiza_shared),
                   Needleleaf_Broadleaf_shared = dplyr::first(Needleleaf_Broadleaf_shared),
                   Deciduous_Evergreen_shared = dplyr::first(Deciduous_Evergreen_shared),
                   Dispersal_syndrome_shared = dplyr::first(Dispersal_syndrome_shared),
                   Sexual_system_shared = dplyr::first(Sexual_system_shared),
                   Shade_tolerance_shared = dplyr::first(Shade_tolerance_shared),
                   Growth_form_shared = dplyr::first(Growth_form_shared),
                   Fleshy_fruit_shared = dplyr::first(Fleshy_fruit_shared),
                   Seed_bank_shared = dplyr::first(Seed_bank_shared),
                   Seed_mass_similarity = mean(Seed_mass_similarity, na.rm = T),
                   CV_similarity = mean(CV_similarity, na.rm = T),
                   ACL1_similarity = mean(ACL1_similarity, na.rm = T),
                   Phylogenetic_similarity = mean(Phylogenetic_similarity, na.rm = T))

# Check this out
dplyr::glimpse(pair_avg_df)

# See how many rows were lost
nrow(sync_df) - nrow(pair_avg_df)

# Export this to locally and to the Drive
write.csv(x = pair_avg_df, file = file.path("tidy_data", "synchrony_data_spp_averages.csv"), 
          row.names = F, na = '')
googledrive::drive_upload(media = file.path("tidy_data", "synchrony_data_spp_averages.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"))

# Identify output folder
stats_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")

# Check its current contents
googledrive::drive_ls(stats_drive)

# Set permutation number
perm_num <- 10000

# Create a local folder for exporting results to
dir.create(path = file.path("stats_results"), showWarnings = F)

## ------------------------------------------ ##
    # Helpful MRM Extraction Function ----
## ------------------------------------------ ##

# Function for tripping necessary info out of MRM model objects
mrm_extract <- function(mrm_model = NULL, response_nickname = "dist_r_spearman"){
  
  # Error out for missing model
  if(is.null(mrm_model) == T)
    stop("`mrm_model` must be provided")
  
  # Strip out the coefficients, F statistic, R^2, and p value
  mrm_coef <- as.data.frame(mrm_model$coef)
  mrm_f <- mrm_model$F.test[1]
  mrm_r2 <- mrm_model$r.squared[1]
  mrm_pval <- mrm_model$r.squared[2]
  
  # Change name of first column
  names(mrm_coef) <- c(response_nickname, setdiff(names(mrm_coef), names(mrm_coef)[1]))
  
  # Identify the number of blank spaces needed
  spacers <- rep(NA, times = nrow(mrm_coef) - 1)
  
  # Assemble these bits into a flat dataframe
  mrm_df <- mrm_coef %>%
    # Add the single values to this dataframe
    dplyr::mutate(r_squared = c(mrm_r2, spacers),
                  F_value = c(mrm_f, spacers),
                  global_p = c(mrm_pval, spacers)) %>%
    # Also strip out coefficient names as a column (default is as rownames)
    dplyr::mutate(coef = rownames(.), .before = dplyr::everything())
  
  # Drop rownames
  rownames(mrm_df) <- NULL
  
  # Return that object
  return(mrm_df) }

## ------------------------------------------ ##
            # Fit MRMs - Full Data ----
## ------------------------------------------ ##

# These MRMs are conducted on "raw" data (i.e., data that has been tidied/wrangled but 'duplicate' species pairs are allowed to exist within each LTER site)
## This is distinct from averages within species pairs (see below for those analyses)


# Make a faux dataframe that we'll use if/when one of the models below errors out
faux_out <- data.frame(coef = "MODEL FAILED",
                       dist_r_spearman = 0,
                       pval = 99,
                       r_squared = 0,
                       F_value = 0,
                       global_p = 99)

# Make an empty list to store outputs in
total_out <- list()

# For each site, run candidate MRMs and extract their results
for(focal_site in c("All", unique(sync_df$lter))) {
# for(focal_site in "SEV"){

# Keep all sites for the 'across site' stats...
if(focal_site == "All"){ sub_df <- sync_df } else {
  ## Otherwise, subset to a particular site
    sub_df <- dplyr::filter(sync_df, lter == focal_site) }

# Message
message("Fitting model no. 1 (phylogeny only) for LTER: ", focal_site)

# MRM model 1 - phylogeny only
mrm_phylo <- ecodist::MRM(dist(r.spearman) ~ dist(Phylogenetic_similarity),
                         data = sub_df, nperm = perm_num, mrank = T) %>%
  ## Extract information as a dataframe
  mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman")

# Add useful information
phylo_out <- mrm_phylo %>%
  dplyr::mutate(lter = focal_site, 
                model = "phylogeny only",
                .before = dplyr::everything())

# MRM models 2-14 - each trait + phylogeny

# Message
message("Fitting model no. 2-12 (single traits + phylogeny) for LTER: ", focal_site)

# Pollinator Code
try(mrm_poll <- ecodist::MRM(dist(r.spearman) ~ dist(Pollinator_code_shared) + 
                               dist(Phylogenetic_similarity),
                             data = sub_df, nperm = perm_num, mrank = T) %>%
      ## Extract information as a dataframe
      mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
### If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_poll") == FALSE){ mrm_poll <- faux_out }

## Add on useful information
poll_out <- mrm_poll %>%
  ## Add a column indicating focal site(s) and model terms
  dplyr::mutate(lter = focal_site, 
                model = "pollinator code & phylogeny",
                .before = dplyr::everything())

# Mycorrhiza
## Extract
try(mrm_myco <- ecodist::MRM(dist(r.spearman) ~ dist(Mycorrhiza_shared) + 
                               dist(Phylogenetic_similarity),
                             data = sub_df, nperm = perm_num, mrank = T) %>%
      mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## Handle possible error
if(exists(x = "mrm_myco") == FALSE){ mrm_myco <- faux_out }
## Add other relevant information
myco_out <- mrm_myco %>%
  dplyr::mutate(lter = focal_site, 
                model = "mycorrhiza & phylogeny",
                .before = dplyr::everything())

# Seed development
try(mrm_seed_dev <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_development_shared) + 
                                   dist(Phylogenetic_similarity),
                                 data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_seed_dev") == FALSE){ mrm_seed_dev <- faux_out }

# Flesh out finished dataframe
seed_dev_out <- mrm_seed_dev %>%
  dplyr::mutate(lter = focal_site, 
                model = "seed development & phylogeny",
                .before = dplyr::everything())

# Deciduous vs. Evergreen
try(mrm_decid_ever <- ecodist::MRM(dist(r.spearman) ~ dist(Deciduous_Evergreen_shared) + 
                                     dist(Phylogenetic_similarity),
                                   data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_decid_ever") == FALSE){ mrm_decid_ever <- faux_out }

# Flesh out finished dataframe
decid_ever_out <- mrm_decid_ever %>%
  dplyr::mutate(lter = focal_site, 
                model = "deciduous/evergreen & phylogeny",
                .before = dplyr::everything())

# Dispersal mode
try(mrm_disp <- ecodist::MRM(dist(r.spearman) ~ dist(Dispersal_syndrome_shared) + 
                               dist(Phylogenetic_similarity),
                             data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_disp") == FALSE){ mrm_disp <- faux_out }

# Flesh out finished dataframe
disp_out <- mrm_disp %>%
  dplyr::mutate(lter = focal_site, 
                model = "dispersal syndrome & phylogeny",
                .before = dplyr::everything())

# Sexual system
try(mrm_sex <- ecodist::MRM(dist(r.spearman) ~ dist(Sexual_system_shared) + 
                              dist(Phylogenetic_similarity),
                            data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_sex") == FALSE){ mrm_sex <- faux_out }

# Flesh out finished dataframe
sex_out <- mrm_sex %>%
  dplyr::mutate(lter = focal_site, 
                model = "sexual system & phylogeny",
                .before = dplyr::everything())

# Shade
try(mrm_shade <- ecodist::MRM(dist(r.spearman) ~ dist(Shade_tolerance_shared) + 
                                dist(Phylogenetic_similarity),
                              data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_shade") == FALSE){ mrm_shade <- faux_out }

# Flesh out finished dataframe
shade_out <- mrm_shade %>%
  dplyr::mutate(lter = focal_site, 
                model = "shade tolerance & phylogeny",
                .before = dplyr::everything())

# Growth form
try(mrm_growth <- ecodist::MRM(dist(r.spearman) ~ dist(Growth_form_shared) + 
                                 dist(Phylogenetic_similarity),
                               data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_growth") == FALSE){ mrm_growth <- faux_out }

# Flesh out finished dataframe
growth_out <- mrm_growth %>%
  dplyr::mutate(lter = focal_site, 
                model = "growth form & phylogeny",
                .before = dplyr::everything())

# Fleshy fruit
try(mrm_fruit <- ecodist::MRM(dist(r.spearman) ~ dist(Fleshy_fruit_shared) + 
                                dist(Phylogenetic_similarity),
                              data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_fruit") == FALSE){ mrm_fruit <- faux_out }

# Flesh out finished dataframe
fruit_out <- mrm_fruit %>%
  dplyr::mutate(lter = focal_site, 
                model = "fleshy fruit & phylogeny",
                .before = dplyr::everything())

# Seed bank
try(mrm_seed_bank <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_bank_shared) +
                                    dist(Phylogenetic_similarity),
                                  data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_seed_bank") == FALSE){ mrm_seed_bank <- faux_out }

# Flesh out finished dataframe
seed_bank_out <- mrm_seed_bank %>%
  dplyr::mutate(lter = focal_site, 
                model = "seed bank & phylogeny",
                .before = dplyr::everything())

# Seed mass similarity
try(mrm_seed_mass <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) +
                                    dist(Phylogenetic_similarity),
                                  data = sub_df, nperm = perm_num, mrank = T) %>%
       mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
## If that fails, create a placeholder object to streamline results gathering
if(exists(x = "mrm_seed_mass") == FALSE){ mrm_seed_mass <- faux_out }

# Flesh out finished dataframe
seed_mass_out <- mrm_seed_mass %>%
  dplyr::mutate(lter = focal_site, 
                model = "seed mass similarity & phylogeny",
                .before = dplyr::everything())

# MRM model 13 - saturated models

# These need to be site specific because they will succeed or fail based on the specific terms in each model based on each site. The single-term models can either succeed or fail more simply

# Message
message("Fitting model no. 13 (saturated) for LTER: ", focal_site)

# All sites
if(focal_site == "All"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Pollinator_code_shared) + dist(Mycorrhiza_shared) +
                             dist(Seed_development_shared) + dist(Deciduous_Evergreen_shared) + 
                             dist(Dispersal_syndrome_shared) + dist(Sexual_system_shared) + 
                             dist(Shade_tolerance_shared) + dist(Growth_form_shared) + 
                             dist(Fleshy_fruit_shared) + dist(Seed_bank_shared) + 
                             dist(Seed_mass_similarity) + 
                             dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Adirondack
if(focal_site == "ADK"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Pollinator_code_shared) + 
                             dist(Mycorrhiza_shared) + dist(Dispersal_syndrome_shared) + 
                             dist(Shade_tolerance_shared) + dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Andrews Forest
if(focal_site == "AND"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Seed_development_shared) + 
                             dist(Shade_tolerance_shared) + dist(Seed_bank_shared) + 
                             dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Bonanza Creek
if(focal_site == "BNZ"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Deciduous_Evergreen_shared) + 
                             dist(Shade_tolerance_shared) + dist(Growth_form_shared) + 
                             dist(Seed_bank_shared) + dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Cedar Creek
if(focal_site == "CDR"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Seed_development_shared) + 
                             dist(Shade_tolerance_shared) + dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Coweeta
if(focal_site == "CWT"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Pollinator_code_shared) + 
                             dist(Mycorrhiza_shared) + dist(Deciduous_Evergreen_shared) + 
                             dist(Dispersal_syndrome_shared) + dist(Sexual_system_shared) + 
                             dist(Shade_tolerance_shared) + dist(Growth_form_shared) + 
                             dist(Fleshy_fruit_shared) + dist(Seed_bank_shared) + 
                             dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Hubbard Brook
if(focal_site == "HBR"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Mycorrhiza_shared) +
                             dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Luquillo
if(focal_site == "LUQ"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Pollinator_code_shared) + 
                             dist(Mycorrhiza_shared) + dist(Deciduous_Evergreen_shared) + 
                             dist(Dispersal_syndrome_shared) + dist(Sexual_system_shared) + 
                             dist(Shade_tolerance_shared) + dist(Growth_form_shared) + 
                             dist(Fleshy_fruit_shared) + dist(Seed_bank_shared) + 
                             dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }
# Sevilleta
if(focal_site == "SEV"){
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) + 
                             dist(Mycorrhiza_shared) +
                             dist(Phylogenetic_similarity),
                           data = sub_df, nperm = perm_num, mrank = T) }

# Extract relevant information
sink_out <- mrm_extract(mrm_model = mrm_sink, response_nickname = "dist_r_spearman") %>%
  # Add useful information regardless of which iteration of the loop we're currently in
  dplyr::mutate(lter = focal_site, 
                model = "saturated model",
                .before = dplyr::everything())

# Message
message("Wrangling outputs for LTER: ", focal_site)

# Identify number of unique species and species pairs for this site
num_unq_spp <- length(unique(c(sub_df$Species1, sub_df$Species2)))
num_pairs <- length(unique(sub_df$Species_Pair))

# Bind together all separate model outputs
site_full_out <- phylo_out %>%
  dplyr::bind_rows(y = poll_out) %>%
  dplyr::bind_rows(y = myco_out) %>%
  dplyr::bind_rows(y = seed_dev_out) %>%
  dplyr::bind_rows(y = decid_ever_out) %>%
  dplyr::bind_rows(y = disp_out) %>%
  dplyr::bind_rows(y = sex_out) %>%
  dplyr::bind_rows(y = shade_out) %>%
  dplyr::bind_rows(y = growth_out) %>%
  dplyr::bind_rows(y = fruit_out) %>%
  dplyr::bind_rows(y = seed_bank_out) %>%
  dplyr::bind_rows(y = seed_mass_out) %>%
  # Also add saturated model
  dplyr::bind_rows(y = sink_out) %>%
  # Add in number of species/species pairs
  dplyr::mutate(unique_species_ct = num_unq_spp,
                species_pair_ct = num_pairs,
                .after = lter)
  
# Add this object to our output list
total_out[[focal_site]] <- site_full_out

# Clean up environment for next loop
## VITAL because need objects to exist/not exist based on each iteration of loop
## Leaving them in environment would make a failed model just user the previous loop's output (but wouldn't throw an error which would be doubly bad)
rm(list = c("mrm_phylo", "mrm_poll", "mrm_myco", "mrm_seed_dev", "mrm_decid_ever", "mrm_disp",
            "mrm_sex", "mrm_shade", "mrm_growth", "mrm_fruit", "mrm_seed_bank", "mrm_seed_mass",
            #"mrm_cv", "mrm_acl1", 
            "mrm_sink",
            "phylo_out", "poll_out", "myco_out", "seed_dev_out", "decid_ever_out", "disp_out",
            "sex_out", "shade_out", "growth_out", "fruit_out", "seed_bank_out", "seed_mass_out",
            #"cv_out", "acl1_out", 
            "sink_out"))

# Message
message("Processing complete for LTER: ", focal_site)

} # Close loop

## ------------------------------------------ ##
          # MRM Export - Full Data ----
## ------------------------------------------ ##
# Process the big list we're left with
total_df <- total_out %>%
  # Unlist
  purrr::list_rbind(x = .)

# Check this out
dplyr::glimpse(total_df)

# Generate a timestamped name
(file_name <- paste0("MRM_not_averaged_results_", Sys.Date(), "_", perm_num, "perm.csv"))

# Export as a CSV locally
write.csv(x = total_df, file = file.path("stats_results", file_name), row.names = F, na = '')

# Upload this CSV there
googledrive::drive_upload(media = file.path("stats_results", file_name), path = stats_drive, overwrite = T)

# Clear environment of almost everything
rm(list = setdiff(ls(), c("sync_df", "pair_avg_df", "stats_drive", 
                          "perm_num", "mrm_extract")))

## ------------------------------------------ ##
    # Sensitivity Analysis - Full Data ----
## ------------------------------------------ ##

# Make a faux dataframe that we'll use if/when one of the models below errors out
faux_out <- data.frame(coef = "MODEL FAILED",
                       dist_r_spearman = 0,
                       pval = 99,
                       r_squared = 0,
                       F_value = 0,
                       global_p = 99)

# Make an empty list to store outputs in
sens_out <- list()

# For each site, *exclude* it and run cross-site models without it
for(exclude_site in unique(sync_df$lter)) {
  # for(exclude_site in "SEV"){
  
  # Remove that site
  remain_df <- sync_df %>%
    dplyr::filter(lter != exclude_site)
  
  # Message
  message("Fitting model no. 1 (phylogeny only) *without* ", exclude_site)
  
  # MRM model 1 - phylogeny only
  mrm_phylo <- ecodist::MRM(dist(r.spearman) ~ dist(Phylogenetic_similarity),
                            data = remain_df, nperm = perm_num, mrank = T) %>%
    ## Extract information as a dataframe
    mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman")
  
  # Add useful information
  phylo_out <- mrm_phylo %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "phylogeny only",
                  .before = dplyr::everything())
  
  # MRM models 2-14 - each trait + phylogeny
  
  # Message
  message("Fitting model no. 2-12 (single traits + phylogeny) without ", exclude_site)
  
  # Pollinator Code
  try(mrm_poll <- ecodist::MRM(dist(r.spearman) ~ dist(Pollinator_code_shared) + 
                                 dist(Phylogenetic_similarity),
                               data = remain_df, nperm = perm_num, mrank = T) %>%
        ## Extract information as a dataframe
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ### If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_poll") == FALSE){ mrm_poll <- faux_out }
  
  ## Add on useful information
  poll_out <- mrm_poll %>%
    ## Add a column indicating focal site(s) and model terms
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "pollinator code & phylogeny",
                  .before = dplyr::everything())
  
  # Mycorrhiza
  ## Extract
  try(mrm_myco <- ecodist::MRM(dist(r.spearman) ~ dist(Mycorrhiza_shared) + 
                                 dist(Phylogenetic_similarity),
                               data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## Handle possible error
  if(exists(x = "mrm_myco") == FALSE){ mrm_myco <- faux_out }
  ## Add other relevant information
  myco_out <- mrm_myco %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "mycorrhiza & phylogeny",
                  .before = dplyr::everything())
  
  # Seed development
  try(mrm_seed_dev <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_development_shared) + 
                                     dist(Phylogenetic_similarity),
                                   data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_seed_dev") == FALSE){ mrm_seed_dev <- faux_out }
  
  # Flesh out finished dataframe
  seed_dev_out <- mrm_seed_dev %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "seed development & phylogeny",
                  .before = dplyr::everything())
  
  # Deciduous vs. Evergreen
  try(mrm_decid_ever <- ecodist::MRM(dist(r.spearman) ~ dist(Deciduous_Evergreen_shared) + 
                                       dist(Phylogenetic_similarity),
                                     data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_decid_ever") == FALSE){ mrm_decid_ever <- faux_out }
  
  # Flesh out finished dataframe
  decid_ever_out <- mrm_decid_ever %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "deciduous/evergreen & phylogeny",
                  .before = dplyr::everything())
  
  # Dispersal mode
  try(mrm_disp <- ecodist::MRM(dist(r.spearman) ~ dist(Dispersal_syndrome_shared) + 
                                 dist(Phylogenetic_similarity),
                               data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_disp") == FALSE){ mrm_disp <- faux_out }
  
  # Flesh out finished dataframe
  disp_out <- mrm_disp %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "dispersal syndrome & phylogeny",
                  .before = dplyr::everything())
  
  # Sexual system
  try(mrm_sex <- ecodist::MRM(dist(r.spearman) ~ dist(Sexual_system_shared) + 
                                dist(Phylogenetic_similarity),
                              data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_sex") == FALSE){ mrm_sex <- faux_out }
  
  # Flesh out finished dataframe
  sex_out <- mrm_sex %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "sexual system & phylogeny",
                  .before = dplyr::everything())
  
  # Shade
  try(mrm_shade <- ecodist::MRM(dist(r.spearman) ~ dist(Shade_tolerance_shared) + 
                                  dist(Phylogenetic_similarity),
                                data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_shade") == FALSE){ mrm_shade <- faux_out }
  
  # Flesh out finished dataframe
  shade_out <- mrm_shade %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "shade tolerance & phylogeny",
                  .before = dplyr::everything())
  
  # Growth form
  try(mrm_growth <- ecodist::MRM(dist(r.spearman) ~ dist(Growth_form_shared) + 
                                   dist(Phylogenetic_similarity),
                                 data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_growth") == FALSE){ mrm_growth <- faux_out }
  
  # Flesh out finished dataframe
  growth_out <- mrm_growth %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "growth form & phylogeny",
                  .before = dplyr::everything())
  
  # Fleshy fruit
  try(mrm_fruit <- ecodist::MRM(dist(r.spearman) ~ dist(Fleshy_fruit_shared) + 
                                  dist(Phylogenetic_similarity),
                                data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_fruit") == FALSE){ mrm_fruit <- faux_out }
  
  # Flesh out finished dataframe
  fruit_out <- mrm_fruit %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "fleshy fruit & phylogeny",
                  .before = dplyr::everything())
  
  # Seed bank
  try(mrm_seed_bank <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_bank_shared) +
                                      dist(Phylogenetic_similarity),
                                    data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_seed_bank") == FALSE){ mrm_seed_bank <- faux_out }
  
  # Flesh out finished dataframe
  seed_bank_out <- mrm_seed_bank %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "seed bank & phylogeny",
                  .before = dplyr::everything())
  
  # Seed mass similarity
  try(mrm_seed_mass <- ecodist::MRM(dist(r.spearman) ~ dist(Seed_mass_similarity) +
                                      dist(Phylogenetic_similarity),
                                    data = remain_df, nperm = perm_num, mrank = T) %>%
        mrm_extract(mrm_model = ., response_nickname = "dist_r_spearman"), silent = T)
  ## If that fails, create a placeholder object to streamline results gathering
  if(exists(x = "mrm_seed_mass") == FALSE){ mrm_seed_mass <- faux_out }
  
  # Flesh out finished dataframe
  seed_mass_out <- mrm_seed_mass %>%
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "seed mass similarity & phylogeny",
                  .before = dplyr::everything())
  
  # MRM model 13 - saturated models
  
  # These need to be site specific because they will succeed or fail based on the specific terms in each model based on each site. The single-term models can either succeed or fail more simply
  
  # Message
  message("Fitting model no. 13 (saturated) without ", exclude_site)
  
  # All sites
  mrm_sink <- ecodist::MRM(dist(r.spearman) ~ dist(Pollinator_code_shared) + dist(Mycorrhiza_shared) +
                             dist(Seed_development_shared) + dist(Deciduous_Evergreen_shared) + 
                             dist(Dispersal_syndrome_shared) + dist(Sexual_system_shared) + 
                             dist(Shade_tolerance_shared) + dist(Growth_form_shared) + 
                             dist(Fleshy_fruit_shared) + dist(Seed_bank_shared) + 
                             dist(Seed_mass_similarity) + 
                             dist(Phylogenetic_similarity),
                           data = remain_df, nperm = perm_num, mrank = T)
  
  # Extract relevant information
  sink_out <- mrm_extract(mrm_model = mrm_sink, response_nickname = "dist_r_spearman") %>%
    # Add useful information regardless of which iteration of the loop we're currently in
    dplyr::mutate(lter_excluded = exclude_site, 
                  model = "saturated model",
                  .before = dplyr::everything())
  
  # MRM output combination
  
  # Message
  message("Wrangling outputs without ", exclude_site)
  
  # Identify number of unique species and species pairs for this site
  num_unq_spp <- length(unique(c(remain_df$Species1, remain_df$Species2)))
  num_pairs <- length(unique(remain_df$Species_Pair))
  
  # Bind together all separate model outputs
  full_out <- phylo_out %>%
    dplyr::bind_rows(y = poll_out) %>%
    dplyr::bind_rows(y = myco_out) %>%
    dplyr::bind_rows(y = seed_dev_out) %>%
    dplyr::bind_rows(y = decid_ever_out) %>%
    dplyr::bind_rows(y = disp_out) %>%
    dplyr::bind_rows(y = sex_out) %>%
    dplyr::bind_rows(y = shade_out) %>%
    dplyr::bind_rows(y = growth_out) %>%
    dplyr::bind_rows(y = fruit_out) %>%
    dplyr::bind_rows(y = seed_bank_out) %>%
    dplyr::bind_rows(y = seed_mass_out) %>%
    # Also add saturated model
    dplyr::bind_rows(y = sink_out) %>%
    # Add in number of species/species pairs
    dplyr::mutate(unique_species_ct = num_unq_spp,
                  species_pair_ct = num_pairs,
                  .after = lter_excluded)
  
  # Add this object to our output list
  sens_out[[exclude_site]] <- full_out
  
  # Clean up environment for next loop
  ## VITAL because need objects to exist/not exist based on each iteration of loop
  ## Leaving them in environment would make a failed model just user the previous loop's output (but wouldn't throw an error which would be doubly bad)
  rm(list = c("mrm_phylo", "mrm_poll", "mrm_myco", "mrm_seed_dev", "mrm_decid_ever", "mrm_disp",
              "mrm_sex", "mrm_shade", "mrm_growth", "mrm_fruit", "mrm_seed_bank", "mrm_seed_mass",
              #"mrm_cv", "mrm_acl1", 
              "mrm_sink",
              "phylo_out", "poll_out", "myco_out", "seed_dev_out", "decid_ever_out", "disp_out",
              "sex_out", "shade_out", "growth_out", "fruit_out", "seed_bank_out", "seed_mass_out",
              #"cv_out", "acl1_out", 
              "sink_out"))
  
  # Message
  message("Processing complete without ", exclude_site)
  
} # Close loop

## ------------------------------------------ ##
     # Sensitivity Export - Full Data ----
## ------------------------------------------ ##
# Process the big list we're left with
sens_df <- sens_out %>%
  # Unlist
  purrr::list_rbind(x = .)

# Check this out
dplyr::glimpse(sens_df)

# Generate a timestamped name
(file_name <- paste0("MRM_sensitivity_analyses_", Sys.Date(), "_", perm_num, "perm.csv"))

# Export as a CSV locally
write.csv(x = sens_df, file = file.path("stats_results", file_name), row.names = F, na = '')

# Upload this CSV there
googledrive::drive_upload(media = file.path("stats_results", file_name), path = stats_drive, overwrite = T)

# Clear environment of almost everything
rm(list = setdiff(ls(), c("sync_df", "pair_avg_df", "stats_drive", 
                          "perm_num", "mrm_extract")))

# End ----

