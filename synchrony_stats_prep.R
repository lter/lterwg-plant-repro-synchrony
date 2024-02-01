## ------------------------------------------ ##
          # Synchrony MRMs - Data Prep
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Synchrony analysis requires a significant amount of data wrangling.
## This script accomplishes all of that so the analysis script can be more streamlined.

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, supportR)

## ------------------------------------------ ##
              # Download Data ----
## ------------------------------------------ ##

# Identify tidy file(s) to download
tidy_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1aPdQBNlrmyWKtVkcCzY0jBGnYNHnwpeE"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("pairwise_corr.csv", "masting_summary_stats.csv"))

# Identify trait file(s) to download
trait_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1PGaPAkNz1lmvZQMwwthmS-ZjQ97BS2Am"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("LTER_integrated_attributes_USDA_2022-12-14.csv"))

# Identify phylogenetic distance file(s) to download
phylo_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1IVY6i79REaF59kZEBbrJCRNOlcxE7Tel"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("phylo distance matrix.csv"))

# Combine these file sets
wanted_files <- tidy_files %>%
  dplyr::bind_rows(trait_files) %>%
  dplyr::bind_rows(phylo_files)

# Check this out to make sure it includes all files that we need
wanted_files

# Create a folder to write these files to
dir.create(path = file.path("source_data"), showWarnings = F)

# Download these files into that folder
purrr::walk2(.x = wanted_files$id,
             .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x),
                                                path = file.path("source_data", .y),
                                                overwrite = T))

# Make a folder to export processed data in
dir.create(path = file.path("tidy_data"), showWarnings = F)

## ------------------------------------------ ##
          # Prepare to Merge Data ----
## ------------------------------------------ ##

# Read in pairwise correlations
pair_cor <- read.csv(file = file.path("source_data", "pairwise_corr.csv"))

# Read in summary statistics
mast_summary <- read.csv(file = file.path("source_data", "masting_summary_stats.csv"))

# Simplify the mast summary statistics
mast_v2 <- mast_summary %>%
  # Pare down to fewer columns
  dplyr::select(lter, Species.Name, Plot.ID, CV.raw, ACL1.raw) %>%
  # Standardize LTER names
  dplyr::mutate(lter = dplyr::case_when(
    lter == "Coweeta" ~ "CWT",
    lter == "Luquillo" ~ "LUQ",
    lter == "Sevilleta" ~ "SEV",
    lter == "adirondack" ~ "ADK",
    TRUE ~ lter)) %>%
  # Remove duplicate rows if any exist
  dplyr::distinct()

# Glimpse it
dplyr::glimpse(mast_v2)

# Read in the relevant CSV of traits
traits <- read.csv(file = file.path("source_data", "LTER_integrated_attributes_USDA_2022-12-14.csv"))
#Note: 29 March 2023 - The seed mass for Eugenia stahill was changed to 4651 mg (Source: Francis and Rodriguez, 1993): rn_so374.pdf (usda.gov)

# Wrangle traits data
traits_v2 <- traits %>%
  # Clean up species name column
  dplyr::mutate(Species.Name = gsub(pattern = " ", replacement = ".", x = species)) %>%
  # Pare down to subset of traits (columns)
  dplyr::select(Species.Name, Seed_development_1_2or3yrs, Pollinator_code,
                Mycorrhiza_AM_EM, Needleleaf_Broadleaf, 
                Deciduous_Evergreen_yrs, Dispersal_syndrome,
                Sexual_system, Shade_tolerance, Growth_form, Fleshy_fruit,
                Seed_bank, Seed_mass_mg) %>%
  # Standardize these entries (only for text values)
  dplyr::mutate(Pollinator_code = tolower(Pollinator_code),
                Mycorrhiza_AM_EM = tolower(Mycorrhiza_AM_EM),
                Needleleaf_Broadleaf = tolower(Needleleaf_Broadleaf), 
                Deciduous_Evergreen_yrs = tolower(Deciduous_Evergreen_yrs),
                Dispersal_syndrome = tolower(Dispersal_syndrome),
                Sexual_system = tolower(Sexual_system),
                Shade_tolerance = tolower(Shade_tolerance),
                Growth_form = tolower(Growth_form),
                Fleshy_fruit = tolower(Fleshy_fruit),
                Seed_bank = tolower(Seed_bank))

# Check that out
dplyr::glimpse(traits_v2)

# Read in phylogenetic data
phylo <- read.csv(file = file.path("source_data", "phylo distance matrix.csv"))

# Wrangle phylogenetic data
phylo_v2 <- phylo %>%
  # Go to long format
  tidyr::pivot_longer(cols = -X, names_to = "SpeciesY", values_to = "Phylo_distance") %>%
  # Fix a typo in one species' name
  dplyr::mutate(SpeciesY = ifelse(test = (SpeciesY == "Dolichandra_unguis.cati"),
                                  yes = "Dolichandra_unguis-cati",
                                  no = SpeciesY)) %>%
  # Lay the groundwork for species pairs with informative column names
  dplyr::mutate(Species1 = gsub(pattern = "_", replacement = ".", x = X),
                Species2 = gsub(pattern = "_", replacement = ".", x = SpeciesY)) %>%
  # Pare down to only needed columns
  dplyr::select(Species1, Species2, Phylo_distance)

# Examine handiwork
dplyr::glimpse(phylo_v2)

## ------------------------------------------ ##
                # Merge Data ----
## ------------------------------------------ ##

# Set the minimum overlap threshold (in years)
overlap_thresh <- 10

# Create the merged data object
merge_cor <- pair_cor %>%
  # Year overlap has to be ≥ 10 years
  dplyr::filter(overlap >= overlap_thresh) %>%
  # Merge trait information
  dplyr::left_join(y = traits_v2, by = c("Species1" = "Species.Name")) %>%
  # Also bring in masting metrics
  dplyr::left_join(y = mast_v2, by = c("lter", "Plot.ID", "Species1" = "Species.Name")) %>%
  # Rename some of the remaining columns
  dplyr::rename(Pollinator_code_Sp1 = Pollinator_code,
                Seed_development_1_2or3yrs_Sp1 = Seed_development_1_2or3yrs,
                Mycorrhiza_AM_EM_Sp1 = Mycorrhiza_AM_EM,
                Needleleaf_Broadleaf_Sp1 = Needleleaf_Broadleaf,
                Deciduous_Evergreen_yrs_Sp1 = Deciduous_Evergreen_yrs,
                Dispersal_syndrome_Sp1 = Dispersal_syndrome,
                Sexual_system_Sp1 = Sexual_system,
                Shade_tolerance_Sp1 = Shade_tolerance,
                Growth_form_Sp1 = Growth_form,
                Fleshy_fruit_Sp1 = Fleshy_fruit,
                Seed_bank_Sp1 = Seed_bank,
                Seed_mass_Sp1 = Seed_mass_mg,
                CV_Sp1 = CV.raw,
                ACL1_Sp1 = ACL1.raw) %>%
  # Now attach traits and masting to species 2!
  dplyr::left_join(y = traits_v2, by = c("Species2" = "Species.Name")) %>%
  # Also bring in masting metrics
  dplyr::left_join(y = mast_v2, by = c("lter", "Plot.ID", "Species2" = "Species.Name")) %>%
  # Rename those columns for species 2
  dplyr::rename(Pollinator_code_Sp2 = Pollinator_code,
                Seed_development_1_2or3yrs_Sp2 = Seed_development_1_2or3yrs,
                Mycorrhiza_AM_EM_Sp2 = Mycorrhiza_AM_EM,
                Needleleaf_Broadleaf_Sp2 = Needleleaf_Broadleaf,
                Deciduous_Evergreen_yrs_Sp2 = Deciduous_Evergreen_yrs,
                Dispersal_syndrome_Sp2 = Dispersal_syndrome,
                Sexual_system_Sp2 = Sexual_system,
                Shade_tolerance_Sp2 = Shade_tolerance,
                Growth_form_Sp2 = Growth_form,
                Fleshy_fruit_Sp2 = Fleshy_fruit,
                Seed_bank_Sp2 = Seed_bank,
                Seed_mass_Sp2 = Seed_mass_mg,
                CV_Sp2 = CV.raw,
                ACL1_Sp2 = ACL1.raw) %>%
  # Do seed_mass_similarity and CV_similarity globally
  dplyr::mutate(maxdiffSeed = log10(max(Seed_mass_Sp1, Seed_mass_Sp2)) - log10(min(Seed_mass_Sp1, Seed_mass_Sp2)),
                Seed_mass_similarity = 1 - abs((log10(Seed_mass_Sp1) - log10(Seed_mass_Sp2)) / maxdiffSeed),
                maxCV = max(CV_Sp1, CV_Sp2),
                CV_similarity = 1 - abs((CV_Sp1 - CV_Sp2) / maxCV)) %>%
    # Globally (i.e., ungrouped) do more calculations
  dplyr::mutate(maxACL1 = pmax(ACL1_Sp1, ACL1_Sp2),
                minACL1 = pmin(ACL1_Sp1, ACL1_Sp2),
                ACL1_similarity = 1 - (maxACL1 - minACL1)) %>%
  # Identify whether each species pair shares a given trait
  dplyr::mutate(
    ## Pollinators
    Pollinator_code_shared = ifelse(Pollinator_code_Sp1 == Pollinator_code_Sp2, 
                                    yes = 1, no = 0),
    Pollinator_code_values = ifelse(Pollinator_code_Sp1 < Pollinator_code_Sp2,
                                    yes = paste0(Pollinator_code_Sp1, "-", Pollinator_code_Sp2),
                                    no = paste0(Pollinator_code_Sp2, "-", Pollinator_code_Sp1)),
    ## Seed dev.
    Seed_development_shared = ifelse(Seed_development_1_2or3yrs_Sp1 == Seed_development_1_2or3yrs_Sp2, 
                                     yes = 1, no = 0),
    Seed_development_values = ifelse(Seed_development_1_2or3yrs_Sp1 < Seed_development_1_2or3yrs_Sp2,
                                     yes = paste0(Seed_development_1_2or3yrs_Sp1, "-",
                                                  Seed_development_1_2or3yrs_Sp2),
                                     no = paste0(Seed_development_1_2or3yrs_Sp2, "-", 
                                                 Seed_development_1_2or3yrs_Sp1)),
    ## Mycorrhiza
    Mycorrhiza_shared = ifelse(Mycorrhiza_AM_EM_Sp1 == Mycorrhiza_AM_EM_Sp2, 
                               yes = 1, no = 0),
    Mycorrhiza_values = ifelse(Mycorrhiza_AM_EM_Sp1 < Mycorrhiza_AM_EM_Sp2,
                               yes = paste0(Mycorrhiza_AM_EM_Sp1, "-", Mycorrhiza_AM_EM_Sp2),
                               no = paste0(Mycorrhiza_AM_EM_Sp2, "-", Mycorrhiza_AM_EM_Sp1)),
    ## Needle/broadleaf
    Needleleaf_Broadleaf_shared = ifelse(Needleleaf_Broadleaf_Sp1 == Needleleaf_Broadleaf_Sp2, 
                                         yes = 1, no = 0),
    Needleleaf_Broadleaf_values = ifelse(Needleleaf_Broadleaf_Sp1 < Needleleaf_Broadleaf_Sp2,
                                         yes = paste0(Needleleaf_Broadleaf_Sp1, "-", 
                                                      Needleleaf_Broadleaf_Sp2),
                                         no = paste0(Needleleaf_Broadleaf_Sp2, "-", 
                                                     Needleleaf_Broadleaf_Sp1)),
    ## Evergreen/decid.
    Deciduous_Evergreen_shared = ifelse(Deciduous_Evergreen_yrs_Sp1 == Deciduous_Evergreen_yrs_Sp2, 
                                        yes = 1, no = 0),
    Deciduous_Evergreen_values = ifelse(Deciduous_Evergreen_yrs_Sp1 < Deciduous_Evergreen_yrs_Sp2,
                                        yes = paste0(Deciduous_Evergreen_yrs_Sp1, "-", 
                                                     Deciduous_Evergreen_yrs_Sp2),
                                        no = paste0(Deciduous_Evergreen_yrs_Sp2, "-", 
                                                    Deciduous_Evergreen_yrs_Sp1)),
    ## Dispersal
    Dispersal_syndrome_shared = ifelse(Dispersal_syndrome_Sp1 == Dispersal_syndrome_Sp2, 
                                       yes = 1, no = 0),
    Dispersal_syndrome_values = ifelse(Dispersal_syndrome_Sp1 < Dispersal_syndrome_Sp2,
                                       yes = paste0(Dispersal_syndrome_Sp1, "-",
                                                    Dispersal_syndrome_Sp2),
                                       no = paste0(Dispersal_syndrome_Sp2, "-",
                                                   Dispersal_syndrome_Sp1)),
    ## Sexual sys.
    Sexual_system_shared = ifelse(Sexual_system_Sp1 == Sexual_system_Sp2, 
                                  yes = 1, no = 0),
    Sexual_system_values = ifelse(Sexual_system_Sp1 < Sexual_system_Sp2,
                                  yes = paste0(Sexual_system_Sp1, "-", Sexual_system_Sp2),
                                  no = paste0(Sexual_system_Sp2, "-", Sexual_system_Sp1)),
    ## Shade tolerance
    Shade_tolerance_shared = ifelse(Shade_tolerance_Sp1 == Shade_tolerance_Sp2, 
                                    yes = 1, no = 0),
    Shade_tolerance_values = ifelse(Shade_tolerance_Sp1 < Shade_tolerance_Sp2,
                                    yes = paste0(Shade_tolerance_Sp1, "-", Shade_tolerance_Sp2),
                                    no = paste0(Shade_tolerance_Sp2, "-", Shade_tolerance_Sp1)),
    ## Growth form
    Growth_form_shared = ifelse(Growth_form_Sp1 == Growth_form_Sp2, 
                                yes = 1, no = 0),
    Growth_form_values = ifelse(Growth_form_Sp1 < Growth_form_Sp2,
                                yes = paste0(Growth_form_Sp1, "-", Growth_form_Sp2),
                                no = paste0(Growth_form_Sp2, "-", Growth_form_Sp1)),
    ## Fleshy fruit
    Fleshy_fruit_shared = ifelse(Fleshy_fruit_Sp1 == Fleshy_fruit_Sp2, 
                                 yes = 1, no = 0),
    Fleshy_fruit_values = ifelse(Fleshy_fruit_Sp1 < Fleshy_fruit_Sp2,
                                 yes = paste0(Fleshy_fruit_Sp1, "-", Fleshy_fruit_Sp2),
                                 no = paste0(Fleshy_fruit_Sp2, "-", Fleshy_fruit_Sp1)),
    ## Seed bank
    Seed_bank_shared = ifelse(Seed_bank_Sp1 == Seed_bank_Sp2, 
                              yes = 1, no = 0),
    Seed_bank_values = ifelse(Seed_bank_Sp1 < Seed_bank_Sp2,
                              yes = paste0(Seed_bank_Sp1, "-", Seed_bank_Sp2),
                              no = paste0(Seed_bank_Sp2, "-", Seed_bank_Sp1)) ) %>%
  # Pare down to only needed columns
  dplyr::select(lter:overlap, contains("shared"), contains("similarity"), 
                contains("values"), Seed_mass_Sp1, Seed_mass_Sp2) %>% 
  # Drop some unwanted columns specifically
  dplyr::select(-r.pearson, -r.spearman.detrend, -r.pearson.detrend,
                -CV_similarity, -ACL1_similarity)

# Glimpse this
dplyr::glimpse(merge_cor)

# If this worked, you should not gain/lose rows (except filter step for minimum overlap duration)
nrow(dplyr::filter(pair_cor, overlap >= overlap_thresh)); nrow(merge_cor)

# Merge the phylogenetic distance into this big dataframe
sync_df <- merge_cor %>%
  # Left join
  dplyr::left_join(y = phylo_v2, by = c("Species1", "Species2")) %>%
  # Identify maximum phylogenetic distance and calculate similarity
  dplyr::mutate(maxPhylo = max(Phylo_distance, na.rm = T),
                Phylogenetic_similarity = 1 - (Phylo_distance / maxPhylo)) %>%
  # Drop maximum phylogenetic distance column
  dplyr::select(-maxPhylo) %>%
  # Drop Adirondack entirely
  dplyr::filter(lter != "ADK") %>%
  # Drop any non-unique rows too
  dplyr::distinct()

# Calculate variant of Jaccard's distance for the following traits
names(sync_df[,7:18])
sync_df$TraitSimilarityJaccardVariant <- rowSums(sync_df[,7:18]) / 12

# Take a look at this object
dplyr::glimpse(sync_df)

# Export locally
write.csv(x = sync_df, file = file.path("tidy_data", "synchrony_data.csv"),
          row.names = F, na = '')

# Upload this to the Drive
googledrive::drive_upload(media = file.path("tidy_data", "synchrony_data.csv"),
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"),
                          overwrite = TRUE)

# Partially clear environment
rm(list = c("mast_summary", "mast_v2", "merge_cor", "pair_cor", "phylo",  "phylo_v2", 
            "overlap_thresh", "phylo_files", "tidy_files", "trait_files", "traits", 
            "traits_v2", "wanted_files"))

## ------------------------------------------ ##
    # Synchrony + PCoA Diff Creation ----
## ------------------------------------------ ##

# Identify PCoA axis values in Drive
pc_file <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), type = "csv") %>%
  # Filter to desired file
  dplyr::filter(name == "trait_space_pcoa_axes.csv")

# Download PCoA axis values CSV
googledrive::drive_download(file = pc_file$id, overwrite = T, 
                            path = file.path("source_data", pc_file$name))

# Read in
pc_axes <- read.csv(file.path("source_data", "trait_space_pcoa_axes.csv"))

# Check that out
dplyr::glimpse(pc_axes)

# Want difference in first principal coordinate axis difference for each species pair
pc_sync_df <- sync_df %>%
  # Bring in loadings for species 1 and 2
  dplyr::mutate(Sp1_PCoA_Axis_1 = pc_axes$PCoA_Axis_1[match(.$Species1, 
                                                            pc_axes$Species.Name)],
                Sp2_PCoA_Axis_1 = pc_axes$PCoA_Axis_1[match(.$Species2, 
                                                            pc_axes$Species.Name)]) %>%
  # Find axis loading difference
  dplyr::mutate(pcoa_diff_abs = abs((Sp1_PCoA_Axis_1 - Sp2_PCoA_Axis_1)))

# Check that out
dplyr::glimpse(pc_sync_df)

# Export locally
write.csv(pc_sync_df, file = file.path("tidy_data", "synchrony_pcoa_combination.csv"), 
          row.names = F, na = '')

# Upload to Drive
googledrive::drive_upload(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), media = file.path("tidy_data", "synchrony_pcoa_combination.csv"), overwrite = T)

## ------------------------------------------ ##
  # Synchrony + PCoA + Climate Creation ----
## ------------------------------------------ ##

# Identify climate files
clim_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1tPM28pofJbpKXod3rXq-QEqjxGcU8xgt"), type = "csv") %>%
  dplyr::filter(name %in% c("climateSites_tidy_ANDupdate.csv", "plot_data_ClimateSites.csv"))

# Check out files
clim_files

# Download climate data and climate plot info
purrr::walk2(.x = clim_files$id, .y = clim_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x),
                                                path = file.path("source_data", .y),
                                                overwrite = T))

# Read in climate data
clim <- read.csv(file.path("source_data", "climateSites_tidy_ANDupdate.csv")) %>%
  # Pare down to desired columns
  dplyr::select(-X) %>%
  dplyr::rename(lter = SiteCode)%>%
  # Drop Adirondack
  dplyr::filter(lter != "ADK") %>%
  # Remove duplicate rows (if any)
  dplyr::distinct() %>% 
  # Drop unwanted climate information
  dplyr::select(-MAP_mean, -MAT, -TMAX, -TMIN, -Tdiff, -AET_SD, -MAT_SD, -CWD)

# Read in special climate sites data
clim_plots <- read.csv(file.path("source_data", "plot_data_ClimateSites.csv")) %>%
  # Rename site and plot columns
  dplyr::rename(lter = LTER.Site) %>%
  # Drop Adirondack
  dplyr::filter(lter != "ADK") %>%
  # Modify the entry of some supersites
  dplyr::mutate(lter = SiteCode, 
                supersite = dplyr::case_when(
                  lter == "LUQ" ~ "1",
                  lter == "CDR" ~ "CDR",
                  TRUE ~ supersite)) %>%
  # Drop unwanted columns
  dplyr::select(-SiteCode, -Plot.ID) %>%
  # Remove duplicate rows (if any)
  dplyr::distinct() %>%
  # Create an LTER specific plot ID column
  dplyr::mutate(lter_plot = paste0(lter, "__", supersite)) %>% 
  # Average coordinates within established plots
  dplyr::group_by(lter, supersite, climatesite, lter_plot) %>% 
  dplyr::summarize(Latitude_dd = mean(Latitude_dd, na.rm = T),
                   Longitude_dd = mean(Longitude_dd, na.rm = T)) %>% 
  dplyr::ungroup()
  
# Look at both
dplyr::glimpse(clim)
dplyr::glimpse(clim_plots)
dplyr::glimpse(pc_sync_df)

# Merge climate plots with rest of (non-climate) data
merged <- pc_sync_df %>%
  # Create the LTER + plot column in the climate plot dataframe
  dplyr::mutate(lter_plot = paste0(lter, "__", Plot.ID),
                .before = dplyr::everything()) %>%
  # Do the merging
  dplyr::left_join(y = clim_plots, by = c("lter", "Plot.ID" = "supersite", "lter_plot"))

# Now combine *that* with the actual climate data
pc_clim_sync_df <- merged %>%
  dplyr::left_join(clim, by = c("lter", "climatesite")) %>%
  # Rename some columns
  dplyr::rename(climate_lat = lat,
                climate_long = long) %>%
  # Relocate the climate coordinates
  dplyr::relocate(climate_lat, climate_long,
                  .after = Longitude_dd) %>%
  # Create a species pair column
  dplyr::mutate(speciespair = paste(Species1, Species2, sep = "-"),
                .after = Species2)

# Check out what that returns
dplyr::glimpse(pc_clim_sync_df)

# Export locally
write.csv(pc_clim_sync_df, row.names = F, na = '',
          file = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv"))

# Upload to Drive
googledrive::drive_upload(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"), media = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv"), overwrite = T)

# End ----
