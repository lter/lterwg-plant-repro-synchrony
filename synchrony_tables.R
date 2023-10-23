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

# Identify links of relevant Drive folders
sync_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")

# Identify relevant data from those folders
## List out all CSVs in all folders
(wanted_files <- googledrive::drive_ls(path = sync_folder, type = "csv") %>%
    ## Filter to only desired files
    dplyr::filter(name %in% c(sync_file, trait_file)))

# Create folder to download files into
dir.create(path = file.path("figure_data"), showWarnings = F)

# Download files into that folder
purrr::walk2(.x = wanted_files$id, 
             .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("figure_data", .y),
                                                overwrite = T))

# Make a folder to export tables to
dir.create(path = file.path("table_data"), showWarnings = F)

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

# Read in trait information
spp_traits <- read.csv(file = file.path("figure_data", trait_file)) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = -lter:-Species.Name,
                      names_to = "trait", values_to = "trait_value") %>%
  # Streamline trait names to make NMS trait vectors simpler
  dplyr::mutate(trait_actual = dplyr::case_when(
    trait == "Deciduous_Evergreen_yrs__deciduous" ~ "Deciduous",
    trait == "Deciduous_Evergreen_yrs__evergreen" ~ "Evergreen",
    trait == "Dispersal_syndrome__abiotic" ~ "Abiotic_disp",
    trait == "Dispersal_syndrome__endozoochory" ~ "Endozo_disp",
    trait == "Dispersal_syndrome__synzoochory" ~ "Synzo_disp",
    trait == "Fleshy_fruit__no" ~ "Not_fleshy_fruit",
    trait == "Fleshy_fruit__yes" ~ "Fleshy_fruit",
    trait == "Growth_form__liana" ~ "Liana",
    trait == "Growth_form__shrub" ~ "Shrub",
    trait == "Growth_form__tree" ~ "Tree",
    trait == "Log10_seed_mass_mg" ~ "Log_seed_mass",
    trait == "Mycorrhiza_AM_EM__am" ~ "AM_mycorr",
    trait == "Mycorrhiza_AM_EM__em" ~ "EM_mycorr",
    trait == "Mycorrhiza_AM_EM__ericoid" ~ "Ericoid_mycorr",
    trait == "Mycorrhiza_AM_EM__none" ~ "No_mycorr",
    trait == "Pollinator_code__animal" ~ "Animal_pollinated",
    trait == "Pollinator_code__wind" ~ "Wind_pollinated",
    ## Can't think of a great abbreviation for these two traits
    # trait == "Seed_bank__no" ~ "No_seed_bank",
    # trait == "Seed_bank__yes" ~ "Yes_seed_bank",
    trait == "Seed_development_1_2or3yrs" ~ "Seed_dev_time",
    trait == "Sexual_system__dioecious" ~ "Dioecious",
    trait == "Sexual_system__hermaphrodite" ~ "Hermaphrodite",
    trait == "Sexual_system__monoecious" ~ "Monoecious",
    trait == "Sexual_system__polygamo_dioecious" ~ "Polygamo_dioecious",
    trait == "Shade_tolerance__intermediate" ~ "Shade_intermediate_tolerant",
    trait == "Shade_tolerance__intolerant" ~ "Shade_intolerant",
    trait == "Shade_tolerance__tolerant" ~ "Shade_tolerant",
    TRUE ~ trait)) %>%
  # Drop original trait column
  dplyr::select(-trait) %>%
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = trait_actual, values_from = trait_value) %>%
  # Filter to only desired LTERs
  dplyr::filter(lter %in% c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV"))

# Glimpse it
dplyr::glimpse(spp_traits)

## ------------------------------------------ ##
              # Site Table Data ----
## ------------------------------------------ ##

# Identify number of species per lter
spp_num <- spp_traits %>%
  dplyr::group_by(lter) %>%
  dplyr::summarize(species_ct = length(unique(Species.Name))) %>%
  dplyr::ungroup()

# Look at that
spp_num

# Identify number of traits that vary
var_trait_num <- sync_df %>%
  # Pare down to desired columns
  dplyr::select(lter, dplyr::ends_with("shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("shared"), names_to = "trait") %>%
  # Identify traits that vary
  dplyr::group_by(lter, trait) %>%
  dplyr::summarize(level_ct = length(unique(value))) %>%
  dplyr::ungroup() %>%
  # Filter to only varying traits
  dplyr::filter(level_ct != 1) %>%
  # Count number of varying traits
  dplyr::group_by(lter) %>%
  dplyr::summarize(variable_trait_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  glimpse()

# Check that out
var_trait_num

# Do remaining summarization
plot_table <- sync_df %>%
  # Calculate desired metrics with sites
  dplyr::group_by(lter) %>%
  dplyr::summarize(plot_ct = length(unique(Plot.ID)),
                   mean_CWD = round(mean(CWD, na.rm = T), digits = 1),
                   min_CWD = round(min(CWD, na.rm = T), digits = 1),
                   max_CWD = round(max(CWD, na.rm = T), digits = 1),
                   mean_overlap = round(mean(overlap, na.rm = T), digits = 1),
                   max_overlap = max(overlap, na.rm = T) ) %>%
  dplyr::ungroup() %>%
  # Attach species and varying trait numbers
  dplyr::left_join(y = spp_num, by = "lter") %>%
  dplyr::left_join(y = var_trait_num, by = "lter") %>%
  # Re-order a bit
  dplyr::relocate(species_ct, .before = plot_ct) %>%
  dplyr::relocate(variable_trait_ct, .before = mean_CWD)

# Check that out
dplyr::glimpse(plot_table)

# Export locally
write.csv(plot_table, row.names = F, na = '', 
          file = file.path("table_data", "plot_data_table.csv"))

## ------------------------------------------ ##
          # Trait Summary Values ----
## ------------------------------------------ ##

# Identify trait value columns
trait_vals <- sync_df %>% 
  dplyr::select(dplyr::contains("_values")) %>%
  names()

# Loop across traits
for(trait in trait_vals){
  
  # Summarize various trait information
  trait_tab <- supportR::summary_table(data = sync_df, groups = trait,
                                      response = "r.spearman", drop_na = F)
  
  # Export locally
  write.csv(trait_tab, row.names = F, na = '', 
            file = file.path("table_data", paste0(trait, "_summary_table.csv")))
  
}

## ------------------------------------------ ##
                  # Export ----
## ------------------------------------------ ##

# Identify folder to export plots to
table_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ReUBqmiZK2gwGePNt9VN0qcfcLiwZ_MZ")

# Identify tables that we have created
table <- dir(path = file.path("table_data"))

# Upload each to the Drive (skipping the map if it's there)
for(file in table){
  googledrive::drive_upload(media = file.path("table_data", file),
                            path = table_folder, overwrite = T) }

# End ----
