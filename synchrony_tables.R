## ------------------------------------------ ##
          # Synchrony Summary Tables
## ------------------------------------------ ##
# Written by: Nick J Lyon, Angel Chen

# PURPOSE
## Create summary tables for inclusion in the paper

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR, rstatix)

# Clear environment
rm(list = ls())

# Create needed local folder(s)
dir.create(path = file.path("table_data"), showWarnings = F)

## ------------------------------------------ ##
              # Data Wrangling ----
## ------------------------------------------ ##

# Read in synchrony data
sync_df <- read.csv(file = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv")) %>%
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
spp_traits <- read.csv(file = file.path("tidy_data", "pre_ordination_trait_data.csv")) %>%
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
  dplyr::ungroup()

# Check that out
var_trait_num

# Do remaining summarization
plot_table <- sync_df %>%
  # Calculate desired metrics with sites
  dplyr::group_by(lter) %>%
  dplyr::summarize(plot_ct = length(unique(Plot.ID)),
                   mean_CWD = round(mean(CWD_log, na.rm = T), digits = 1),
                   min_CWD = round(min(CWD_log, na.rm = T), digits = 1),
                   max_CWD = round(max(CWD_log, na.rm = T), digits = 1),
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
#                Table S2 ----
## ------------------------------------------ ##

# ------------------------------- Data Prep ------------------------------------

# Read in the pre ordination trait data
pre_ord_traits <- read.csv(file = file.path("tidy_data", "pre_ordination_trait_data.csv")) %>%
  mutate(Species.Name = gsub("\\."," ", Species.Name)) 

# Read in the full trait data
all_traits <- read.csv(file = file.path("tidy_data", "LTER_integrated_attributes_USDA_2022-12-14.csv")) 

# Grab the exact seed mass values 
seed_mass <- all_traits %>%
  dplyr::select(species, Seed_mass_mg)

pre_ord_with_mass <- pre_ord_traits %>%
  # Left join with our full attributes table for the actual seed mass values
  dplyr::left_join(seed_mass, by = c("Species.Name" = "species")) %>%
  # Drop the log seed mass values
  dplyr::select(-Log10_seed_mass_mg) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = -lter:-Species.Name,
                      names_to = "trait", values_to = "trait_value") %>%
  # Separate the name of the trait 
  tidyr::separate(trait, sep = "__", into = c("trait", "trait_category")) %>%
  # Drop the rows where the species didn't have the trait 
  dplyr::filter(trait_value > 0) %>%
  # Fill in the trait category column with missing seed development and seed mass info 
  dplyr::mutate(trait_category = dplyr::coalesce(trait_category, as.character(trait_value))) %>%
  # Drop the trait value column
  dplyr::select(-trait_value) %>%
  # Rename the trait category column into the true trait value column
  dplyr::rename(trait_value = trait_category) %>%
  # Pivot wider into more columns
  tidyr::pivot_wider(names_from = trait, values_from = trait_value) %>%
  # Convert the seed development and seed mass columns into numeric
  dplyr::mutate(dplyr::across(.cols = c(Seed_development_1_2or3yrs, Seed_mass_mg), .fns = as.numeric)) %>%
  # Filter to only desired LTERs
  dplyr::filter(lter %in% c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV"))

# --------------------- Calculating Quantitative Stats -------------------------

# Calculating overall quantitative stats 
overall_quant_stats <- pre_ord_with_mass %>% 
  dplyr::select(-lter) %>%
  dplyr::distinct() %>%
  get_summary_stats(
    Seed_mass_mg, 
    show = c("n", "min", "max", "mean", "sd")) %>%
  dplyr::rename(num_species = n) %>%
  dplyr::mutate(site = "All data")

# Listing the sites we want
sites_we_want <- c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV")

site_quant_stats <- list()

# Calculating quantitative stats for each individual site
for (site in sites_we_want){
  some_stats <-  pre_ord_with_mass %>% 
    dplyr::filter(lter == site) %>%
    get_summary_stats(
      Seed_mass_mg, 
      show = c("n", "min", "max", "mean", "sd"))
  
  site_quant_stats[[site]] <- some_stats
  
}

# Adding a 'site' column and combining all the site dataframes into one dataframe
site_quant_stats_v2 <- site_quant_stats %>%
  purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
  purrr::map_dfr(.f = select, everything()) %>%
  dplyr::rename(num_species = n)

# Combine the overall and site specific stats together
quant_stats_altogether <- overall_quant_stats %>%
  dplyr::bind_rows(site_quant_stats_v2) %>%
  dplyr::relocate(site, .before = num_species) %>%
  dplyr::rename(trait = variable)

# Exporting quantitative stats
write.csv(quant_stats_altogether, file.path("table_data", "Table_S2_quant_trait_stats.csv"), row.names = FALSE)

# --------------------- Calculating Qualitative Stats --------------------------

# Listing the columns we want
columns_we_want <- c("Deciduous_Evergreen_yrs", "Dispersal_syndrome", "Fleshy_fruit", "Growth_form",
                     "Mycorrhiza_AM_EM", "Pollinator_code", "Seed_bank",
                     "Seed_development_1_2or3yrs", "Sexual_system", "Shade_tolerance")

overall_qual_stats <- list()

# Calculating overall qualitative stats 
for (col in columns_we_want){
  some_stats <- pre_ord_with_mass %>%
    dplyr::select(-lter) %>%
    dplyr::distinct() %>%
    dplyr::group_by(!!sym(col)) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::mutate(variable = col, .before = everything()) %>%
    dplyr::rename(value = !!sym(col)) %>%
    dplyr::mutate(value = as.character(value))
  
  overall_qual_stats[[col]] <- some_stats
  
}

# Combining overall qualitative stats into one dataframe
overall_qual_stats_v2 <- overall_qual_stats %>%
  purrr::map_dfr(.f = select, everything()) %>%
  dplyr::rename(num_species = count) %>%
  dplyr::mutate(site = "All data")

site_qual_stats <- list()

# Calculating qualitative stats for each individual site
for (site in sites_we_want){
  for (col in columns_we_want){
    some_stats <- pre_ord_with_mass %>%
      dplyr::filter(lter == site) %>%
      dplyr::group_by(!!sym(col)) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::mutate(variable = col, .before = everything()) %>%
      dplyr::rename(value = !!sym(col)) %>%
      dplyr::mutate(value = as.character(value))
    
    site_qual_stats[[site]][[col]] <- some_stats
  }
}

# Adding a 'site' column 
for (site in sites_we_want){
  site_qual_stats[[site]] <- site_qual_stats[[site]] %>% 
    purrr::map_dfr(.f = select, everything()) %>%
    dplyr::mutate(site = site, .before = everything())
}

# Combining all the site dataframes into one dataframe
site_qual_stats_v2 <- site_qual_stats %>%
  purrr::map_dfr(.f = select, everything()) %>%
  dplyr::rename(num_species = count) 

# Combine the overall and site specific stats together
qual_stats_altogether <- overall_qual_stats_v2 %>%
  dplyr::bind_rows(site_qual_stats_v2) %>%
  tidyr::pivot_wider(names_from = site, values_from = num_species) %>%
  dplyr::rename(trait = variable) %>%
  dplyr::rename(trait_value = value)

# Exporting qualitative stats
write.csv(qual_stats_altogether, file.path("table_data", "Table_S2_qual_trait_stats.csv"), row.names = FALSE)

# End ----
