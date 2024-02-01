## ------------------------------------------ ##
        # Synchrony Visualization Prep
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Perform pre-visualization prep
## Several tidy data files + stats results files need to be combined for to make desired figures

# PRE-REQUISITES
## The following script(s) must be run--in order--for this script to work as intended
## 1. Run `synchrony_stats_prep.R`
## 2. Run `synchrony_mrm.R`
## 3. Run `synchrony_perm-stats.R`
## 4. Run `synchrony_anova.R`
## 5. Run `synchrony_similarity-stats.R`

## ------------------------------------------ ##
                # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR, magrittr)

# Clear environment
rm(list = ls())

# Create needed local folder(s)
dir.create(path = file.path("figure_data"), showWarnings = F)

# Identify any file names with time stamps
## Done here to make updating time stamps easier
mrm_file <- "MRM_not_averaged_results_2023-06-14_10000perm.csv" # MRM results
aov_file <- "ANOVA_trait_aov_tables_2023-06-14_10000perm.csv" # ANOVA results (trait levels)
pair_file <- "ANOVA_trait_pairwise_comps_2023-06-14_10000perm.csv" # ANOVA pairwise comps
stat_aov_file <- "ANOVA_trait_status_aov_tables_2023-06-14_10000perm.csv" # ANOVA on trait status (0 vs 1)

## ------------------------------------------ ##
        # Wrangle - Synchrony Data ----
## ------------------------------------------ ##

# Read in synchrony data
sync_df <- read.csv(file = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv")) %>%
  # Make a species pair column quickly
  dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
                .before = Species1) %>%
  # Drop needleleaf vs broadleaf
  dplyr::select(-dplyr::starts_with("Needleleaf_Broadleaf_")) %>%
  # Rename 'deciduous/evergreen' column
  dplyr::rename(Leaf_Longevity_values = Deciduous_Evergreen_values) %>% 
  dplyr::rename(Leaf_Longevity_shared = Deciduous_Evergreen_shared) %>% 
  # Add in solid shape values
  dplyr::mutate(solid_shapes = dplyr::case_when(lter %in% c("AND", "HBR") ~ 15,
                                                lter %in% c("BNZ", "LUQ") ~ 16,
                                                lter %in% c("CDR", "SEV") ~ 17,
                                                lter %in% c("CWT") ~ 18))

# Glimpse it
dplyr::glimpse(sync_df)

# Export locally
write.csv(x = sync_df, na = '', row.names = F,
          file = file.path("figure_data", "synchrony_viz-ready.csv"))

## ------------------------------------------ ##
        # Wrangle - Species Traits ----
## ------------------------------------------ ##

# Read in trait information
spp_traits <- read.csv(file = file.path("tidy_data", "pre_ordination_trait_data.csv")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = -lter:-Species.Name,
                      names_to = "trait", values_to = "trait_value") %>%
  # Streamline trait names to make NMS trait vectors simpler
  dplyr::mutate(trait_actual = dplyr::case_when(
    trait == "Deciduous_Evergreen_yrs__deciduous" ~ "Deciduous",
    trait == "Deciduous_Evergreen_yrs__evergreen" ~ "Evergreen",
    trait == "Dispersal_syndrome__abiotic" ~ "Abiotic_disp.",
    trait == "Dispersal_syndrome__endozoochory" ~ "Endozoochory",
    trait == "Dispersal_syndrome__synzoochory" ~ "Synzoochory",
    trait == "Fleshy_fruit__no" ~ "Dry_fruit",
    trait == "Fleshy_fruit__yes" ~ "Fleshy_fruit",
    trait == "Growth_form__liana" ~ "Liana",
    trait == "Growth_form__shrub" ~ "Shrub",
    trait == "Growth_form__tree" ~ "Tree",
    trait == "Log10_seed_mass_mg" ~ "Seed_mass",
    trait == "Mycorrhiza_AM_EM__am" ~ "AM",
    trait == "Mycorrhiza_AM_EM__em" ~ "EM",
    trait == "Mycorrhiza_AM_EM__ericoid" ~ "Ericoid",
    trait == "Mycorrhiza_AM_EM__none" ~ "No_mycorr.",
    trait == "Pollinator_code__animal" ~ "Animal_pollinated",
    trait == "Pollinator_code__wind" ~ "Wind_pollinated",
    trait == "Seed_bank__no" ~ "No_seed_bank",
    trait == "Seed_bank__yes" ~ "Seed_bank",
    trait == "Seed_development_1_2or3yrs" ~ "Seed_dev_time",
    trait == "Sexual_system__dioecious" ~ "Dioecious",
    trait == "Sexual_system__hermaphrodite" ~ "Hermaphrodite",
    trait == "Sexual_system__monoecious" ~ "Monoecious",
    trait == "Sexual_system__polygamo_dioecious" ~ "Polygamo_dioecious",
    trait == "Shade_tolerance__intermediate" ~ "Shade_med._tolerant",
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

# Export locally
write.csv(x = spp_traits, na = '', row.names = F,
          file = file.path("figure_data", "traits_viz-ready.csv"))

## ------------------------------------------ ##
    # Wrangle - Observed vs. Permuted ----
## ------------------------------------------ ##
# Read in permutations of correlations
perm_df <- read.csv(file = file.path("tidy_data", "permutation_corr_unsummarized.csv")) %>%
  # Cut off below overlap threshold
  dplyr::filter(overlap > 9) %>%
  # Filter to only desired LTERs
  dplyr::filter(lter %in% c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV")) %>%
  # Add in solid shape values
  dplyr::mutate(solid_shapes = dplyr::case_when(lter %in% c("AND", "HBR") ~ 15,
                                                lter %in% c("BNZ", "LUQ") ~ 16,
                                                lter %in% c("CDR", "SEV") ~ 17,
                                                lter %in% c("CWT") ~ 18))

# Check out structure
dplyr::glimpse(perm_df)

# Export locally
write.csv(x = perm_df, na = '', row.names = F,
          file = file.path("figure_data", "perm_viz-ready.csv"))

## ------------------------------------------ ##
          # Wrangle - MRM Results ----
## ------------------------------------------ ##
# Read in MRM results
mrm_results <- read.csv(file = file.path("tidy_data", mrm_file)) %>%
  # Drop all but saturated model
  dplyr::filter(model == "saturated model" & coef != "Int") %>%
  # Make coefficient column match trait name
  dplyr::mutate(coef = gsub(pattern = "dist\\(|\\)", replacement = "", x = coef)) %>%
  # Determine result (sig vs. NS)
  dplyr::mutate(result = ifelse(test = (pval < 0.05), yes = "sig", no = "NS")) %>%
  # Pare down to minimum needed columns
  dplyr::select(lter, coef, result) %>%
  # Need to identify missing trait-site combinations
  ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
  tidyr::pivot_wider(names_from = coef, values_from = result, values_fill = "NA") %>%
  tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "result") %>%
  # Tweak leaf longevity values
  dplyr::mutate(trait = dplyr::case_when(
    trait == "Deciduous_Evergreen_shared" ~ "Leaf_Longevity_shared",
    T ~ trait))

# Glimpse it
dplyr::glimpse(mrm_results)

# Export locally
write.csv(x = mrm_results, na = '', row.names = F,
          file = file.path("figure_data", "mrm_viz-ready.csv"))

## ------------------------------------------ ##
    # Wrangle - ANOVA on 'Levels' (Main) ----
## ------------------------------------------ ##
# Read in ANOVA results too
aov_results <- read.csv(file = file.path("tidy_data", aov_file)) %>%
  # Remove unwanted rows
  dplyr::filter(!term %in% c("Residuals", "Total")) %>%
  # Pare down to minimum needed columns
  dplyr::select(lter, term, P) %>%
  # Get to unique rows only
  dplyr::distinct() %>%
  # Need to identify missing trait-site combinations
  ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
  tidyr::pivot_wider(names_from = term, values_from = P,
                     values_fill = 999) %>%
  tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "P") %>%
  # Determine result (sig vs. NS vs. NA)
  dplyr::mutate(result = dplyr::case_when(P > 10 ~ "NA",
                                          P < 0.05 ~ "sig",
                                          TRUE ~ "NS")) %>%
  # Wrangle P value for use as a label in plots
  dplyr::mutate(P_round = as.character(round(P, digits = 12))) %>%
  dplyr::mutate(P_label = dplyr::case_when(
    as.numeric(P_round) < 0.001 ~ "P < 0.001",
    as.numeric(P_round) > 990 ~ "",
    as.numeric(P_round) >= 0.05 & as.numeric(P_round) < 900 ~ "NS",
    as.numeric(P_round) >= 0.001 & as.numeric(P_round) < 0.05 ~ paste0("P = ", stringr::str_sub(string = P_round, start = 1,  end = 5)))) %>%
  # Drop unwanted columns
  dplyr::select(-P_round, -P) %>% 
  # Tweak leaf longevity values
  dplyr::mutate(trait = dplyr::case_when(
    trait == "Mycorrhiza_values" ~ "Mycorrhizal_assoc._values",
    trait == "Deciduous_Evergreen_values" ~ "Leaf_Longevity_values",
    T ~ trait))

# Glimpse it
dplyr::glimpse(aov_results)

# Export locally
write.csv(x = aov_results, na = '', row.names = F,
          file = file.path("figure_data", "aov-levels_viz-ready.csv"))

## ------------------------------------------ ##
      # ANOVA on 'Levels' (Pairs) ----
## ------------------------------------------ ##
# Read in pairwise comparisons results
aov_pairs <- read.csv(file = file.path("tidy_data", pair_file)) %>%
  # Pare down to minimum needed columns
  dplyr::select(lter, model, pairs, P) %>%
  # Get to unique rows only
  dplyr::distinct() %>% 
  # Make the pairs non hyphenated
  dplyr::mutate(pairs = gsub(pattern = "\\-", replacement = "_", x = pairs)) %>%
  # Separate pairs into two columns
  tidyr::separate_wider_delim(cols = pairs, delim = ":", cols_remove = T,
                              names = c("pair1", "pair2")) %>% 
  # Do any desired manual tweaks of the trait names
  dplyr::mutate(model = dplyr::case_when(
    model == "mycorrhiza values" ~ "mycorrhizal assoc. values",
    model == "deciduous evergreen values" ~ "leaf longevity values",
    T ~ model))

# Glimpse it
dplyr::glimpse(aov_pairs)

# Export locally
write.csv(x = aov_pairs, na = '', row.names = F,
          file = file.path("figure_data", "aov-levels-pairs_viz-ready.csv"))

## ------------------------------------------ ##
         # ANOVA on 'Levels' (CLD) ----
## ------------------------------------------ ##
## CLD = Compact Letter Display

# Make an empty list
aov_cld_list <- list()

# Now we need to generate the 'compact letter display' (aka "CLD") to use in the plots
## Loop across site
for(pairs_lter in sort(unique(aov_pairs$lter))){
  
  # Subset to that site
  pairs_sub <- aov_pairs %>%
    dplyr::filter(lter == pairs_lter)
  
  # Generate an empty list
  pairs_sub_list <- list()
  
  ## Loop across model (within site)
  for(pairs_model in sort(unique(pairs_sub$model))) {
    
    # Subset to just this model
    pairs_meta_sub <- pairs_sub %>%
      dplyr::filter(model == pairs_model)
    
    # Create the necessary object for CLD identification
    pre_cld <- pairs_meta_sub$P
    names(pre_cld) <- paste0(pairs_meta_sub$pair1, "-", pairs_meta_sub$pair2)
    
    # Extract the raw CLD
    raw_cld <- multcompView::multcompLetters(x = pre_cld, Letters = letters)
    
    # Wrangle this into a dataframe with necessary information
    cld_df <- data.frame("lter" = pairs_lter,
                         "trait" = pairs_model,
                         "trait_levels" = gsub(pattern = "\\_", replacement = "-",
                                               x = names(raw_cld$Letters)),
                         "letter" = raw_cld$Letters)
    
    # Drop rownames
    rownames(cld_df) <- NULL
    
    # Add to list
    pairs_sub_list[[pairs_model]] <- cld_df
    
  } # Close within-LTER loop
  
  # Unlist into a dataframe
  pairs_sub_df <- pairs_sub_list %>%
    purrr::list_rbind(x = .)
  
  # And add that dataframe to the larger list
  aov_cld_list[[pairs_lter]] <- pairs_sub_df
  
  # Success message
  message("Finished processing '", pairs_lter, "'")
  
} # Close larger loop

# Unlist to a data frame
aov_cld <- purrr::list_rbind(x = aov_cld_list)

# Glimpse this
dplyr::glimpse(aov_cld)

# Export locally
write.csv(x = aov_cld, na = '', row.names = F,
          file = file.path("figure_data", "aov-levels-cld_viz-ready.csv"))

## ------------------------------------------ ##
      # Wrangle - ANOVA on 'Status' ----
## ------------------------------------------ ##
# Read in trait status ANOVA results too
stat_aov <- read.csv(file.path("tidy_data", stat_aov_file)) %>%
  # Pare down to minimum needed columns
  dplyr::select(lter, term, P) %>%
  # Get to unique rows only
  dplyr::distinct() %>%
  # Filter out non-P value rows
  dplyr::filter(!is.na(P)) %>%
  # Need to identify missing trait-site combinations
  ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
  tidyr::pivot_wider(names_from = term, values_from = P,
                     values_fill = 999) %>%
  tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "P") %>%
  # Determine result (sig vs. NS vs. NA)
  dplyr::mutate(result = dplyr::case_when(P > 10 ~ "NA",
                                          P < 0.05 ~ "sig",
                                          TRUE ~ "NS")) %>%
  # Wrangle P value for use as a label in plots
  dplyr::mutate(P_round = as.character(round(P, digits = 12))) %>%
  dplyr::mutate(
    P_label = dplyr::case_when(as.numeric(P_round) < 0.001 ~ "P < 0.001",
                               as.numeric(P_round) > 990 ~ "",
                               as.numeric(P_round) >= 0.05 & 
                                 as.numeric(P_round) < 900 ~ "NS",
                               as.numeric(P_round) >= 0.001 &
                                 as.numeric(P_round) < 0.05 ~ paste0("P = ",
                                                                     stringr::str_sub(string = P_round, start = 1,  end = 5)))) %>%
  # Drop unwanted columns
  dplyr::select(-P_round, -P) %>% 
  # Tweak leaf longevity values
  dplyr::mutate(trait = dplyr::case_when(
    trait == "Mycorrhiza_shared" ~ "Mycorrhizal_assoc._shared",
    trait == "Deciduous_Evergreen_shared" ~ "Leaf_longevity_shared",
    T ~ trait))

# Glimpse this
dplyr::glimpse(stat_aov)

# Export locally
write.csv(x = stat_aov, na = '', row.names = F,
          file = file.path("figure_data", "aov-status_viz-ready.csv"))

# End ----
