## ------------------------------------------ ##
      # Synchrony Permutation Statistics
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Performs statistical analysis on permuted vs. actual correlations
## As of 10/23/23 this corresponds to the data presented in figures 2 and 3A

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
perm_file <- "permutation_corr_unsummarized.csv" # correlation permutation data

# Identify links of relevant Drive folders
sync_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")
gen_data_folder <- googledrive::as_id("https://drive.google.com/drive/folders/1aPdQBNlrmyWKtVkcCzY0jBGnYNHnwpeE")
stats_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")

# Identify relevant data from those folders
## List out all CSVs in all folders
(wanted_files <- googledrive::drive_ls(path = sync_folder, type = "csv") %>%
    dplyr::bind_rows(googledrive::drive_ls(path = gen_data_folder, type = "csv")) %>%
    dplyr::bind_rows(googledrive::drive_ls(path = stats_folder, type = "csv")) %>%
    ## Filter to only desired files
    dplyr::filter(name %in% c(sync_file, perm_file)))

# Create folder to download files into
dir.create(path = file.path("stats_results"), showWarnings = F)
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Download files into that folder
purrr::walk2(.x = wanted_files$id, .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("tidy_data", .y),
                                                overwrite = T))

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

# Read in permutations of correlations
perm_df <- read.csv(file = file.path("figure_data", perm_file)) %>%
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

# Read in MRM results
mrm_results <- read.csv(file = file.path("figure_data", mrm_file)) %>%
  # Drop all but saturated model
  dplyr::filter(model == "saturated model" & coef != "Int") %>%
  # Make coefficient column match trait name
  dplyr::mutate(coef = gsub(pattern = "dist\\(|\\)", replacement = "", x = coef)) %>%
  # Determine result (sig vs. NS)
  dplyr::mutate(result = ifelse(test = (pval < 0.05),
                                yes = "sig", no = "NS")) %>%
  # Pare down to minimum needed columns
  dplyr::select(lter, coef, result) %>%
  # Need to identify missing trait-site combinations
  ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
  tidyr::pivot_wider(names_from = coef, values_from = result,
                     values_fill = "NA") %>%
  tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "result") %>%
  # Filter to only the across site model
  dplyr::filter(lter == "All")

# Glimpse it
dplyr::glimpse(mrm_results)

# Create a local folder to export figures & supplemental figures to
dir.create(path = file.path("synchrony_figure_files"), showWarnings = F)

# Define color palettes
# Site palette
site_palette <- c("CWT" = "#bd0026", "LUQ" = "orange", "HBR" = "gold", 
                  "AND" = "limegreen", "CDR" = "lightblue", "BNZ" = "#f1b6da", 
                  "SEV" = "#9d4edd")

# Significance palette
signif_palette <- c("sig" = "#2a9d8f", "NS" = "gray87", "NA" = "white")

# Define shape palette
shp_palette <- c("AND" = 22, "BNZ" = 21, "CDR" = 24, "CWT" = 23, 
                 "HBR" = 22, "LUQ" = 21, "SEV" = 24)

# Define objects to keep
keep_objects <- c("sync_df", "spp_traits", "perm_df", "mrm_results", 
                  "site_palette", "signif_palette", "shp_palette")

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
# Figure 2 - Actual / Permuted Histograms ----
## ------------------------------------------ ##

# Calculate summary statistics for the manuscript
(summary_stats <- data.frame("lter" = "All Sites",
                             "mean" = round(mean(perm_df$r.spearman, na.rm = T), digits = 2),
                             "median" = round(median(perm_df$r.spearman, na.rm = T), digits = 2),
                             "min" = round(min(perm_df$r.spearman, na.rm = T), digits = 2),
                             "max" = round(max(perm_df$r.spearman, na.rm = T), digits = 2)) )

# Calculate same metrics for each site as well
for(site in unique(perm_df$lter)){
  
  # Filter to one site
  sub_site <- dplyr::filter(perm_df, lter == site)
  
  # Calculate metrics
  site_stats <- data.frame("lter" = site,
                           "mean" = round(mean(sub_site$r.spearman, na.rm = T), digits = 2),
                           "median" = round(median(sub_site$r.spearman, na.rm = T), digits = 2),
                           "min" = round(min(sub_site$r.spearman, na.rm = T), digits = 2),
                           "max" = round(max(sub_site$r.spearman, na.rm = T), digits = 2))
  
  # Attach to extant dataframe
  summary_stats %<>%
    dplyr::bind_rows(site_stats) }

# Check that out
summary_stats

# Actual figure construction below here

# Identify average synchrony for the actual data and permuted data
avg_corr_perm <- mean(perm_df$perm_r.spearman, na.rm = T)
avg_corr_real <- mean(perm_df$r.spearman, na.rm = T)

# Make the density plot graph
ggplot(sync_df) +
  # Histogram of permuted synchrony + line at mean
  geom_vline(xintercept = avg_corr_perm, color = "gray32", trim = T,
             linetype = 2, linewidth = 1) +
  geom_density(data = perm_df, aes(x = perm_r.spearman), alpha = 0.5, 
               fill = "gray32", color = "gray32") +
  # Same for real synchrony distribution + average
  geom_vline(xintercept = avg_corr_real, color = signif_palette[1],
             linetype = 2, linewidth = 1) +
  geom_density(aes(x = r.spearman), alpha = 0.5, trim = T,
               fill = signif_palette[1], color = signif_palette[1]) +
  # Formatting / aesthetics tweaks
  labs(x = "Cross-Species Synchrony", y = "Density") +
  supportR::theme_lyon() +
  theme(legend.position = "none")

# Export locally
ggsave(filename = file.path("synchrony_figure_files", "sync_fig2_hist_perm_vs_real.png"),
       plot = last_plot(), width = 6, height = 4, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
# Figure 3 - Climate & Site ----
## ------------------------------------------ ##

# Create panel for per-site variation
fig3_sites <- ggplot(sync_df, aes(x = lter)) +
  # Actual data points
  geom_jitter(aes(x = lter, y = r.spearman, shape = lter, color = lter), 
              alpha = 0.3, width = 0.2, size = 1.3, pch = sync_df$solid_shapes) +
  # Horizontal line at 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Add permuted / non-permuted synchrony values
  see::geom_violinhalf(aes(y = r.spearman, fill = lter), 
                       flip = F, alpha = 0.8) +
  see::geom_violinhalf(data = perm_df, aes(y = perm_r.spearman), 
                       flip = T, fill = "gray32", alpha = 0.5) +
  # Flip coordinates to be vertical
  coord_flip() +
  # Customize graph aesethtics
  labs(y = "Cross-Species Synchrony", x = "LTER Site") +
  scale_color_manual(values = site_palette) +
  scale_fill_manual(values = site_palette) +
  scale_shape_manual(values = shp_palette) +
  scale_x_discrete(limits = c("CWT", "LUQ", "HBR", "AND", "CDR", "BNZ", "SEV")) +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.position = 'none'); fig3_sites

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))


# End ----
