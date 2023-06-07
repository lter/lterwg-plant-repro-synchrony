## ------------------------------------------ ##
           # Synchrony Exploration
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Create exploratory plots / data visualizations for synchrony data

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, supportR, magrittr)

# Clear environment
rm(list = ls())

# Download prepared data
## See "synchrony_data_prep.R" for how this file is created
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) %>%
  dplyr::filter(name == "synchrony_data.csv") %>%
  googledrive::drive_download(., overwrite = T)

# Read in that file
sync_df <- read.csv(file = "synchrony_data.csv") %>%
  # Make a species pair column quickly
  dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
                .before = Species1)

# Glimpse it
dplyr::glimpse(sync_df)

# Identify folder to export plots to
fig_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1TJxskD7obtZl7j8b51sbfJBMkNxEXkCN")

# Define color palette per site
site_palette <- c("SEV" = "#c51b7d", "LUQ" = "#7fbc41", "HBR" = "#d73027", "CWT" = "#4575b4",
                  "CDR" = "#e08214", "BNZ" = "#8073ac", "AND" = "#f1b6da")

## ------------------------------------------ ##
  # Correlation vs. Trait Levels Filtered ----
## ------------------------------------------ ##

# Make a simplified object including only what we'll need here
simp_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_values")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_values")) %>%
  # Count unique levels per trait
  dplyr::group_by(lter, name) %>%
  dplyr::mutate(level_ct = length(unique(value))) %>%
  dplyr::ungroup() %>%
  # Keep only traits that have more than one level
  dplyr::filter(level_ct > 1) %>%
  dplyr::select(-level_ct) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name)))

# Check that structure out
dplyr::glimpse(simp_df)

# We also want to add a facet for 'all' that includes all data points regardless of site identity
all_site_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_values")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_values")) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name))) %>%
  # Coerce all site names to be the same
  dplyr::mutate(lter = "All Sites")

dplyr::glimpse(all_site_df)

# Attach these two graphs
combo_df <- simp_df %>%
  dplyr::bind_rows(all_site_df)

# For each focal trait
for(focal_trait in unique(combo_df$name)){
  
  # Subset that dataframe
  trait_df <- combo_df %>%
    dplyr::filter(name == focal_trait) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, values_from = value)
  
  # Create plot
  ggplot(trait_df, aes(x = .data[[focal_trait]], y = r.spearman, fill = lter)) +
    geom_boxplot(outlier.shape = 23) +
    geom_jitter(pch = 21, alpha = 0.4, height = 0, width = 0.15) +
    facet_grid(. ~ lter) +
    labs(y = "Spearman Correlation") +
    scale_fill_manual(values = site_palette) +
    supportR::theme_lyon() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1),
          legend.position = "none")
  
  # Assemble file name
  plot_name <- paste("multi-site", focal_trait, "plot.png", sep = "_")
  
  # Save locally
  ggsave(filename = plot_name, plot = last_plot(), width = 8, height = 6, units = "in")
  
  # Export to Drive
  googledrive::drive_upload(media = plot_name, path = fig_folder, overwrite = T) 
  
  # Completion message
  message("Finished with plots for ", focal_trait) }

# Clean up environment a bit
rm(list = c("all_site_df", "combo_df", "simp_df", "trait_df", "focal_trait", "plot_name"))

## ------------------------------------------ ##
 # Correlation vs. Trait Levels Unfiltered ----
## ------------------------------------------ ##

# Make a simplified object including only what we'll need here
simp_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_values")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_values")) %>%
  # Count unique levels per trait
  # dplyr::group_by(lter, name) %>%
  # dplyr::mutate(level_ct = length(unique(value))) %>%
  # dplyr::ungroup() %>%
  # # Keep only traits that have more than one level
  # dplyr::filter(level_ct > 1) %>%
  # dplyr::select(-level_ct) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name)))

# Check that structure out
dplyr::glimpse(simp_df)

# We also want to add a facet for 'all' that includes all data points regardless of site identity
all_site_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_values")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_values")) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name))) %>%
  # Coerce all site names to be the same
  dplyr::mutate(lter = "All Sites")

dplyr::glimpse(all_site_df)

# Attach these two graphs
combo_df <- simp_df %>%
  dplyr::bind_rows(all_site_df)

# For each focal trait
for(focal_trait in unique(combo_df$name)){
  
  # Subset that dataframe
  trait_df <- combo_df %>%
    dplyr::filter(name == focal_trait) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, values_from = value)
  
  # Create plot
  ggplot(trait_df, aes(x = .data[[focal_trait]], y = r.spearman, fill = lter)) +
    geom_boxplot(outlier.shape = 23) +
    geom_jitter(pch = 21, alpha = 0.4, height = 0, width = 0.15) +
    facet_grid(. ~ lter) +
    labs(y = "Spearman Correlation") +
    scale_fill_manual(values = site_palette) +
    supportR::theme_lyon() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # Assemble file name
  plot_name <- paste("multi-site", focal_trait, "Unfiltered_plot.png", sep = "_")
  
  # Save locally
  ggsave(filename = plot_name, plot = last_plot(), width = 12, height = 6, units = "in")
  
  # Export to Drive
  googledrive::drive_upload(media = plot_name, path = fig_folder, overwrite = T) 
  
  # Completion message
  message("Finished with plots for ", focal_trait) }

# Clean up environment a bit
rm(list = c("all_site_df", "combo_df", "simp_df", "trait_df", "focal_trait", "plot_name"))

## ------------------------------------------ ##
      # Correlation vs. Trait Status ----
## ------------------------------------------ ##

# Make a simplified object including only what we'll need here
simp_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_shared")) %>%
  # # Count unique levels per trait
  # dplyr::group_by(lter, name) %>%
  # dplyr::mutate(level_ct = length(unique(value))) %>%
  # dplyr::ungroup() %>%
  # # Keep only traits that have more than one level
  # dplyr::filter(level_ct > 1) %>%
  # dplyr::select(-level_ct) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name)))

# Check that structure out
dplyr::glimpse(simp_df)

# We also want to add a facet for 'all' that includes all data points regardless of site identity
all_site_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_shared")) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name))) %>%
  # Coerce all site names to be the same
  dplyr::mutate(lter = "All Sites")

dplyr::glimpse(all_site_df)

# Attach these two graphs
combo_df <- simp_df %>%
  dplyr::bind_rows(all_site_df)

# For each focal trait
for(focal_trait in unique(combo_df$name)){
  
  # Subset that dataframe
  trait_df <- combo_df %>%
    dplyr::filter(name == focal_trait) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, values_from = value)
  
  # Create plot
  ggplot(trait_df, aes(x = as.factor(.data[[focal_trait]]), y = r.spearman, fill = lter)) +
    geom_boxplot(outlier.shape = 23) +
    geom_jitter(pch = 21, alpha = 0.4, height = 0, width = 0.15) +
    facet_grid(. ~ lter) +
    labs(y = "Spearman Correlation", x = focal_trait) +
    scale_fill_manual(values = site_palette) +
    supportR::theme_lyon() +
    theme(legend.position = "none")
  
  # Assemble file name
  plot_name <- paste("multi-site", focal_trait, "plot.png", sep = "_")
  
  # Save locally
  ggsave(filename = plot_name, plot = last_plot(), width = 7, height = 6, units = "in")
  
  # Export to Drive
  googledrive::drive_upload(media = plot_name, path = fig_folder, overwrite = T) 
  
  # Completion message
  message("Finished with plots for ", focal_trait) }

# Clean up environment a bit
rm(list = c("all_site_df", "combo_df", "simp_df", "trait_df", "focal_trait", "plot_name"))

## ------------------------------------------ ##
# Correlation vs. Trait Status Excluded Sites ----
## ------------------------------------------ ##

# Make a simplified object including only what we'll need here
simp_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_shared")) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name)))

# Check that structure out
dplyr::glimpse(simp_df)

# Make an empty list
exclude_list <- list()

# Loop across sites to exclude each site in its own dataframe
for(exclude_site in unique(simp_df$lter)){
  
  # Filter out that site
  sub_df <- simp_df %>%
    dplyr::filter(lter != exclude_site) %>%
    # And coerce lter column contents to reflect this
    dplyr::mutate(lter = paste0("Exclude ", exclude_site))
  
  # Add to list
  exclude_list[[exclude_site]] <- sub_df }

# Wrangle plotting dataframe
exclude_df <- simp_df %>%
  # Unlist list and bind on
  dplyr::bind_rows(purrr::list_rbind(exclude_list)) %>%
  # Fix original site names too
  dplyr::mutate(lter = ifelse(lter %in% c("SEV", "LUQ", "HBR", "CWT", "CDR", "BNZ", "AND"),
                              yes = "All", no = lter))

# For each focal trait
for(focal_trait in unique(exclude_df$name)){
  
  # Subset that dataframe
  trait_df <- exclude_df %>%
    dplyr::filter(name == focal_trait)
  
  # Create plot
  ggplot(trait_df, aes(x = as.factor(value), y = r.spearman, fill = lter)) +
    geom_boxplot(outlier.shape = 23) +
    geom_jitter(pch = 21, alpha = 0.4, height = 0, width = 0.15) +
    facet_grid(. ~ lter) +
    labs(y = "Spearman Correlation", x = focal_trait) +
    supportR::theme_lyon() +
    theme(legend.position = "none")
  
  # Assemble file name
  plot_name <- paste("multi-site", focal_trait, "site-exclusions_plot.png", sep = "_")
  
  # Save locally
  ggsave(filename = plot_name, plot = last_plot(), width = 7, height = 6, units = "in")
  
  # Export to Drive
  googledrive::drive_upload(media = plot_name, path = fig_folder, overwrite = T) 
  
  # Completion message
  message("Finished with plots for ", focal_trait) }

# Clean up environment a bit
rm(list = c("exclude_df", "exclude_list", "simp_df", "sub_df", "trait_df", 
            "exclude_site", "focal_trait", "plot_name"))

## ------------------------------------------ ##
  # Correlation vs. Trait Status w/ Sig. ----
## ------------------------------------------ ##
# Make a simplified object including only what we'll need here
simp_df <- sync_df %>%
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_shared")) %>%
  # Tidy trait names for use as axis labels
  dplyr::mutate(name = stringr::str_to_title(gsub(pattern = "_", replacement = " ", x = name))) %>%
  # Create column for whether each trait was sig in saturated model
  dplyr::mutate(sig = ifelse(name %in% c("Pollinator Code Shared", "Mycorrhiza Shared", "Needleleaf Broadleaf Shared", "Deciduous Evergreen Shared", "Dispersal Syndrome Shared", "Shade Tolerance Shared", "Fleshy Fruit Shared"),
                                      yes = "sig",
                                      no = "NS")) %>%
  # Make combo significance and status column
  dplyr::mutate(sig_status = ifelse(sig == "sig",
                                    yes = paste0(sig, "-", value),
                                    no = sig))

# Check that structure out
dplyr::glimpse(simp_df)

# Make palette
sig_palette = c("sig-1" = "#b35806", "sig-0" = "#542788", "NS" = "gray")

# Identify color palette
ggplot(simp_df, aes(x = as.factor(value), y = r.spearman, fill = sig_status)) +
  geom_boxplot(outlier.shape = 23) +
  geom_jitter(pch = 21, alpha = 0.4, height = 0, width = 0.15) +
  facet_wrap(. ~ name) +
  labs(y = "Spearman Correlation") +
  scale_fill_manual(values = sig_palette) +
  supportR::theme_lyon() +
  theme(legend.position = "right")

# Save locally
ggsave(filename = "saturated_model_all_data_w_significance.png",
       plot = last_plot(), width = 10, height = 10, units = "in")

# Export to Drive
googledrive::drive_upload(media = "saturated_model_all_data_w_significance.png", 
                          path = fig_folder, overwrite = T) 

## ------------------------------------------ ##
              # Phylo Distance ----
## ------------------------------------------ ##

# Explore per-site phylogenetic distance vs. Spearman/Pearson relationships

# Pare down to needed columns
phy_simp <- sync_df %>%
  dplyr::select(lter, Species_Pair, r.spearman, r.pearson, Phylo_distance) %>%
  # Make sure there are no duplicate rows
  dplyr::distinct()

# Make plot(s)
ggplot(phy_simp, aes(x = Phylo_distance, y = r.spearman, fill = lter)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(pch = 23, size = 4) +
  facet_wrap(. ~ lter) +
  labs(x = "Phylogenetic Distance", y = "Spearman Correlation") +
  # Coarsely handle plot theming
  theme_bw()

# Export plot
ggsave(filename = "spearman_vs_phylodist.png", height = 8, width = 10, units = "in")
  
# Do the same for pearson distance
ggplot(phy_simp, aes(x = Phylo_distance, y = r.pearson, fill = lter)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(pch = 23, size = 4) +
  facet_wrap(. ~ lter) +
  labs(x = "Phylogenetic Distance", y = "Pearson Correlation") +
  # Coarsely handle plot theming
  theme_bw()

# Export plot
ggsave(filename = "pearson_vs_phylodist.png", height = 8, width = 10, units = "in")



# End ----
