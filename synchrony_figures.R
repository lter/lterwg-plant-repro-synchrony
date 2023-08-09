## ------------------------------------------ ##
        # Synchrony Figure Preparation
## ------------------------------------------ ##
# Written by: Nick J Lyon, Jalene LaMontagne, Angel Chen

# PURPOSE
## Create publication-quality figures for the synchrony paper

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, see, vegan, supportR, cowplot, magrittr)

# Clear environment
rm(list = ls())

# Identify names of files this script requires
sync_file <- "synchrony_pcoa_climate_combination.csv" # synchrony + climate data
trait_file <- "pre_ordination_trait_data.csv" # trait data
perm_file <- "permutation_corr_unsummarized.csv" # correlation permutation data
mrm_file <- "MRM_not_averaged_results_2023-06-14_10000perm.csv" # MRM results
time_series_files <- c("series_andrews.csv", "series_bonanza.csv") # AND + BNZ time series info

# Identify links of relevant Drive folders
gen_data_folder <- googledrive::as_id("https://drive.google.com/drive/folders/1aPdQBNlrmyWKtVkcCzY0jBGnYNHnwpeE")
sync_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")
stats_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")

# Identify relevant data from those folders
## List out all CSVs in all folders
(wanted_files <- googledrive::drive_ls(path = sync_folder, type = "csv") %>%
    dplyr::bind_rows(googledrive::drive_ls(path = gen_data_folder, type = "csv")) %>%
    dplyr::bind_rows(googledrive::drive_ls(path = stats_folder, type = "csv")) %>%
    ## Filter to only desired files
    dplyr::filter(name %in% c(sync_file, trait_file, perm_file, mrm_file, time_series_files)))

# Create folder to download files into
dir.create(path = file.path("figure_data"), showWarnings = F)

# Download files into that folder
purrr::walk2(.x = wanted_files$id, .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("figure_data", .y),
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
       # Figure 1A/B - Time Series ----
## ------------------------------------------ ##

# Read in data
and_df <- read.csv(file.path("figure_data", "series_andrews.csv"))
bnz_df <- read.csv(file.path("figure_data", "series_bonanza.csv"))

# Wrangle Andrews data
and_v2 <- and_df %>%
  # Filter to a specific supersite
  dplyr::filter(supersite == "Mosquito Lakes") %>%
  # Filter to a certain timerange too
  dplyr::filter(Year >= 1985 & Year <= 2006) %>%
  # Summarize within species
  dplyr::group_by(Species.Name, supersite) %>%
  dplyr::mutate(min = min(total.seedsorcones),
                max = max(total.seedsorcones),
                standardized = ((total.seedsorcones - min)/(max - min)) * 100) %>%
  dplyr::ungroup()

# Check it out
dplyr::glimpse(and_df)

# Wrangle Bonanza in the same way
bnz_v2 <- bnz_df %>%
  # Filter to one site
  dplyr::filter(supersite == "FP5A") %>%
  # Filter to same years
  dplyr::filter(Year >= 1985 & Year <= 2006) %>%
  # Calculate standardized values
  dplyr::group_by(Species.Name, supersite) %>%
  dplyr::mutate(min = min(total.seeds),
                max = max(total.seeds),
                standardized = ((total.seeds - min)/(max - min)) * 100) %>%
  dplyr::ungroup()

# Glimpse this as well
dplyr::glimpse(bnz_v2)

# Define species color palettes for both LTERs
spp_palette <- c("Abies.amabilis" = "#238b45", "Abies.lasiocarpa" = "#a8ddb5", 
                 "Abies.procera" = "#2b8cbe", "Tsuga.mertensiana" = "#225ea8", 
                 "Alnus.incana" = "#8c2d04", "Picea.glauca" = "#f768a1", 
                 "Alnus.viridis" = "#ec7014", "Betula.neoalaskana" = "#fec44f", 
                 "Picea.mariana" = "#ae017e", "Larix.laricina" = "#d7301f")

# Create the time series for Andrews Forest
fig1_and <- ggplot(and_v2, aes(x = Year, y = standardized, color = Species.Name)) +
  geom_path(lwd = 1.25) +
  # Tweak graph aesthetics
  labs(x = "Year", y = "Standardized Reproduction") +
  scale_color_manual(values = spp_palette) +
  supportR::theme_lyon(title_size = 16, text_size = 13) +
  theme(legend.position = "right"); fig1_and

# Do the same for Bonanza for one supersite
fig1_bnz <- ggplot(bnz_v2, aes(x = Year, y = standardized, color = Species.Name)) +
  geom_path(lwd = 1.25) +
  # Tweak graph aesthetics
  labs(x = "Year", y = "Standardized Reproduction") +
  scale_color_manual(values = spp_palette) +
  supportR::theme_lyon(title_size = 16, text_size = 13) +
  theme(legend.position = "right"); fig1_bnz

# Assemble A & B into column
cowplot::plot_grid(fig1_bnz, fig1_and, labels = "AUTO", nrow = 2)

# Export locally as both PNG & EPS (raster vs. vector files)
ggsave(filename = file.path("synchrony_figure_files", "sync_fig1AB_time_series.png"),
       plot = last_plot(), width = 7, height = 8, units = "in", dpi = 720)
ggsave(filename = file.path("synchrony_figure_files", "sync_fig1AB_time_series.eps"),
       plot = last_plot(), width = 7, height = 8, units = "in", dpi = 720)

# Generate version without legend
cowplot::plot_grid(fig1_bnz + theme(legend.position = "none"), 
                   fig1_and + theme(legend.position = "none"), 
                   labels = "AUTO", nrow = 2)

# Export locally again
ggsave(filename = file.path("synchrony_figure_files", "sync_fig1AB_time_series_noleg.png"),
       plot = last_plot(), width = 7, height = 8, units = "in", dpi = 720)
ggsave(filename = file.path("synchrony_figure_files", "sync_fig1AB_time_series_noleg.eps"),
       plot = last_plot(), width = 7, height = 8, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
          # Figure 1C - Site Map ----
## ------------------------------------------ ##

# See "synchrony_map.R" for the creation of this figure panel

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
  geom_vline(xintercept = avg_corr_perm, color = "gray32",
             linetype = 2, linewidth = 1) +
  geom_density(data = perm_df, aes(x = perm_r.spearman), alpha = 0.5, 
               fill = "gray32", color = "gray32") +
  # Same for real synchrony distribution + average
  geom_vline(xintercept = avg_corr_real, color = signif_palette[1],
             linetype = 2, linewidth = 1) +
  geom_density(aes(x = r.spearman), alpha = 0.5, 
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
# 4a = Synchrony ~ log water deficit
# 4b = site ~ synchrony

# Make a summarized dataframe for figure 4A
fig3_cwd_df <- supportR::summary_table(data = sync_df, groups = c("lter", "CWD_log"),
                                    response = "r.spearman", drop_na = T)

# Create climate panel
fig3_cwd <- ggplot(sync_df, aes(x = CWD_log, y = r.spearman)) +
  # Horizontal line at 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Add un-averaged points
  geom_smooth(color = "black", fill = "gray82", method = "lm", formula = "y ~ x") +
  geom_point(aes(color = lter), alpha = 0.3, pch = sync_df$solid_shapes) +
  # Add averaged points with SD bars
  geom_errorbar(data = fig3_cwd_df, aes(x = CWD_log, y = mean, 
                                     ymax = mean + std_dev, 
                                     ymin = mean - std_dev), width = 0) +
  geom_point(data = fig3_cwd_df, aes(x = CWD_log, y = mean, fill = lter, 
                                  shape = lter), size = 3) +
  scale_shape_manual(values = shp_palette) +
  # Customize colors, fills, and plot formatting
  labs(x = "log(Climate Water Deficit [mm])", y = "Cross-Species Synchrony") +
  ylim(-0.75, 1.1) +
  scale_color_manual(values = site_palette) +
  scale_fill_manual(values = site_palette) +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.background = element_blank(),
        legend.position = "right"); fig3_cwd

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

# Assemble the figure
cowplot::plot_grid(fig3_sites, fig3_cwd, labels = "AUTO", nrow = 1, ncol = 2, 
                   rel_widths = c(0.8, 1.2))

# Export this
ggsave(filename = file.path("synchrony_figure_files", "sync_fig3_climate_plus_site_var.png"),
       plot = last_plot(), width = 8, height = 4, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
        # Figure 4A - Trait Space ----
## ------------------------------------------ ##

# Get dissimilarity metric
trait_mds <- vegan::metaMDS(comm = spp_traits[-c(1:2)], distance = "jaccard",
                            k = 2, try = 100)

# Identify which traits are driving the variation
trait_fit <- vegan::envfit(ord = trait_mds, env = spp_traits[-c(1:2)], permutations = 999)

# Make (and export) ordination
png(file = file.path("synchrony_figure_files", "sync_fig4A_trait_nms.png"), 
     width = 720, height = 720)
supportR::nms_ord(mod = trait_mds, groupcol = spp_traits$lter, leg_pos = "bottomleft",
                  colors = site_palette[sort(names(site_palette))],
                  pt_size = 2.5, pt_alpha = 0.5)
graphics::plot(x = trait_fit, col = "black", cex = 1.0)
dev.off()

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
    # Figure 4B - Trait Similarity ----
## ------------------------------------------ ##

# Make graph
ggplot(sync_df, aes(x = TraitSimilarityJaccardVariant, y = r.spearman)) +
  # Horizontal line at 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Actual data points
  geom_point(aes(color = lter), alpha = 0.4, pch = sync_df$solid_shapes) +
  ## Semi-duplicate geom_smooths / LTER are necessary to make legend pretty
  geom_smooth(aes(color = lter, fill = lter), method = "lm", formula = "y ~ x", 
              alpha = 0.2, show.legend = F) +
  geom_smooth(aes(color = lter), se = F, method = "lm", formula = "y ~ x") +
  geom_smooth(color = "black", fill = "gray82", method = "lm", formula = "y ~ x") +
  # Customize colors, fills, and plot formatting
  labs(x = "Trait Similarity", y = "Cross-Species Synchrony") +
  scale_color_manual(values = site_palette) +
  scale_fill_manual(values = site_palette) +
  scale_shape_manual(values = shp_palette) +
  guides(fill = 'none') +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.background = element_blank(),
        legend.key = element_rect(color = 'white'),
        legend.position = "right")

# Export locally
ggsave(filename = file.path("synchrony_figure_files", "sync_fig4B_trait_sim_by_site.png"),
       plot = last_plot(), width = 6, height = 5, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
    # Figure 5 - MRMs of Trait 'Status' ----
## ------------------------------------------ ##
# 'Status' = 0 for unshared vs. 1 for shared where trait value identity doesn't matter

# A = Synchrony ~ trait status (faceted by trait)
# B = Synchrony ~ *log* seed mass similarity
# C = Synchrony ~ phylogenetic similarity

# Make a separate dataframe for this part of this figure
fig5a_df <- sync_df %>%
  # Pare down to needed columns
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_shared"),
                      names_to = "trait", values_to = "value") %>%
  # Force the lter column to be "All"
  dplyr::mutate(lter = "All") %>%
  # Assign significance (identified by 'synchrony_mrm.R')
  dplyr::left_join(mrm_results, by = c("lter", "trait")) %>%
  # Tidy trait names for use as axis labels & make the value a factor
  dplyr::mutate(trait = stringr::str_to_title(gsub(pattern = "_", replacement = " ", 
                                                   x = trait)),
                value = as.factor(value)) %>%
  # Also drop "shared" from trait names
  dplyr::mutate(trait = gsub(pattern = " Shared", replacement = "", x = trait))

# Check that out
glimpse(fig5a_df)

# Get a summarized dataframe as well
fig5a_avgdf <- supportR::summary_table(data = fig5a_df, response = "r.spearman",
                                       groups = c("trait", "result", "value"))

# Make figure 5A
fig5a <- ggplot(fig5a_df, aes(x = value, y = r.spearman)) +
  # Add horizontal line at synchrony = 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Actual plotting content
  geom_jitter(aes(color = result), width = 0.15, alpha = 0.25) +
  geom_violin(aes(fill = result), alpha = 0.1) +
  facet_wrap(. ~ trait, ncol = 5) +
  # Add averaged points with SD bars
  geom_errorbar(data = fig5a_avgdf, aes(x = value, y = mean, 
                                     ymax = mean + std_dev, 
                                     ymin = mean - std_dev), width = 0) +
  geom_point(data = fig5a_avgdf, aes(x = value, y = mean, fill = result), 
             shape = 21, size = 3) +
  # Customize formatting / aesthetics
  labs(x = "Trait Status (Unshared vs. Shared)", y = "Cross-Species Synchrony") +
  scale_fill_manual(values = signif_palette) +
  scale_color_manual(values = signif_palette) +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 11)); fig5a

# Check significance of phylogeny + seed mass
phylo_sig <- mrm_results %>%
  dplyr::filter(lter == "All" & trait == "Phylogenetic_similarity") %>%
  dplyr::pull(result)
seed_sig <- mrm_results %>%
  dplyr::filter(lter == "All" & trait == "Seed_mass_similarity") %>%
  dplyr::pull(result)

# Create dataframe for 5B & 5C
fig5bc_df <- sync_df %>%
  # Pare down to needed columns
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, 
                Phylogenetic_similarity, Seed_mass_similarity) %>%
  # Add on the stats results
  dplyr::mutate(phylo_result = phylo_sig,
                seed_result = seed_sig)

# Check out that dataframe
dplyr::glimpse(fig5bc_df)

# Make figure 5B
fig5b <- ggplot(fig5bc_df, aes(x = Phylogenetic_similarity, y = r.spearman)) +
  # Add horizontal line at synchrony = 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Actual plotting content
  geom_point(aes(color = phylo_result), alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x", color = 'black') +
  labs(x = "Phylogenetic Similarity", y = "Cross-Species Synchrony") +
  scale_color_manual(values = signif_palette) +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 11)); fig5b

# Make figure 5C too
fig5c <- ggplot(fig5bc_df, aes(x = Seed_mass_similarity, y = r.spearman)) +
  # Add horizontal line at synchrony = 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Actual plotting content
  geom_point(aes(color = seed_result), alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x", color = 'black') +
  labs(x = "log(Seed Mass) Similarity", y = "Cross-Species Synchrony") +
  scale_color_manual(values = signif_palette) +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 11)); fig5c

# Add B & C into one object
(fig5bc <- cowplot::plot_grid(fig5b, fig5c, nrow = 1, labels = c("B", "C")))

# Assemble figure
cowplot::plot_grid(fig5a, fig5bc, labels = c("A", ""), nrow = 2, ncol = 1,
                   rel_heights = c(1.5, 0.8))

# Export it!
ggsave(filename = file.path("synchrony_figure_files", "sync_fig5_MRM_results.png"),
       plot = last_plot(), width = 10, height = 10, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
         # Export Figures to Drive ----
## ------------------------------------------ ##

# Identify folder to export plots to
figure_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1wZqCP-axj9KUfAaiPJsTamc03zsgngCY")

# Identify figures that we have created
figures <- dir(path = file.path("synchrony_figure_files"))

# Upload each to the Drive (skipping the map if it's there)
for(file in setdiff(figures, c("sync_fig1C_map.png"))){
  googledrive::drive_upload(media = file.path("synchrony_figure_files", file),
                            path = figure_folder, overwrite = T) }

# End ----
