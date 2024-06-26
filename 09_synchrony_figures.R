## ------------------------------------------ ##
        # Synchrony Figure Preparation
## ------------------------------------------ ##
# Written by: Nick J Lyon, Jalene LaMontagne, Angel Chen

# PURPOSE
## Create publication-quality figures for the synchrony paper

# PRE-REQUISITES
## The following script(s) must be run--in order--for this script to work as intended
## 1. Run `synchrony_vis-prep.R`

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, see, vegan, njlyon0/supportR, cowplot, magrittr)

# Create needed local folder(s)
dir.create(path = file.path("synchrony_figure_files"), showWarnings = F)

# Run the entirety of the preparation script (if not already done so)
## Takes ~2 minutes to complete
source("08_synchrony_vis-prep.R")

# Clear environment
rm(list = ls())

# Read in 'core' synchrony data
sync_df <- read.csv(file = file.path("figure_data", "synchrony_viz-ready.csv"))

# Read in species-specific trait information
spp_traits <- read.csv(file = file.path("figure_data", "traits_viz-ready.csv"))

# Read in permuted vs. observed correlations
perm_df <- read.csv(file = file.path("figure_data", "perm_viz-ready.csv"))

# Read in MRM results
mrm_results <- read.csv(file = file.path("figure_data", "mrm_viz-ready.csv")) %>% 
  # Filter to only the across site model
  dplyr::filter(lter == "All")

# Read in ANOVA on trait 'status' (i.e., shared vs. unshared)
stat_aov <- read.csv(file = file.path("figure_data", "aov-status_viz-ready.csv"))

# Read in main results of ANOVA on trait 'levels' (i.e., actual trait values)
aov_results <- read.csv(file = file.path("figure_data", "aov-levels_viz-ready.csv"))

# Read in pairwise results of levels ANOVA
aov_pairs <- read.csv(file = file.path("figure_data", "aov-levels-pairs_viz-ready.csv"))

# Read in compact letter display for pairwise comparisons of levels ANOVA
aov_cld <- read.csv(file = file.path("figure_data", "aov-levels-cld_viz-ready.csv"))

## ------------------------------------------ ##
    # Graph Aesthetic Standardization ----
## ------------------------------------------ ##

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
                  "stat_aov", "aov_results", "aov_cld", "aov_pairs",
                  "site_palette", "signif_palette", "shp_palette")

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
       # Figure 1A/B - Time Series ----
## ------------------------------------------ ##

# Check the mean synchrony at our two desired plots
sync_df %>% 
  # Subset to just these plots
  dplyr::filter((lter == "AND" & Plot.ID == "Mosquito Lakes") |
                  (lter == "BNZ" & Plot.ID == "FP5A")) %>% 
  # Calculate min/max synchrony
  dplyr::group_by(lter, Plot.ID) %>% 
  dplyr::summarize(min_corr = min(r.spearman, na.rm = T),
                   max_corr = max(r.spearman, na.rm = T))

# Read them in
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
  # Set x-axis limits
  xlim(1989, 2006) +
  # Tweak graph aesthetics
  labs(x = "Year", y = "Standardized Reproduction") +
  scale_color_manual(values = spp_palette) +
  supportR::theme_lyon(title_size = 16, text_size = 13) +
  theme(legend.position = "right"); fig1_and

# Do the same for Bonanza for one supersite
fig1_bnz <- ggplot(bnz_v2, aes(x = Year, y = standardized, color = Species.Name)) +
  geom_path(lwd = 1.25) +
  # Set x-axis limits
  xlim(1989, 2006) +
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
# 3a = site ~ synchrony
# 3b = Synchrony ~ log water deficit

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
  # Figure 3 Variant - Climate PREDICTED ----
## ------------------------------------------ ##
# 3b = Synchrony ~ log water deficit

# Load needed libraries
librarian::shelf(lmerTest, ggeffects)

# Fit model
cwd_mod <- lmerTest::lmer(r.spearman ~ (AET) + (CWD_log) + (1|climatesite) + (1|speciespair), data = sync_df)

# Get predicted values for logCWD
mod_predicts <- ggeffects::ggpredict(model = cwd_mod, terms = c("CWD_log"),
                                     type = "random")

# Make a summarized dataframe of the predicted values
fig3_cwd_df <- supportR::summary_table(data = sync_df, groups = c("lter", "CWD_log"),
                                       response = "r.spearman", drop_na = T)

# Create climate panel
fig3_cwd <- ggplot() +
  # Horizontal line at 0
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  # Add predicted and observed best-fit lines
  geom_smooth(data = mod_predicts, mapping = aes(x = x, y = predicted),
              color = "#9a8c98", fill = "gray82", linetype = 2,
              method = "lm", formula = "y ~ x") +
  geom_smooth(data = sync_df, mapping = aes(x = CWD_log, y = r.spearman),
              color = "black", fill = "gray82", linetype = 1,
              method = "lm", formula = "y ~ x") +
  # Add un-averaged points
  geom_point(data = sync_df, mapping = aes(x = CWD_log, y = r.spearman, color = lter),
             alpha = 0.3, pch = sync_df$solid_shapes) +
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

# Export this
ggsave(filename = file.path("synchrony_figure_files", "sync_fig3-var_climate_pred_vs_obs.png"),
       plot = last_plot(), width = 6, height = 6, units = "in", dpi = 720)

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

# Give warning if not using right version of `supportR`
if(packageVersion("supportR") < '1.3.0.900'){
  warning("Label text size arguments only supported in version 1.3.0.900 or later. \nRun the following code to update your version:")
  print("devtools::install_github('njlyon0/supportR', force = T)")
  message("Note you may need to re-start your R session for the change to take effect") }

# Make (and export) ordination
png(file = file.path("synchrony_figure_files", "sync_fig4A_trait_nms.png"), 
     width = 800, height = 600)
supportR::nms_ord(mod = trait_mds, groupcol = spp_traits$lter, leg_pos = "bottomleft",
                  colors = site_palette[sort(names(site_palette))],
                  pt_size = 2.5, pt_alpha = 0.5,
                  lab_text_size = 1.5, axis_text_size = 1.25)
graphics::plot(x = trait_fit, col = "black", cex = 1.2)
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
  geom_point(aes(color = lter, shape = lter), alpha = 0.4, pch = sync_df$solid_shapes) +
  ## Semi-duplicate geom_smooths / LTER are necessary to make legend pretty
  # geom_smooth(aes(color = lter, fill = lter), method = "lm", formula = "y ~ x", 
  #             alpha = 0.2, show.legend = F) +
  # geom_smooth(aes(color = lter), se = F, method = "lm", formula = "y ~ x") +
  geom_smooth(color = "black", fill = "gray82", method = "lm", formula = "y ~ x") +
  # Customize colors, fills, and plot formatting
  labs(x = "Trait Similarity", y = "Cross-Species Synchrony") +
  scale_color_manual(values = site_palette) +
  scale_fill_manual(values = site_palette) +
  # scale_shape_manual(values = shp_palette) +
  guides(fill = 'none') +
  supportR::theme_lyon(title_size = 14, text_size = 11) +
  theme(legend.background = element_blank(),
        legend.key = element_rect(color = 'white'),
        legend.position = "right")

# Export locally
ggsave(filename = file.path("synchrony_figure_files", "sync_fig4B_trait_sim.png"),
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
  dplyr::mutate(trait = gsub(pattern = " Shared", replacement = "", x = trait)) %>% 
  # Do any desired manual tweaks of the trait names
  dplyr::mutate(trait = dplyr::case_when(
    trait == "Mycorrhiza" ~ "Mycorrhizal Assoc.",
    trait == "Deciduous Evergreen" ~ "Leaf Longevity",
    T ~ trait))

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
    # Figure 6 - ANOVAs of Trait 'Levels' ----
## ------------------------------------------ ##

# Make a dataframe for this figure
fig6_df <- sync_df %>%
  # Pare down to needed columns
  dplyr::select(lter, Species_Pair, r.spearman, dplyr::ends_with("_values")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_values"),
                      names_to = "trait", values_to = "trait_levels") %>%
  # Coerce lter to all
  dplyr::mutate(lter = "All") %>%
  # Attach ANOVA results
  dplyr::left_join(aov_results, by = c("lter", "trait")) %>%
  # Simplify trait entries
  dplyr::mutate(trait = tolower(gsub(pattern = "_", replacement = " ", x = trait))) %>%
  # Handle one synonymization issue
  dplyr::mutate(trait = ifelse(trait == "pollinator code values", 
                               yes = "pollinator values", no = trait)) %>%
  # Attach compact letter display content
  dplyr::left_join(aov_cld, by = c("lter", "trait", "trait_levels")) %>%
  # Now drop CLDs if the global test was non-significant
  dplyr::mutate(cld_letters = ifelse(result %in% c("NS", "NA"),
                                     yes = NA, no = letter)) %>%
  # Drop original CLD letter column
  dplyr::select(-letter) %>%
  # Do any desired manual tweaks of the trait names
  dplyr::mutate(trait = dplyr::case_when(
    trait == "mycorrhiza values" ~ "mycorrhizal assoc. values",
    trait == "deciduous evergreen values" ~ "leaf longevity values",
    T ~ trait)) %>% 
  # Tweak trait formatting to make cleaner facet labels
  dplyr::mutate(trait_label = factor(stringr::str_to_title(trait),
                                     levels = sort(unique(stringr::str_to_title(trait)))), 
                .before = trait) %>%
  # Tidy up / simplify trait levels to make x-axis tick marks as simple as possible
  dplyr::mutate(trait_levels = dplyr::case_when(
    ## Dispersal syndrome
    trait_levels == "abiotic-endozoochory" ~ "abio.-endozo.",
    trait_levels == "abiotic-abiotic" ~ "abiotic",
    trait_levels == "abiotic-synzoochory" ~ "abio.-synzo.",
    trait_levels == "endozoochory-endozoochory" ~ "endozo.",
    trait_levels == "endozoochory-synzoochory" ~ "endozo.-synzo.",
    trait_levels == "synzoochory-synzoochory" ~ "synzo.",
    ## Mycorrhiza
    trait_levels == "am-am" ~ "AM",
    trait_levels == "am-em" ~ "AM-EM",
    trait_levels == "am-ericoid" ~ "AM-ericoid",
    trait_levels == "am-none" ~ "AM-none",
    trait_levels == "em-em" ~ "EM",
    trait_levels == "em-ericoid" ~ "EM-ericoid",
    trait_levels == "ericoid-ericoid" ~ "ericoid",
    ## Pollinators
    trait_levels == "animal-animal" ~ "animal",
    trait_levels == "wind-wind" ~ "wind",
    # trait_levels == "animal-wind" ~ "",
    ## Deciduous vs. evergreen
    trait_levels == "deciduous-deciduous" ~ "deciduous",
    trait_levels == "deciduous-evergreen" ~ "decid.-everg.",
    trait_levels == "evergreen-evergreen" ~ "evergreen",
    ## Sexual system
    trait_levels == "dioecious-dioecious" ~ "dioec.",
    trait_levels == "dioecious-hermaphrodite" ~ "dioec.-hermaph.",
    trait_levels == "dioecious-monoecious" ~ "dioec.-monoec.",
    trait_levels == "dioecious-polygamo-dioecious" ~ "dioec.-polyg. dioec.",
    trait_levels == "hermaphrodite-hermaphrodite" ~ "hermaph.",
    trait_levels == "hermaphrodite-monoecious" ~ "hermaph.-monoec.",
    trait_levels == "hermaphrodite-polygamo-dioecious" ~ "hermaph.-polyg. dioec.",
    trait_levels == "monoecious-monoecious" ~ "monoec.",
    trait_levels == "monoecious-polygamo-dioecious" ~ "monoec.-polyg. dioec.",
    trait_levels == "polygamo-dioecious-polygamo-dioecious" ~ "polygamo dioec.",
    ## Shade tolerance
    trait_levels == "intermediate-intermediate" ~ "mid.",
    trait_levels == "intermediate-intolerant" ~ "mid.-intoler.",
    trait_levels == "intermediate-tolerant" ~ "mid.-toler.",
    trait_levels == "intolerant-intolerant" ~ "intolerant",
    trait_levels == "intolerant-tolerant" ~ "intoler.-toler.",
    trait_levels == "tolerant-tolerant" ~ "tolerant",
    ## Growth form
    trait_levels == "liana-liana" ~ "liana",
    # trait_levels == "liana-shrub" ~ "",
    # trait_levels == "liana-tree" ~ "",
    trait_levels == "shrub-shrub" ~ "shrub",
    # trait_levels == "shrub-tree" ~ "",
    trait_levels == "tree-tree" ~ "tree",
    ## Seed development
    trait_levels == "1-1" ~ "1",
    trait_levels == "2-2" ~ "2",
    trait_levels == "3-3" ~ "3",
    ## Seed bank
    trait_levels == "no-no" ~ "no",
    # trait_levels == "no-yes" ~ "",
    trait_levels == "yes-yes" ~ "yes",
    # If isn't "fixed" above, keep as-is
    TRUE ~ trait_levels)) %>% 
  # Filter to only desired traits
  dplyr::filter(trait %in% c("pollinator values", "leaf longevity values"))

# Check it out
dplyr::glimpse(fig6_df)

# Specify order (left to right) of trait levels across all traits
level_vec <- c(
  ## Pollinators
  "animal", "wind", "animal-wind",
  ## Seed development
  "1", "2", "3", "2-3",
  ## Mycorrhiza
  "AM", "EM", "ericoid", "AM-EM", "AM-ericoid", "AM-none", "EM-ericoid",
  ## Deciduous vs. Evergreen
  "deciduous", "evergreen", "decid.-everg.",
  ## Dispersal
  "abiotic", "endozo.", "synzo.", "abio.-endozo.", "abio.-synzo.", "endozo.-synzo.",
  ## Sexual system
  "dioec.", "monoec.", "hermaph.", "polygamo dioec.", "dioec.-hermaph.", 
  "dioec.-monoec.", "dioec.-polyg. dioec.", "hermaph.-monoec.", 
  "hermaph.-polyg. dioec.", "monoec.-polyg. dioec.",
  ## Shade tolerance
  "intolerant", "mid.", "tolerant", "intoler.-toler.", "mid.-intoler.", "mid.-toler.",
  ## Growth form
  "liana", "shrub", "tree", "liana-shrub", "liana-tree", "shrub-tree",
  ## Fleshy fruit / seed bank
  "yes", "no", "no-yes", "yes-no")

# Make an empty list for storing per-trait graphs
fig6_plotlist <- list()

# Loop across traits
for(aov_trait in sort(unique(fig6_df$trait_label))){
  # for(aov_trait in "Pollinator Values") {
  
  # Subset data to only that trait
  fig6_subdf <- fig6_df %>%
    dplyr::filter(trait_label == aov_trait)
  
  # Get an averaged dataframe too
  fig6_avgsubdf <- supportR::summary_table(data = fig6_subdf, response = "r.spearman",
                                           groups = c("trait_label", "result", "trait_levels"))
  
  # Make another object that is *only* the compact letter display content
  fig6_subclddf <- fig6_subdf %>%
    dplyr::select(trait_levels, cld_letters) %>%
    dplyr::distinct()
  
  # Make this a named vector
  fig6_subcld <- fig6_subclddf$cld_letters
  names(fig6_subcld) <- fig6_subclddf$trait_levels
  
  # Identify trait levels for this trait  
  sub_level_vec <- level_vec[level_vec %in% unique(fig6_subdf$trait_levels)]
  
  # Make plot
  fig6_subplot <- ggplot(fig6_subdf, aes(x = trait_levels, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_jitter(aes(color = result), width = 0.15, alpha = 0.25) +
    geom_violin(aes(fill = result), alpha = 0.1) +
    facet_wrap(. ~ trait_label, ncol = 5) +
    # Add averaged points with SD bars
    geom_errorbar(data = fig6_avgsubdf, aes(x = trait_levels, y = mean,
                                            ymax = mean + std_dev,
                                            ymin = mean - std_dev), width = 0) +
    geom_point(data = fig6_avgsubdf, aes(x = trait_levels, y = mean, fill = result),
               shape = 21, size = 3) +
    # Add label for global P value
    ylim(-1, 1.1) +
    geom_text(label = unique(fig6_subdf$P_label), 
              x = (length(unique(fig6_subdf$trait_levels)) / 2) + 0.5, 
              y = -0.975, size = 6) +
    # Customize formatting / aesthetics
    labs(x = "Trait Values", y = "Cross-Species Synchrony") +
    scale_fill_manual(values = signif_palette) +
    scale_color_manual(values = signif_palette) +
    scale_x_discrete(limits = sub_level_vec) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text.x = element_text(size = 11))
  
  # Now add CLD letters to that plot (for each level in this trait)
  if(unique(is.na(fig6_subclddf$cld_letters)) != TRUE){
    # Loop to be flexible across different number of within-plot trait level numbers
    for(level_num in 1:length(unique(fig6_subdf$trait_levels))){
      fig6_subplot <- fig6_subplot +
        geom_text(label = fig6_subcld[[level_num]], 
                  x = names(fig6_subcld[level_num]), 
                  y = 1, angle = 0) } }
  
  # Add plot to the list
  fig6_plotlist[[aov_trait]] <- fig6_subplot
  
  # Message
  message("Graph created for '", aov_trait, "'")
  
} # Close figure list

# Generate actual figure
cowplot::plot_grid(plotlist = fig6_plotlist, nrow = 1, ncol = 2, align = 'h', labels = "AUTO")

# Save it locally
ggsave(filename = file.path("synchrony_figure_files", "sync_fig6_ANOVA_results_levels.png"),
       plot = last_plot(), width = 7, height = 5, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

# End ----
