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
librarian::shelf(googledrive, tidyverse, sf, see, maps, vegan,
                 njlyon0/supportR, cowplot, multcompView)

# Clear environment
rm(list = ls())

# Make sure R is authorized to work with GoogleDrive
googledrive::drive_auth()

# Run the entirety of the preparation script (if not already done so)
## Takes ~2 minutes to complete
source("synchrony_vis_prep.R")

# Clear environment
rm(list = ls())

# Read in 'core' synchrony data
sync_df <- read.csv(file = file.path("figure_data", "synchrony_viz-ready.csv"))

# Read in species-specific trait information
spp_traits <- read.csv(file = file.path("figure_data", "traits_viz-ready.csv"))

# Read in permuted vs. observed correlations
perm_df <- read.csv(file = file.path("figure_data", "perm_viz-ready.csv"))

# Read in MRM results
mrm_results <- read.csv(file = file.path("figure_data", "mrm_viz-ready.csv"))

# Read in ANOVA on trait 'status' (i.e., shared vs. unshared)
stat_aov <- read.csv(file = file.path("figure_data", "aov-status_viz-ready.csv"))

# Read in main results of ANOVA on trait 'levels' (i.e., actual trait values)
aov_results <- read.csv(file = file.path("figure_data", "aov-levels_viz-ready.csv"))

# Read in pairwise results of levels ANOVA
aov_pairs <- read.csv(file = file.path("figure_data", "aov-levels-pairs_viz-ready.csv"))

# Read in compact letter display for pairwise comparisons of levels ANOVA
aov_cld <- read.csv(file = file.path("figure_data", "aov-levels-cld_viz-ready.csv"))

# 
# 
# 
# # Identify names of files this script requires
# sync_file <- "synchrony_pcoa_climate_combination.csv" # synchrony + climate data
# trait_file <- "pre_ordination_trait_data.csv" # trait data
# perm_file <- "pairwise_corr_perm.csv" # correlation permutation data
# mrm_file <- "MRM_not_averaged_results_2023-06-14_10000perm.csv" # MRM results
# aov_file <- "ANOVA_trait_aov_tables_2023-06-14_10000perm.csv" # ANOVA results (trait levels)
# pair_file <- "ANOVA_trait_pairwise_comps_2023-06-14_10000perm.csv" # ANOVA pairwise comparisons
# stat_aov_file <- "ANOVA_trait_status_aov_tables_2023-06-14_10000perm.csv" # ANOVA on trait status (0 vs. 1)
# 
# # Identify links of relevant Drive folders
# sync_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")
# stats_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1cRJkEcoy81Keed6KWlj2FlOq3V_SnuPH")
# 
# # Identify relevant data from those folders
# ## List out all CSVs in all folders
# wanted_files <- googledrive::drive_ls(path = sync_folder, type = "csv") %>%
#   dplyr::bind_rows(googledrive::drive_ls(path = stats_folder, type = "csv")) %>%
#   ## Filter to only desired files
#   dplyr::filter(name %in% c(sync_file, trait_file, perm_file,
#                             mrm_file, aov_file, pair_file, stat_aov_file))
# 
# # Check this out to make sure it worked
# wanted_files
# 
# # Create folder to download files into
# dir.create(path = file.path("figure_data"), showWarnings = F)
# 
# # Download files into that folder
# purrr::walk2(.x = wanted_files$id, .y = wanted_files$name,
#              .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
#                                                 path = file.path("figure_data", .y),
#                                                 overwrite = T))
# 
# ## ------------------------------------------ ##
#               # Data Wrangling 
# ## ------------------------------------------ ##
# 
# # Read in synchrony data
# sync_df <- read.csv(file = file.path("figure_data", sync_file)) %>%
#   # Make a species pair column quickly
#   dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
#                 .before = Species1) %>%
#   # Drop needleleaf vs broadleaf
#   dplyr::select(-dplyr::starts_with("Needleleaf_Broadleaf_"))
# 
# # Glimpse it
# dplyr::glimpse(sync_df)
# 
# # Read in trait information
# spp_traits <- read.csv(file = file.path("figure_data", trait_file)) %>%
#   # Pivot to long format
#   tidyr::pivot_longer(cols = -lter:-Species.Name,
#                       names_to = "trait", values_to = "trait_value") %>%
#   # Streamline trait names to make NMS trait vectors simpler
#   dplyr::mutate(trait_actual = dplyr::case_when(
#     trait == "Deciduous_Evergreen_yrs__deciduous" ~ "Deciduous",
#     trait == "Deciduous_Evergreen_yrs__evergreen" ~ "Evergreen",
#     trait == "Dispersal_syndrome__abiotic" ~ "Abiotic_disp",
#     trait == "Dispersal_syndrome__endozoochory" ~ "Endozo_disp",
#     trait == "Dispersal_syndrome__synzoochory" ~ "Synzo_disp",
#     trait == "Fleshy_fruit__no" ~ "Not_fleshy_fruit",
#     trait == "Fleshy_fruit__yes" ~ "Fleshy_fruit",
#     trait == "Growth_form__liana" ~ "Liana",
#     trait == "Growth_form__shrub" ~ "Shrub",
#     trait == "Growth_form__tree" ~ "Tree",
#     trait == "Log10_seed_mass_mg" ~ "Log_seed_mass",
#     trait == "Mycorrhiza_AM_EM__am" ~ "AM_mycorr",
#     trait == "Mycorrhiza_AM_EM__em" ~ "EM_mycorr",
#     trait == "Mycorrhiza_AM_EM__ericoid" ~ "Ericoid_mycorr",
#     trait == "Mycorrhiza_AM_EM__none" ~ "No_mycorr",
#     trait == "Pollinator_code__animal" ~ "Animal_pollinated",
#     trait == "Pollinator_code__wind" ~ "Wind_pollinated",
#     ## Can't think of a great abbreviation for these two traits
#     # trait == "Seed_bank__no" ~ "No_seed_bank",
#     # trait == "Seed_bank__yes" ~ "Yes_seed_bank",
#     trait == "Seed_development_1_2or3yrs" ~ "Seed_dev_time",
#     trait == "Sexual_system__dioecious" ~ "Dioecious",
#     trait == "Sexual_system__hermaphrodite" ~ "Hermaphrodite",
#     trait == "Sexual_system__monoecious" ~ "Monoecious",
#     trait == "Sexual_system__polygamo_dioecious" ~ "Polygamo_dioecious",
#     trait == "Shade_tolerance__intermediate" ~ "Shade_intermediate_tolerant",
#     trait == "Shade_tolerance__intolerant" ~ "Shade_intolerant",
#     trait == "Shade_tolerance__tolerant" ~ "Shade_tolerant",
#     TRUE ~ trait)) %>%
#   # Drop original trait column
#   dplyr::select(-trait) %>%
#   # Pivot back to wide format
#   tidyr::pivot_wider(names_from = trait_actual, values_from = trait_value) %>%
#   # Filter to only desired LTERs
#   dplyr::filter(lter %in% c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV"))
# 
# # Glimpse it
# dplyr::glimpse(spp_traits)
# 
# # Read in permutations of correlations
# perm_df <- read.csv(file = file.path("figure_data", perm_file)) %>%
#   # Cut off below overlap threshold
#   dplyr::filter(overlap > 9) %>%
#   # Filter to only desired LTERs
#   dplyr::filter(lter %in% c("AND", "BNZ", "CDR", "CWT", "HBR", "LUQ", "SEV"))
# 
# # Check out structure
# dplyr::glimpse(perm_df)
# 
# # Read in MRM results
# mrm_results <- read.csv(file = file.path("figure_data", mrm_file)) %>%
#   # Drop all but saturated model
#   dplyr::filter(model == "saturated model" & coef != "Int") %>%
#   # Make coefficient column match trait name
#   dplyr::mutate(coef = gsub(pattern = "dist\\(|\\)", replacement = "", x = coef)) %>%
#   # Determine result (sig vs. NS)
#   dplyr::mutate(result = ifelse(test = (pval < 0.05),
#                                 yes = "sig", no = "NS")) %>%
#   # Pare down to minimum needed columns
#   dplyr::select(lter, coef, result) %>%
#   # Need to identify missing trait-site combinations
#   ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
#   tidyr::pivot_wider(names_from = coef, values_from = result,
#                      values_fill = "NA") %>%
#   tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "result")
# 
# # Glimpse it
# dplyr::glimpse(mrm_results)
# 
# # Read in ANOVA results too
# aov_results <- read.csv(file = file.path("figure_data", aov_file)) %>%
#   # Remove unwanted rows
#   dplyr::filter(!term %in% c("Residuals", "Total")) %>%
#   # Pare down to minimum needed columns
#   dplyr::select(lter, term, P) %>%
#   # Get to unique rows only
#   dplyr::distinct() %>%
#   # Need to identify missing trait-site combinations
#   ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
#   tidyr::pivot_wider(names_from = term, values_from = P,
#                      values_fill = 999) %>%
#   tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "P") %>%
#   # Determine result (sig vs. NS vs. NA)
#   dplyr::mutate(result = dplyr::case_when(P > 10 ~ "NA",
#                                           P < 0.05 ~ "sig",
#                                           TRUE ~ "NS")) %>%
#   # Wrangle P value for use as a label in plots
#   dplyr::mutate(P_round = as.character(round(P, digits = 12))) %>%
#   dplyr::mutate(
#     P_label = dplyr::case_when(as.numeric(P_round) < 0.001 ~ "P < 0.001",
#                                as.numeric(P_round) > 990 ~ "",
#                                as.numeric(P_round) >= 0.05 & 
#                                  as.numeric(P_round) < 900 ~ "NS",
#                                as.numeric(P_round) >= 0.001 &
#                                  as.numeric(P_round) < 0.05 ~ paste0("P = ",
#                                                                      stringr::str_sub(string = P_round, start = 1,  end = 5)))) %>%
#   # Drop unwanted columns
#   dplyr::select(-P_round, -P)
# 
# # Glimpse it
# dplyr::glimpse(aov_results)
# 
# # Read in pairwise comparisons results
# aov_pairs <- read.csv(file = file.path("figure_data", pair_file)) %>%
#   # Pare down to minimum needed columns
#   dplyr::select(lter, model, pairs, P) %>%
#   # Get to unique rows only
#   dplyr::distinct() %>% 
#   # Make the pairs non hyphenated
#   dplyr::mutate(pairs = gsub(pattern = "\\-", replacement = "_", x = pairs)) %>%
#   # Separate pairs into two columns
#   tidyr::separate(col = pairs, into = c("pair1", "pair2"), sep = ":")
# 
# # Glimpse it
# dplyr::glimpse(aov_pairs)
# 
# # Make an empty list
# aov_cld_list <- list()
# 
# # Now we need to generate the 'compact letter display' (aka "CLD") to use in the plots
# ## Loop across site
# for(pairs_lter in sort(unique(aov_pairs$lter))){
#   
#   # Subset to that site
#   pairs_sub <- aov_pairs %>%
#     dplyr::filter(lter == pairs_lter)
#   
#   # Generate an empty list
#   pairs_sub_list <- list()
#   
#   ## Loop across model (within site)
#   for(pairs_model in sort(unique(pairs_sub$model))) {
#     
#     # Subset to just this model
#     pairs_meta_sub <- pairs_sub %>%
#       dplyr::filter(model == pairs_model)
#     
#     # Create the necessary object for CLD identification
#     pre_cld <- pairs_meta_sub$P
#     names(pre_cld) <- paste0(pairs_meta_sub$pair1, "-", pairs_meta_sub$pair2)
#     
#     # Extract the raw CLD
#     raw_cld <- multcompView::multcompLetters(x = pre_cld, Letters = letters)
#     
#     # Wrangle this into a dataframe with necessary information
#     cld_df <- data.frame("lter" = pairs_lter,
#                          "trait" = pairs_model,
#                          "trait_levels" = gsub(pattern = "\\_", replacement = "-",
#                                                x = names(raw_cld$Letters)),
#                          "letter" = raw_cld$Letters)
#     
#     # Drop rownames
#     rownames(cld_df) <- NULL
#     
#     # Add to list
#     pairs_sub_list[[pairs_model]] <- cld_df
#     
#   } # Close within-LTER loop
#   
#   # Unlist into a dataframe
#   pairs_sub_df <- pairs_sub_list %>%
#     purrr::list_rbind(x = .)
#   
#   # And add that dataframe to the larger list
#   aov_cld_list[[pairs_lter]] <- pairs_sub_df
#   
#   # Success message
#   message("Finished processing '", pairs_lter, "'")
#   
# } # Close larger loop
# 
# # Unlist
# aov_cld <- aov_cld_list %>%
#   purrr::list_rbind(x = .)
# 
# # Glimpse this
# dplyr::glimpse(aov_cld)
# 
# # Read in trait status ANOVA results too
# stat_aov <- read.csv(file.path("figure_data", stat_aov_file)) %>%
#   # Pare down to minimum needed columns
#   dplyr::select(lter, term, P) %>%
#   # Get to unique rows only
#   dplyr::distinct() %>%
#   # Filter out non-P value rows
#   dplyr::filter(!is.na(P)) %>%
#   # Need to identify missing trait-site combinations
#   ## Can leverage `values_fill` argument in `pivot_wider` to do this *very* quickly
#   tidyr::pivot_wider(names_from = term, values_from = P,
#                      values_fill = 999) %>%
#   tidyr::pivot_longer(cols = -lter, names_to = "trait", values_to = "P") %>%
#   # Determine result (sig vs. NS vs. NA)
#   dplyr::mutate(result = dplyr::case_when(P > 10 ~ "NA",
#                                           P < 0.05 ~ "sig",
#                                           TRUE ~ "NS")) %>%
#   # Wrangle P value for use as a label in plots
#   dplyr::mutate(P_round = as.character(round(P, digits = 12))) %>%
#   dplyr::mutate(
#     P_label = dplyr::case_when(as.numeric(P_round) < 0.001 ~ "P < 0.001",
#                                as.numeric(P_round) > 990 ~ "",
#                                as.numeric(P_round) >= 0.05 & 
#                                  as.numeric(P_round) < 900 ~ "NS",
#                                as.numeric(P_round) >= 0.001 &
#                                  as.numeric(P_round) < 0.05 ~ paste0("P = ",
#                                                                      stringr::str_sub(string = P_round, start = 1,  end = 5)))) %>%
#   # Drop unwanted columns
#   dplyr::select(-P_round, -P)
# 
# # Glimpse this
# dplyr::glimpse(stat_aov)

# Create a local folder to export figures & supplemental figures to
dir.create(path = file.path("synchrony_supp_figures"), showWarnings = F)

# Define color palettes
## Site palette
site_palette <- c("CWT" = "#bd0026", "LUQ" = "orange", "HBR" = "gold", 
                  "AND" = "limegreen", "CDR" = "lightblue", "BNZ" = "#f1b6da", 
                  "SEV" = "#9d4edd")

## Significance palette
signif_palette <- c("sig" = "#2a9d8f", "NS" = "gray87", "NA" = "white")

# Define shape palette
shp_palette <- c("AND" = 22, "BNZ" = 21, "CDR" = 24, "CWT" = 23, 
                 "HBR" = 22, "LUQ" = 21, "SEV" = 24)

# Define objects to keep
keep_objects <- c("sync_df", "spp_traits", "perm_df", "mrm_results", "aov_results",
                  "aov_cld", "stat_aov", "site_palette", "signif_palette", "shp_palette")

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
          # ANOVAs of Trait 'Status' ----
## ------------------------------------------ ##
# 'Status' = 0 for unshared vs. 1 for shared where trait value identity doesn't matter

# 5a = Synchrony ~ trait status (faceted by trait)
# 5b = Synchrony ~ *log* seed mass similarity
# 5c = Synchrony ~ phylogenetic similarity

# Make a separate dataframe for this part of this figure
fig5a_df <- sync_df %>%
  # Pare down to needed columns
  dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_shared"),
                      names_to = "trait", values_to = "value") %>%
  # Force the lter column to be "All"
  dplyr::mutate(lter = "All") %>%
  # Make sure these traits match the trait status ones
  dplyr::mutate(trait = dplyr::case_when(
    trait == "Mycorrhiza_shared" ~ "Mycorrhizal_assoc._shared",
    trait == "Leaf_Longevity_shared" ~ "Leaf_longevity_shared",
    T ~ trait)) %>% 
  # Assign significance (identified by 'synchrony_mrm.R')
  dplyr::left_join(stat_aov, by = c("lter", "trait")) %>%
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
        strip.text.x = element_text(size = 11))

# View it
fig5a

# Check significance of phylogeny + seed mass
phylo_sig <- stat_aov %>%
  dplyr::filter(lter == "All" & trait == "Phylogenetic_similarity") %>%
  dplyr::pull(result)
seed_sig <- stat_aov %>%
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
        strip.text.x = element_text(size = 11))

# View it
fig5b

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
        strip.text.x = element_text(size = 11))

# View it as well
fig5c

# Add B & C into one object
fig5bc <- cowplot::plot_grid(fig5b, fig5c, nrow = 1, labels = c("B", "C"))
fig5bc

# Assemble figure
cowplot::plot_grid(fig5a, fig5bc, labels = c("A", ""), nrow = 2, ncol = 1,
                   rel_heights = c(1.5, 0.8))

# Export it!
ggsave(filename = file.path("synchrony_supp_figures", "anova_trait_status.png"),
       plot = last_plot(), width = 10, height = 10, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
         # ANOVA of Trait Levels ----
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
  # Synonymize any trait name mismatches
  dplyr::mutate(trait = dplyr::case_when(
    trait == "Mycorrhiza_values" ~ "Mycorrhizal_assoc._values",
    T ~ trait)) %>% 
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
    TRUE ~ trait_levels))

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

# Make a list (it's necessary)
fig6_plotlist <- list()

# Loop across traits
for(aov_trait in levels(fig6_df$trait_label)){
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
                  y = 1, angle = 90) } }
  
  # Add plot to the list
  fig6_plotlist[[aov_trait]] <- fig6_subplot
  
  # Message
  message("Graph created for '", aov_trait, "'")
  
} # Close figure list

# Drop x-axis label for plots that are in the top row
for(fig6_num in 1:5){
  fig6_plotlist[[fig6_num]] <- fig6_plotlist[[fig6_num]] +
    theme(axis.title.x = element_blank())
}

# Drop y-axis title, text, and label for all but leftmost graphs
for(fig6_num in c(2:5, 7:10)){
  fig6_plotlist[[fig6_num]] <- fig6_plotlist[[fig6_num]] +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
}

# Assemble figure
## Due to figure complexity this call takes a few minutes to complete
cowplot::plot_grid(plotlist = fig6_plotlist, nrow = 2, ncol = 5, align = 'h',
                   labels = rep(x = "", times = length(fig6_plotlist)))

# Save it locally
ggsave(filename = file.path("synchrony_supp_figures", "anova_trait_levels.png"),
       plot = last_plot(), width = 14, height = 10, units = "in", dpi = 720)

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
      # Per Site Trait 'Status' MRMs ----
## ------------------------------------------ ##
# 'Status' = 0 for un-shared vs. 1 for shared where trait value identity doesn't matter

# a = Synchrony ~ trait status (faceted by trait)
# b = Synchrony ~ *log* seed mass similarity
# c = Synchrony ~ phylogenetic similarity

# Loop across sites
for(sup5_site in unique(sync_df$lter)){
  
  # Subset to particular site
  sup5a_df <- sync_df %>%
    dplyr::filter(lter == sup5_site) %>%
    # Pare down to needed columns
    dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
    # Pivot to long format
    tidyr::pivot_longer(cols = dplyr::ends_with("_shared"),
                        names_to = "trait", values_to = "value") %>%
    # Assign significance (identified by 'synchrony_mrm.R')
    dplyr::left_join(mrm_results, by = c("lter", "trait")) %>%
    # Tidy trait names for use as axis labels & make the value a factor
    dplyr::mutate(trait = stringr::str_to_title(gsub(pattern = "_", replacement = " ", 
                                                     x = trait)),
                  value = as.factor(value)) %>%
    # Also drop "shared" from trait names
    dplyr::mutate(trait = gsub(pattern = " Shared", replacement = "", x = trait))
  
  # Get a summarized dataframe as well
  sup5a_avgdf <- supportR::summary_table(data = sup5a_df, response = "r.spearman",
                                         groups = c("trait", "result", "value"))
  
  # Make figure 5A
  sup5a <- ggplot(sup5a_df, aes(x = value, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_jitter(aes(color = result), width = 0.15, alpha = 0.25) +
    geom_violin(aes(fill = result, color = result), alpha = 0.1) +
    facet_wrap(. ~ trait, ncol = 5) +
    # Add averaged points with SD bars
    geom_errorbar(data = sup5a_avgdf, aes(x = value, y = mean, color = result, 
                                          ymax = mean + std_dev, 
                                          ymin = mean - std_dev), width = 0) +
    geom_point(data = sup5a_avgdf, aes(x = value, y = mean, fill = result, color = result), 
               shape = 21, size = 3) +
    # Customize formatting / aesthetics
    labs(x = "Trait Status (Unshared vs. Shared)", y = "Cross-Species Synchrony") +
    scale_fill_manual(values = signif_palette) +
    scale_color_manual(values = signif_palette) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 11))
  
  # Check significance of phylogeny + seed mass
  phylo_sig <- mrm_results %>%
    dplyr::filter(lter == sup5_site & trait == "Phylogenetic_similarity") %>%
    dplyr::pull(result)
  seed_sig <- mrm_results %>%
    dplyr::filter(lter == sup5_site & trait == "Seed_mass_similarity") %>%
    dplyr::pull(result)
  
  # Create dataframe for 5B & 5C
  sup5bc_df <- sync_df %>%
    # Filter to desired lter
    dplyr::filter(lter == sup5_site) %>%
    # Pare down to needed columns
    dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, 
                  Phylogenetic_similarity, Seed_mass_similarity) %>%
    # Add on the stats results
    dplyr::mutate(phylo_result = phylo_sig,
                  seed_result = seed_sig)
  
  # Make figure 5B
  sup5b <- ggplot(sup5bc_df, aes(x = Phylogenetic_similarity, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_point(aes(color = phylo_result), alpha = 0.3) +
    geom_smooth(aes(color = phylo_result), method = "lm", formula = "y ~ x") +
    labs(x = "Phylogenetic Similarity", y = "Cross-Species Synchrony") +
    scale_color_manual(values = signif_palette) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 11))
  
  # Make figure 5C too
  sup5c <- ggplot(sup5bc_df, aes(x = Seed_mass_similarity, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_point(aes(color = seed_result), alpha = 0.3) +
    geom_smooth(aes(color = seed_result), method = "lm", formula = "y ~ x") +
    labs(x = "log(Seed Mass) Similarity", y = "Cross-Species Synchrony") +
    scale_color_manual(values = signif_palette) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 11))
  
  # Add B & C into one object
  sup5bc <- cowplot::plot_grid(sup5b, sup5c, nrow = 1, labels = c("B", "C"))
  
  # Assemble figure
  cowplot::plot_grid(sup5a, sup5bc, labels = c("A", ""), nrow = 2, ncol = 1,
                     rel_heights = c(1.5, 0.8))
  
  # Export it!
  ggsave(filename = file.path("synchrony_supp_figures", 
                              paste0("mrm_trait_status_", sup5_site, ".png")),
         plot = last_plot(), width = 10, height = 10, units = "in", dpi = 720)
  
  # Message
  message("Supplemental figure 5 created for ", sup5_site) }

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
      # Per Site Trait 'Status' ANOVAs ----
## ------------------------------------------ ##
# 'Status' = 0 for unshared vs. 1 for shared where trait value identity doesn't matter

# 5a = Synchrony ~ trait status (faceted by trait)
# 5b = Synchrony ~ *log* seed mass similarity
# 5c = Synchrony ~ phylogenetic similarity

# Loop across sites
for(sup5_site in unique(sync_df$lter)){
  
  # Subset to particular site
  sup5a_df <- sync_df %>%
    dplyr::filter(lter == sup5_site) %>%
    # Pare down to needed columns
    dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, dplyr::ends_with("_shared")) %>%
    # Pivot to long format
    tidyr::pivot_longer(cols = dplyr::ends_with("_shared"),
                        names_to = "trait", values_to = "value") %>%
    # Make sure these traits match the trait status ones
    dplyr::mutate(trait = dplyr::case_when(
      trait == "Mycorrhiza_shared" ~ "Mycorrhizal_assoc._shared",
      trait == "Leaf_Longevity_shared" ~ "Leaf_longevity_shared",
      T ~ trait)) %>% 
    # Assign significance (identified by 'synchrony_mrm.R')
    dplyr::left_join(stat_aov, by = c("lter", "trait")) %>%
    # Tidy trait names for use as axis labels & make the value a factor
    dplyr::mutate(trait = stringr::str_to_title(gsub(pattern = "_", replacement = " ", 
                                                     x = trait)),
                  value = as.factor(value)) %>%
    # Also drop "shared" from trait names
    dplyr::mutate(trait = gsub(pattern = " Shared", replacement = "", x = trait))
  
  # Get a summarized dataframe as well
  sup5a_avgdf <- supportR::summary_table(data = sup5a_df, response = "r.spearman",
                                         groups = c("trait", "result", "value"))
  
  # Make figure 5A
  sup5a <- ggplot(sup5a_df, aes(x = value, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_jitter(aes(color = result), width = 0.15, alpha = 0.25) +
    geom_violin(aes(fill = result, color = result), alpha = 0.1) +
    facet_wrap(. ~ trait, ncol = 5) +
    # Add averaged points with SD bars
    geom_errorbar(data = sup5a_avgdf, aes(x = value, y = mean, color = result, 
                                          ymax = mean + std_dev, 
                                          ymin = mean - std_dev), width = 0) +
    geom_point(data = sup5a_avgdf, aes(x = value, y = mean, fill = result, color = result), 
               shape = 21, size = 3) +
    # Customize formatting / aesthetics
    labs(x = "Trait Status (Unshared vs. Shared)", y = "Cross-Species Synchrony") +
    scale_fill_manual(values = signif_palette) +
    scale_color_manual(values = signif_palette) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 11))
  
  # Check significance of phylogeny + seed mass
  phylo_sig <- stat_aov %>%
    dplyr::filter(lter == sup5_site & trait == "Phylogenetic_similarity") %>%
    dplyr::pull(result)
  seed_sig <- stat_aov %>%
    dplyr::filter(lter == sup5_site & trait == "Seed_mass_similarity") %>%
    dplyr::pull(result)
  
  # Create dataframe for 5B & 5C
  sup5bc_df <- sync_df %>%
    # Filter to desired lter
    dplyr::filter(lter == sup5_site) %>%
    # Pare down to needed columns
    dplyr::select(lter, Plot.ID, Species_Pair, r.spearman, 
                  Phylogenetic_similarity, Seed_mass_similarity) %>%
    # Add on the stats results
    dplyr::mutate(phylo_result = phylo_sig,
                  seed_result = seed_sig)
  
  # Make figure 5B
  sup5b <- ggplot(sup5bc_df, aes(x = Phylogenetic_similarity, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_point(aes(color = phylo_result), alpha = 0.3) +
    geom_smooth(aes(color = phylo_result), method = "lm", formula = "y ~ x") +
    labs(x = "Phylogenetic Similarity", y = "Cross-Species Synchrony") +
    scale_color_manual(values = signif_palette) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 11))
  
  # Make figure 5C too
  sup5c <- ggplot(sup5bc_df, aes(x = Seed_mass_similarity, y = r.spearman)) +
    # Add horizontal line at synchrony = 0
    geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
    # Actual plotting content
    geom_point(aes(color = seed_result), alpha = 0.3) +
    geom_smooth(aes(color = seed_result), method = "lm", formula = "y ~ x") +
    labs(x = "log(Seed Mass) Similarity", y = "Cross-Species Synchrony") +
    scale_color_manual(values = signif_palette) +
    supportR::theme_lyon(title_size = 14, text_size = 11) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 11))
  
  # Add B & C into one object
  sup5bc <- cowplot::plot_grid(sup5b, sup5c, nrow = 1, labels = c("B", "C"))
  
  # Assemble figure
  cowplot::plot_grid(sup5a, sup5bc, labels = c("A", ""), nrow = 2, ncol = 1,
                     rel_heights = c(1.5, 0.8))
  
  # Export it!
  ggsave(filename = file.path("synchrony_supp_figures", 
                              paste0("anova_trait_status_", sup5_site, ".png")),
         plot = last_plot(), width = 10, height = 10, units = "in", dpi = 720)
  
  # Message
  message("Supplemental figure created for ", sup5_site) }

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
          # Per Site Trait Levels ----
## ------------------------------------------ ##

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

# Loop across sites
for(sup6_site in unique(sync_df$lter)){
  
  # Make a dataframe for this figure
  sup6_df <- sync_df %>%
    # Filter to just this site
    dplyr::filter(lter == sup6_site) %>%
    # Pare down to needed columns
    dplyr::select(lter, Species_Pair, r.spearman, dplyr::ends_with("_values")) %>%
    # Pivot to long format
    tidyr::pivot_longer(cols = dplyr::ends_with("_values"),
                        names_to = "trait", values_to = "trait_levels") %>%
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
      TRUE ~ trait_levels))
  
  # Make a list (it's necessary)
  sup6_plotlist <- list()
  
  # Loop across traits
  for(aov_trait in levels(sup6_df$trait_label)){
    # for(aov_trait in "Pollinator Values") {
    
    # Subset data to only that trait
    sup6_subdf <- sup6_df %>%
      dplyr::filter(trait_label == aov_trait)
    
    # Get an averaged dataframe too
    sup6_avgsubdf <- supportR::summary_table(data = sup6_subdf, response = "r.spearman",
                                             groups = c("trait_label", "result", "trait_levels"))
    
    # Make another object that is *only* the compact letter display content
    sup6_subclddf <- sup6_subdf %>%
      dplyr::select(trait_levels, cld_letters) %>%
      dplyr::distinct()
    
    # Make this a named vector
    sup6_subcld <- sup6_subclddf$cld_letters
    names(sup6_subcld) <- sup6_subclddf$trait_levels
    
    # Identify trait levels for this trait  
    sub_level_vec <- level_vec[level_vec %in% unique(sup6_avgsubdf$trait_levels)]
    
    # Make plot
    sup6_subplot <- ggplot(sup6_subdf, aes(x = trait_levels, y = r.spearman)) +
      # Add horizontal line at synchrony = 0
      geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
      # Actual plotting content
      geom_jitter(aes(color = result), width = 0.15, alpha = 0.25) +
      geom_violin(aes(fill = result, color = result), alpha = 0.1) +
      facet_wrap(. ~ trait_label, ncol = 5) +
      # Add averaged points with SD bars
      geom_errorbar(data = sup6_avgsubdf, aes(x = trait_levels, y = mean, color = result,
                                              ymax = mean + std_dev,
                                              ymin = mean - std_dev), width = 0) +
      geom_point(data = sup6_avgsubdf, aes(x = trait_levels, y = mean, 
                                           fill = result, color = result),
                 shape = 21, size = 3) +
      # Add label for global P value
      ylim(-1, 1.1) +
      geom_text(label = unique(sup6_subdf$P_label), 
                x = (length(unique(sup6_subdf$trait_levels)) / 2) + 0.5, 
                y = -0.975, size = 6) +
      # Customize formatting / aesthetics
      labs(x = "Trait Values", y = "Cross-Species Synchrony") +
      scale_fill_manual(values = signif_palette) +
      scale_x_discrete(limits = sub_level_vec) +
      scale_color_manual(values = signif_palette) +
      supportR::theme_lyon(title_size = 14, text_size = 11) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text.x = element_text(size = 11))
    
    # Now add CLD letters to that plot (for each level in this trait)
    if(unique(is.na(sup6_subclddf$cld_letters)) != TRUE){
      # Loop to be flexible across different number of within-plot trait level numbers
      for(level_num in 1:length(unique(sup6_subdf$trait_levels))){
        sup6_subplot <- sup6_subplot +
          geom_text(label = sup6_subcld[[level_num]], 
                    x = names(sup6_subcld[level_num]), 
                    y = 1, angle = 90) } }
    
    # Add plot to the list
    sup6_plotlist[[aov_trait]] <- sup6_subplot } # Close figure list
  
  # Drop x-axis label for plots that are in the top row
  for(sup6_num in 1:5){
    sup6_plotlist[[sup6_num]] <- sup6_plotlist[[sup6_num]] +
      theme(axis.title.x = element_blank())
  }
  
  # Drop y-axis title, text, and label for all but leftmost graphs
  for(sup6_num in c(2:5, 7:10)){
    sup6_plotlist[[sup6_num]] <- sup6_plotlist[[sup6_num]] +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank())
  }
  
  # Assemble figure
  ## Due to figure complexity this call takes a few minutes to complete
  cowplot::plot_grid(plotlist = sup6_plotlist, nrow = 2, ncol = 5, align = 'h',
                     labels = rep(x = "", times = length(sup6_plotlist)))
  
  # Save it locally
  ggsave(filename = file.path("synchrony_supp_figures", 
                              paste0("anova_trait_levels_", sup6_site, ".png")),
         plot = last_plot(), width = 14, height = 10, units = "in", dpi = 720)
  
  # Message
  message("Supplemental figure created for ", sup6_site) }

# Clean up  environment
rm(list = setdiff(ls(), c(keep_objects, "keep_objects")))

## ------------------------------------------ ##
        # Export Figures to Drive ----
## ------------------------------------------ ##

# Identify folder to export plots to
suppfig_folder <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1YsDZwsav5saLHaYrPis4OvSGAd1_Q_C7")

# Identify figures that we have created
suppfig <- dir(path = file.path("synchrony_supp_figures"))

# Upload each to the Drive (skipping giant files if there)
for(file in setdiff(suppfig, c("anova_trait_levels.png", "anova_trait_status.png"))){
  googledrive::drive_upload(media = file.path("synchrony_supp_figures", file),
                            path = suppfig_folder, overwrite = T) }

# End ----
