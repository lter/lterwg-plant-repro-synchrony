## ------------------------------------------ ##
        # Synchrony Mixed-Effect Models
## ------------------------------------------ ##
# Written by: Miranda Redmond, Nick J Lyon

# PURPOSE
## Performs linear mixed effects model of synchrony as related to trait similarity, phylogenetic relatedness, and climate variables. 

## ------------------------------------------ ##
                # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, lme4, MuMIn, lmerTest, purrr)

# Clear environment
rm(list = ls())

# Create local needed folders
dir.create(path = file.path("tidy_data"), showWarnings = F)
dir.create(path = file.path("stats_results"), showWarnings = F)

# Read in synchrony data
pc_clim_sync_df <- read.csv(file = file.path("tidy_data", "synchrony_pcoa_climate_combination.csv"))

# Do needed wrangling
pc_clim_sync_df_climsite <- pc_clim_sync_df %>%
  # Pare down to only needed columns
  dplyr::select(AET, CWD_log, TraitSimilarityJaccardVariant, Phylogenetic_similarity,
                speciespair, climatesite, r.spearman) %>%
  # Summarize within groups
  group_by(speciespair, climatesite) %>%
  dplyr::summarise(AET = mean(AET, na.rm = T),
                   CWD_log = mean(CWD_log),
                   TraitSimilarityJaccardVariant = mean(TraitSimilarityJaccardVariant, 
                                                        na.rm = T),
                   Phylogenetic_similarity = mean(Phylogenetic_similarity, na.rm = T),
                   r.spearman = mean(r.spearman, na.rm = T)) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(pc_clim_sync_df_climsite)

## ------------------------------------------ ##
        # Fit Mixed-Effects Models ----
## ------------------------------------------ ##

# Fit full model
fullmod <- lmerTest::lmer(r.spearman ~ scale(AET) + scale(CWD_log) + scale(TraitSimilarityJaccardVariant) + scale(Phylogenetic_similarity) + (1|climatesite) + (1|speciespair),
                          data = pc_clim_sync_df_climsite, na.action = na.fail, REML = T)

# Check model summary
summary(fullmod)

# Fit climate variables only mod
climateonlymod <- lmer(r.spearman ~ (AET) + (CWD_log) + (1|climatesite) + (1|speciespair),
                       data = pc_clim_sync_df_climsite)

# Check that model's summary
summary(climateonlymod)




