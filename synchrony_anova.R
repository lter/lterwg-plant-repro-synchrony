## ------------------------------------------ ##
            # Synchrony ANOVAs
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Using permutation analysis of variance (perANOVA)
## analyze effects of trait levels (separately) on synchrony (i.e., correlation)

# PRE-REQUISITES
## The following script(s) must be run--in order--for this script to work as intended
## 1. Run `synchrony_stats_prep.R`

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP)

# Clear environment
rm(list = ls())

# Create needed local folder(s)
dir.create(path = file.path("stats_results"), showWarnings = F)

# Read in needed file
sync_df <- read.csv(file = file.path("tidy_data", "synchrony_data.csv")) %>%
  # Make a species pair column quickly
  dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
                .before = Species1)

# Glimpse it
dplyr::glimpse(sync_df)

# Set permutation number
iter_num <- 10000

## ------------------------------------------ ##
        # Trait Level ANOVAs - Fit ----
## ------------------------------------------ ##

# Make an empty list for anova tables and one for pairwise
aov_list <- list()
pairs_list <- list()

# For each site (plus "all" sites together)
for(site in c("All", unique(sync_df$lter))){
# for(site in "All"){

  # Starting message
  message("Processing begun for site: ", site)
  
  # Subset to data to relevant site
  if(site == "All"){ sub_df <- sync_df } else { sub_df <- dplyr::filter(sync_df, lter == site) }
  
  # Pollinator_code_values
  if(length(unique(sub_df$Pollinator_code_values)) != 1){
    
  ## Fit anova
  poll_fit <- RRPP::lm.rrpp(r.spearman ~ Pollinator_code_values, 
                            iter = iter_num, data = sub_df)
  ## Strip AOV table
  poll_aov <- as.data.frame(anova(poll_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "pollinator values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  poll_pairs <- summary(RRPP::pairwise(fit = poll_fit, groups = sub_df$Pollinator_code_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "pollinator values", 
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_poll")]] <- poll_aov
  pairs_list[[paste0(site, "_poll")]] <- poll_pairs }
  
  # Seed_development_values
  if(length(unique(sub_df$Seed_development_values)) != 1){
  ## Fit anova
  seed_dev_fit <- RRPP::lm.rrpp(r.spearman ~ Seed_development_values, 
                                iter = iter_num, data = sub_df)
  ## Strip AOV table
  seed_dev_aov <- as.data.frame(anova(seed_dev_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "seed development values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  seed_dev_pairs <- summary(RRPP::pairwise(fit = seed_dev_fit, 
                                           groups = sub_df$Seed_development_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "seed development values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_seed_dev")]] <- seed_dev_aov
  pairs_list[[paste0(site, "_seed_dev")]] <- seed_dev_pairs }
  
  # Mycorrhiza_values
  if(length(unique(sub_df$Mycorrhiza_values)) != 1){
  ## Fit anova
  myco_fit <- RRPP::lm.rrpp(r.spearman ~ Mycorrhiza_values, 
                            iter = iter_num, data = sub_df)
  ## Strip AOV table
  myco_aov <- as.data.frame(anova(myco_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "mycorrhiza values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  myco_pairs <- summary(RRPP::pairwise(fit = myco_fit, groups = sub_df$Mycorrhiza_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "mycorrhiza values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_myco")]] <- myco_aov
  pairs_list[[paste0(site, "_myco")]] <- myco_pairs }
  
  # Deciduous_Evergreen_values
  if(length(unique(sub_df$Deciduous_Evergreen_values)) != 1){
  ## Fit anova
  decid_fit <- RRPP::lm.rrpp(r.spearman ~ Deciduous_Evergreen_values, 
                             iter = iter_num, data = sub_df)
  ## Strip AOV table
  decid_aov <- as.data.frame(anova(decid_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "deciduous evergreen values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  decid_pairs <- summary(RRPP::pairwise(fit = decid_fit, 
                                        groups = sub_df$Deciduous_Evergreen_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "deciduous evergreen values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_decid")]] <- decid_aov
  pairs_list[[paste0(site, "_decid")]] <- decid_pairs }
  
  # Dispersal_syndrome_values
  if(length(unique(sub_df$Dispersal_syndrome_values)) != 1){
  ## Fit anova
  disp_fit <- RRPP::lm.rrpp(r.spearman ~ Dispersal_syndrome_values, 
                            iter = iter_num, data = sub_df)
  ## Strip AOV table
  disp_aov <- as.data.frame(anova(disp_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "dispersal syndrome values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  disp_pairs <- summary(RRPP::pairwise(fit = disp_fit, 
                                       groups = sub_df$Dispersal_syndrome_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "dispersal syndrome values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_disp")]] <- disp_aov
  pairs_list[[paste0(site, "_disp")]] <- disp_pairs }
  
  # Sexual_system_values
  if(length(unique(sub_df$Sexual_system_values)) != 1){
  ## Fit anova
  sex_fit <- RRPP::lm.rrpp(r.spearman ~ Sexual_system_values, 
                           iter = iter_num, data = sub_df)
  ## Strip AOV table
  sex_aov <- as.data.frame(anova(sex_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "sexual system values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  sex_pairs <- summary(RRPP::pairwise(fit = sex_fit, 
                                      groups = sub_df$Sexual_system_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "sexual system values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_sex")]] <- sex_aov
  pairs_list[[paste0(site, "_sex")]] <- sex_pairs }
  
  # Shade_tolerance_values
  if(length(unique(sub_df$Shade_tolerance_values)) != 1){
  ## Fit anova
  shade_fit <- RRPP::lm.rrpp(r.spearman ~ Shade_tolerance_values, 
                             iter = iter_num, data = sub_df)
  ## Strip AOV table
  shade_aov <- as.data.frame(anova(shade_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "shade tolerance values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  shade_pairs <- summary(RRPP::pairwise(fit = shade_fit, groups = sub_df$Shade_tolerance_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "shade tolerance values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_shade")]] <- shade_aov
  pairs_list[[paste0(site, "_shade")]] <- shade_pairs }
  
  # Growth_form_values
  if(length(unique(sub_df$Growth_form_values)) != 1){
  ## Fit anova
  growth_fit <- RRPP::lm.rrpp(r.spearman ~ Growth_form_values, 
                              iter = iter_num, data = sub_df)
  ## Strip AOV table
  growth_aov <- as.data.frame(anova(growth_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "growth form values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  growth_pairs <- summary(RRPP::pairwise(fit = growth_fit, groups = sub_df$Growth_form_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "growth form values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_growth")]] <- growth_aov
  pairs_list[[paste0(site, "_growth")]] <- growth_pairs }
  
  # Fleshy_fruit_values
  if(length(unique(sub_df$Fleshy_fruit_values)) != 1){
  ## Fit anova
  flesh_fit <- RRPP::lm.rrpp(r.spearman ~ Fleshy_fruit_values, 
                             iter = iter_num, data = sub_df)
  ## Strip AOV table
  flesh_aov <- as.data.frame(anova(flesh_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "fleshy fruit values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  flesh_pairs <- summary(RRPP::pairwise(fit = flesh_fit, groups = sub_df$Fleshy_fruit_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "fleshy fruit values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_flesh")]] <- flesh_aov
  pairs_list[[paste0(site, "_flesh")]] <- flesh_pairs }
  
  # Seed_bank_values
  if(length(unique(sub_df$Seed_bank_values)) != 1){
  ## Fit anova
  seed_bank_fit <- RRPP::lm.rrpp(r.spearman ~ Seed_bank_values, 
                                 iter = iter_num, data = sub_df)
  ## Strip AOV table
  seed_bank_aov <- as.data.frame(anova(seed_bank_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "seed bank values", 
                  term = row.names(.),
                  .before = dplyr::everything())
  ## Strip pairwise comparisons out as well
  seed_bank_pairs <- summary(RRPP::pairwise(fit = seed_bank_fit, groups = sub_df$Seed_bank_values))$summary.table %>%
    as.data.frame() %>%
    dplyr::mutate(lter = site,
                  model = "seed bank values",  
                  pairs = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_seed_bank")]] <- seed_bank_aov
  pairs_list[[paste0(site, "_seed_bank")]] <- seed_bank_pairs }
  
  # Seed_mass_similarity
  ## Fit anova
  seed_mass_fit <- RRPP::lm.rrpp(r.spearman ~ Seed_mass_similarity, 
                                 iter = iter_num, data = sub_df)
  ## Strip AOV table
  seed_mass_aov <- as.data.frame(anova(seed_mass_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "seed mass similarity", 
                  term = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_seed_mass")]] <- seed_mass_aov
  
  # Phylogenetic_similarity
  ## Fit anova
  phylo_fit <- RRPP::lm.rrpp(r.spearman ~ Phylogenetic_similarity, 
                             iter = iter_num, data = sub_df)
  ## Strip AOV table
  phylo_aov <- as.data.frame(anova(phylo_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "phylogenetic similarity", 
                  term = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  aov_list[[paste0(site, "_phylo")]] <- phylo_aov
  
  # Ending message
  message("Finished with site: ", site) }

## ------------------------------------------ ##
      # Trait Level ANOVAs - Export ----
## ------------------------------------------ ##

# Unlist and examine the ANOVA tables
aov_df <- aov_list %>%
  purrr::list_rbind() %>%
  # Rename problematic columns
  dplyr::rename(P = `Pr(>F)`) %>%
  # Arrange by sites and model term
  dplyr::arrange(lter, model)

# Glimpse
dplyr::glimpse(aov_df)

# Do the same for pairwise comparisons
pairs_df <- pairs_list %>%
  purrr::list_rbind() %>%
  # Fix some column names
  dplyr::rename(UCL_95perc = `UCL (95%)`,
                P = `Pr > d`) %>%
  # Arrange by sites and model term and pair
  dplyr::arrange(lter, model, pairs)

# Examine
dplyr::glimpse(pairs_df)

# Assemble file names for both
(aov_name <- paste0("ANOVA_trait_aov_tables_", Sys.Date(), "_", iter_num, "perm.csv"))
(pairs_name <- paste0("ANOVA_trait_pairwise_comps_", Sys.Date(), "_", iter_num, "perm.csv"))

# Export both locally
write.csv(x = aov_df, file = file.path("stats_results", aov_name), row.names = F, na = '')
write.csv(x = pairs_df, file = file.path("stats_results", pairs_name), row.names = F, na = '')

# Clean up environment
rm(list = setdiff(ls(), c("sync_df", "iter_num")))

## ------------------------------------------ ##
       # Trait Status ANOVAs - Fit ----
## ------------------------------------------ ##

# Make an empty list for anova tables for trait status
stat_list <- list()
stat_pair_list <- list()

# For each site (plus "all" sites together)
for(site in c("All", unique(sync_df$lter))){
  # for(site in "All"){
  
  # Starting message
  message("Processing begun for site: ", site)
  
  # Subset to data to relevant site
  if(site == "All"){ sub_df <- sync_df } else { sub_df <- dplyr::filter(sync_df, lter == site) }
  
  # Pollinator_code_shared
  if(length(unique(sub_df$Pollinator_code_shared)) != 1){
    
    ## Fit anova
    poll_fit <- RRPP::lm.rrpp(r.spearman ~ Pollinator_code_shared, 
                              iter = iter_num, data = sub_df)
    ## Strip AOV table
    poll_aov <- as.data.frame(anova(poll_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "pollinator values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    poll_pairs <- summary(RRPP::pairwise(fit = poll_fit, groups = sub_df$Pollinator_code_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "pollinator values", 
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_poll")]] <- poll_aov
    stat_pair_list[[paste0(site, "_poll")]] <- poll_pairs }
  
  # Seed_development_shared
  if(length(unique(sub_df$Seed_development_shared)) != 1){
    ## Fit anova
    seed_dev_fit <- RRPP::lm.rrpp(r.spearman ~ Seed_development_shared, 
                                  iter = iter_num, data = sub_df)
    ## Strip AOV table
    seed_dev_aov <- as.data.frame(anova(seed_dev_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "seed development values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    seed_dev_pairs <- summary(RRPP::pairwise(fit = seed_dev_fit, 
                                             groups = sub_df$Seed_development_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "seed development values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_seed_dev")]] <- seed_dev_aov
    stat_pair_list[[paste0(site, "_seed_dev")]] <- seed_dev_pairs }
  
  # Mycorrhiza_shared
  if(length(unique(sub_df$Mycorrhiza_shared)) != 1){
    ## Fit anova
    myco_fit <- RRPP::lm.rrpp(r.spearman ~ Mycorrhiza_shared, 
                              iter = iter_num, data = sub_df)
    ## Strip AOV table
    myco_aov <- as.data.frame(anova(myco_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "mycorrhiza values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    myco_pairs <- summary(RRPP::pairwise(fit = myco_fit, groups = sub_df$Mycorrhiza_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "mycorrhiza values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_myco")]] <- myco_aov
    stat_pair_list[[paste0(site, "_myco")]] <- myco_pairs }
  
  # Deciduous_Evergreen_shared
  if(length(unique(sub_df$Deciduous_Evergreen_shared)) != 1){
    ## Fit anova
    decid_fit <- RRPP::lm.rrpp(r.spearman ~ Deciduous_Evergreen_shared, 
                               iter = iter_num, data = sub_df)
    ## Strip AOV table
    decid_aov <- as.data.frame(anova(decid_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "deciduous evergreen values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    decid_pairs <- summary(RRPP::pairwise(fit = decid_fit, 
                                          groups = sub_df$Deciduous_Evergreen_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "deciduous evergreen values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_decid")]] <- decid_aov
    stat_pair_list[[paste0(site, "_decid")]] <- decid_pairs }
  
  # Dispersal_syndrome_shared
  if(length(unique(sub_df$Dispersal_syndrome_shared)) != 1){
    ## Fit anova
    disp_fit <- RRPP::lm.rrpp(r.spearman ~ Dispersal_syndrome_shared, 
                              iter = iter_num, data = sub_df)
    ## Strip AOV table
    disp_aov <- as.data.frame(anova(disp_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "dispersal syndrome values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    disp_pairs <- summary(RRPP::pairwise(fit = disp_fit, 
                                         groups = sub_df$Dispersal_syndrome_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "dispersal syndrome values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_disp")]] <- disp_aov
    stat_pair_list[[paste0(site, "_disp")]] <- disp_pairs }
  
  # Sexual_system_shared
  if(length(unique(sub_df$Sexual_system_shared)) != 1){
    ## Fit anova
    sex_fit <- RRPP::lm.rrpp(r.spearman ~ Sexual_system_shared, 
                             iter = iter_num, data = sub_df)
    ## Strip AOV table
    sex_aov <- as.data.frame(anova(sex_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "sexual system values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    sex_pairs <- summary(RRPP::pairwise(fit = sex_fit, 
                                        groups = sub_df$Sexual_system_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "sexual system values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_sex")]] <- sex_aov
    stat_pair_list[[paste0(site, "_sex")]] <- sex_pairs }
  
  # Shade_tolerance_shared
  if(length(unique(sub_df$Shade_tolerance_shared)) != 1){
    ## Fit anova
    shade_fit <- RRPP::lm.rrpp(r.spearman ~ Shade_tolerance_shared, 
                               iter = iter_num, data = sub_df)
    ## Strip AOV table
    shade_aov <- as.data.frame(anova(shade_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "shade tolerance values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    shade_pairs <- summary(RRPP::pairwise(fit = shade_fit, groups = sub_df$Shade_tolerance_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "shade tolerance values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_shade")]] <- shade_aov
    stat_pair_list[[paste0(site, "_shade")]] <- shade_pairs }
  
  # Growth_form_shared
  if(length(unique(sub_df$Growth_form_shared)) != 1){
    ## Fit anova
    growth_fit <- RRPP::lm.rrpp(r.spearman ~ Growth_form_shared, 
                                iter = iter_num, data = sub_df)
    ## Strip AOV table
    growth_aov <- as.data.frame(anova(growth_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "growth form values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    growth_pairs <- summary(RRPP::pairwise(fit = growth_fit, groups = sub_df$Growth_form_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "growth form values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_growth")]] <- growth_aov
    stat_pair_list[[paste0(site, "_growth")]] <- growth_pairs }
  
  # Fleshy_fruit_shared
  if(length(unique(sub_df$Fleshy_fruit_shared)) != 1){
    ## Fit anova
    flesh_fit <- RRPP::lm.rrpp(r.spearman ~ Fleshy_fruit_shared, 
                               iter = iter_num, data = sub_df)
    ## Strip AOV table
    flesh_aov <- as.data.frame(anova(flesh_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "fleshy fruit values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    flesh_pairs <- summary(RRPP::pairwise(fit = flesh_fit, groups = sub_df$Fleshy_fruit_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "fleshy fruit values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_flesh")]] <- flesh_aov
    stat_pair_list[[paste0(site, "_flesh")]] <- flesh_pairs }
  
  # Seed_bank_shared
  if(length(unique(sub_df$Seed_bank_shared)) != 1){
    ## Fit anova
    seed_bank_fit <- RRPP::lm.rrpp(r.spearman ~ Seed_bank_shared, 
                                   iter = iter_num, data = sub_df)
    ## Strip AOV table
    seed_bank_aov <- as.data.frame(anova(seed_bank_fit)$table) %>%
      dplyr::mutate(lter = site,
                    model = "seed bank values", 
                    term = row.names(.),
                    .before = dplyr::everything())
    ## Strip pairwise comparisons out as well
    seed_bank_pairs <- summary(RRPP::pairwise(fit = seed_bank_fit, groups = sub_df$Seed_bank_shared))$summary.table %>%
      as.data.frame() %>%
      dplyr::mutate(lter = site,
                    model = "seed bank values",  
                    pairs = row.names(.),
                    .before = dplyr::everything())
    
    ## Add to their respective lists
    stat_list[[paste0(site, "_seed_bank")]] <- seed_bank_aov
    stat_pair_list[[paste0(site, "_seed_bank")]] <- seed_bank_pairs }
  
  # Seed_mass_similarity
  ## Fit anova
  seed_mass_fit <- RRPP::lm.rrpp(r.spearman ~ Seed_mass_similarity, 
                                 iter = iter_num, data = sub_df)
  ## Strip AOV table
  seed_mass_aov <- as.data.frame(anova(seed_mass_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "seed mass similarity", 
                  term = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  stat_list[[paste0(site, "_seed_mass")]] <- seed_mass_aov
  
  # Phylogenetic_similarity
  ## Fit anova
  phylo_fit <- RRPP::lm.rrpp(r.spearman ~ Phylogenetic_similarity, 
                             iter = iter_num, data = sub_df)
  ## Strip AOV table
  phylo_aov <- as.data.frame(anova(phylo_fit)$table) %>%
    dplyr::mutate(lter = site,
                  model = "phylogenetic similarity", 
                  term = row.names(.),
                  .before = dplyr::everything())
  
  ## Add to their respective lists
  stat_list[[paste0(site, "_phylo")]] <- phylo_aov
  
  # Ending message
  message("Finished with site: ", site) }

## ------------------------------------------ ##
      # Trait Status ANOVAs - Export ----
## ------------------------------------------ ##

# Unlist and examine the ANOVA tables
stat_df <- stat_list %>%
  purrr::list_rbind() %>%
  # Rename problematic columns
  dplyr::rename(P = `Pr(>F)`) %>%
  # Arrange by sites and model term
  dplyr::arrange(lter, model)

# Glimpse
dplyr::glimpse(stat_df)

# Do the same for pairwise comparisons
stat_pair_df <- stat_pair_list %>%
  purrr::list_rbind() %>%
  # Fix some column names
  dplyr::rename(UCL_95perc = `UCL (95%)`,
                P = `Pr > d`) %>%
  # Arrange by sites and model term and pair
  dplyr::arrange(lter, model, pairs)

# Examine
dplyr::glimpse(stat_pair_df)

# Assemble file names for both
(stat_name <- paste0("ANOVA_trait_status_aov_tables_", Sys.Date(), "_", iter_num, "perm.csv"))
(stat_pair_name <- paste0("ANOVA_trait_status_pairwise_comps_", Sys.Date(), "_", iter_num, "perm.csv"))

# Export both locally
write.csv(x = stat_df, file = file.path("stats_results", stat_name), row.names = F, na = '')
write.csv(x = stat_pair_df, file = file.path("stats_results", stat_pair_name), row.names = F, na = '')

# Clean up environment
rm(list = setdiff(ls(), c("sync_df", "iter_num")))

# End ----
