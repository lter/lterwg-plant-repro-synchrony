# Cross-species synchrony in mast seeding in 103 woody plant species: Functional traits, climate, and geographic patterns

## Authors

Under Construction!

## Abstract

Under Construction!

## Script Explanations

### Core Workflow

1. **`synchrony_stats_prep`** - Absorb tidied mast seeding data and do pre-statistics preparatory wrangling. Tidying scripts housed in a different repository and will be published on in a different paper

2. **`synchrony_mrm`** - Performs multiple regression on distance matrices (MRM) to assess effect of species' traits on synchrony. Requires data produced by <u>script 1</u>

3. **`synchrony_perm-stats`** - Analyzes difference between observed and permuted correlations. Requires data produced by <u>script 1</u>

4. **`synchrony_anova`** - Performs permutation analysis of variance (perANOVA) to assess the effect of _each trait_ (<u>separately</u>) on synchrony. Requires data produced by <u>script 1</u>

5. **`synchrony_similarity-stats`** - Performs perANOVA on the 'Jaccard variant' of trait similarity. "Variant" because the authors have made some tweaks to the calculation of this metric so it is similar to Jaccard but not technically the same thing. Requires data produced by <u>script 1</u>

6. **`synchrony_climate_trait_phylog-stats`** - Performs linear mixed effects model of synchrony as related to trait similarity, phylogenetic relatedness, and climate variables. Requires data produced by <u>script 1</u>

7. **`synchrony_tables`** - Creates the summary tables included in the paper. This makes it easier to update in the manuscript than manually re-summarizing. Requires data produced by <u>script 1</u>

8. **`synchrony_map`** - Makes the map of all sites included in this paper (plus a forest cover layer)

9. **`synchrony_vis_prep`** - Wrangles tidy data and statistical results files in preparation for use in creating downstream visualizations. Requires data produced by <u>scripts 1-5</u>

10. **`synchrony_figures`** - Generates the figures included in this paper. Requires data produced by <u>script 9</u>

11. **`synchrony_supp_figs`** - Generates both the literal supplemental figures included in the Supplementary Materials for this publication and a host of other non-primary figures that may or may not be included in any part of the official publication (supplemental or otherwise). Requires data produced by <u>script 9</u>

### Ancillary

- **`synchrony_explore`** - Creates exploratory visuals that are not necessarily publication quality nor necessarily of interest for this publication. Requires data produced by <u>script 9</u>
