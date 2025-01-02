# [Community Synchrony in Seed Production is Associated With Trait Similarity and Climate Across North America](https://onlinelibrary.wiley.com/doi/ftr/10.1111/ele.14498)

![DOI](https://img.shields.io/badge/DOI-10.1111/ele.14498-blue.svg)

## Authors

Jalene M. LaMontagne, Department of Biological Sciences, DePaul University, Chicago, IL. USA;
David F. Greene, Department of Forestry, Fire, and Range Management. Cal Poly Humboldt, Arcata, CA, USA;
E. Penelope Holland, Department of Biology, University of York, York, UK;
Jill F. Johnstone, Institute of Arctic Biology, University of Alaska Fairbanks, Fairbanks, AK, USA;
Mark Schulze, H.J. Andrews Experimental Forest, Oregon State University; Blue River, OR, USA;
Jess K. Zimmerman, Department of Environmental Sciences, University of Puerto Rico, San Juan, PR, USA;
Nicholas J. Lyon, Long Term Ecological Research Network Office. National Center for Ecological Analysis and Synthesis, University of California Santa Barbara, Santa Barbara, CA, USA;
Angel Chen, Long Term Ecological Research Network Office. National Center for Ecological Analysis and Synthesis, University of California Santa Barbara, Santa Barbara, CA, USA;
Tom E.X. Miller, Department of BioSciences, Rice University, Houston, TX USA;
Katherine M. Nigro, ORISE Postdoctoral fellow, Rocky Mountain Research Station, USDA Forest Service, Fort Collins, CO, USA;
Rebecca S. Snell, Department of Environmental and Plant Biology, Ohio University, Athens, OH, USA;
Jessica H. Barton, Department of Biological Sciences, DePaul University, Chicago, IL. USA;
V. Bala Chaudhary, Environmental Studies Department, Dartmouth College, Hanover, NH, USA;
Natalie L. Cleavitt, Department of Natural Resources and the Environment, Cornell University, Ithaca, NY, USA;
Elizabeth E. Crone, Department of Evolution & Ecology, University of California, Davis CA 95618 USA;
Walter D. Koenig, Hastings Reservation, University of California Berkeley, Carmel Valley, CA, USA;
Diana Macias, Department of Environmental Science, Policy, and Management. University of California Berkeley, Berkeley, CA, USA;
Ian S. Pearse, U.S. Geological Survey, Fort Collins Science Center, Fort Collins, CO, USA;
Miranda D. Redmond, Department of Environmental Science, Policy, and Management. University of California Berkeley, Berkeley, CA, USA

## Associated Paper Abstract

Mast seeding, the synchronous and highly variable production of seed crops in perennial plants, is a population-level phenomenon and has cascading effects in ecosystems. Mast seeding studies are typically conducted at the population/species level. Much less is known about synchrony in mast seeding between species because the necessary long-term data are rarely available. To investigate synchrony between species within communities, we used long-term data from seven forest communities in the US Long-Term Ecological Research (LTER) network, ranging from tropical rainforest to boreal forest. We focus on cross-species synchrony and (i) quantify synchrony in reproduction overall and within LTER sites, (ii) test for relationships between synchrony with trait and phylogenetic similarity, and (iii) investigate how climate conditions are related to levels of synchrony. Overall, reproductive synchrony between woody plant species was greater than expected by chance, but spanned a wide range of values between species. Based on 11 functional and reproductive traits for 103 species (plus phylogenetic relatedness), cross-species synchrony in reproduction was driven primarily by trait similarity with phylogeny being largely unimportant, and synchrony was higher in sites with greater climatic water deficit. Community-level synchrony in asting has consequences for understanding forest regeneration dynamics and consumer-resource interactions.

## Script Explanations

### Analysis

1. **`synchrony_mrm`** - Performs multiple regression on distance matrices (MRM) to assess effect of species' traits on synchrony. Requires data archived on <u>Figshare</u> (use 'synchrony_pcoa_climate_combination.csv').

2. **`synchrony_perm-stats`** - Analyzes difference between observed and permuted correlations. Requires data archived on <u>Figshare</u> (use 'synchrony_pcoa_climate_combination.csv' and 'perm_corr_unsummarized.csv').

3. **`synchrony_anova`** - Performs permutation analysis of variance (perANOVA) to assess the effect of _each trait_ (<u>separately</u>) on synchrony. Requires data archived on <u>Figshare</u> (use 'synchrony_pcoa_climate_combination.csv').

4. **`synchrony_similarity-stats`** - Performs perANOVA on trait similarity.Requires data archived on <u>Figshare</u> (use 'synchrony_pcoa_climate_combination.csv').

5. **`synchrony_climate-trait-phylog-stats`** - Performs linear mixed effects model of synchrony as related to trait similarity, phylogenetic relatedness, and climate variables. Requires data archived on <u>Figshare</u> (use 'synchrony_pcoa_climate_combination.csv').

### Summary Calculation & Visualization

6. **`synchrony_tables`** - Creates the summary tables included in the paper. This makes it easier to update in the manuscript than manually re-summarizing. Requires data archived on <u>Figshare</u> (use 'synchrony_pcoa_climate_combination.csv').

7. **`synchrony_map`** - Makes the map of all sites included in this paper. Requires a site coordinate CSV and [a forest cover raster](https://lpdaac.usgs.gov/products/mcd12q1v006/)

8. **`synchrony_vis-prep`** - Wrangles tidy data and statistical results files in preparation for use in creating downstream visualizations. Requires data produced by <u>all preceding scripts</u>

9. **`synchrony_figures`** - Generates the figures included in this paper. Requires data produced by <u>script 8</u>

10. **`synchrony_supp-figs`** - Generates both the literal supplemental figures included in the Supplementary Materials for this publication and a host of other non-primary figures that may or may not be included in any part of the official publication (supplemental or otherwise). Requires data produced by <u>script 8</u>
