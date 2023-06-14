# Synchrony graph script
# Run Nick's Script and filtered data for graphing - might want a separate filtering script from analysis/graphing
# Jalene LaMontagne - March 2023

## ------------------------------------------ ##
# Model Comparison & Extraction
## ------------------------------------------ ##
# Written by: Nick J Lyon & Jalene LaMontagne

# PURPOSE
## Create figures for the LTER Synchrony paper

## ------------------------------------------ ##
# Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, MuMIn, ecodist, lme4, lmerTest)

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
# Download Data ----
## ------------------------------------------ ##

# Download prepared data - same as lines 20-34 in synchrony_mrm.R
## See "synchrony_data_prep.R" for how this file is created
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) %>%
  dplyr::filter(name=="synchrony_data.csv") %>%
    googledrive::drive_download(., overwrite = T)

# Read in that file
sync_df <- read.csv(file = "synchrony_data.csv") %>%
  # Make a species pair column quickly
  dplyr::mutate(Species_Pair = paste(Species1, Species2, sep = "__"),
                .before = Species1)

#Get the permutation dataframe for synchrony to make some graphs
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL")) %>%
  dplyr::filter(name=="pairwise_corr_perm.csv") %>%
  googledrive::drive_download(., overwrite = T)
# Read in that file
sync_perm_df <- read.csv(file ="pairwise_corr_perm.csv")
#Ensure that the permutation dataframe only includes species with timeseries that overlap for at least 9 years
sync_perm_df<-subset(sync_perm_df,overlap>9)
#Also, exclude lter ADK
sync_perm_df<-subset(sync_perm_df, lter!="ADK")

# Glimpse it
dplyr::glimpse(sync_df)
dplyr::glimpse(sync_perm_df)

## End of script to obtain data

################################################
### Now, make graphs

# consistent colour theme for all sites
site.colours <- c("LUQ" = "red3", "SEV" = "orange","CWT"= "limegreen", 
                 "HFR" = "limegreen","HBR" = "forestgreen","ADK"="grey20",
                 "AND"= "blue", "CDR"="violet",  "BNZ" = "purple3")

# consistent shape theme for all sites
site.shape <- c("LUQ" = 15, "SEV" = 16,"CWT"= 17, 
                "HFR" = 15,"HBR" = 18,"ADK"= 19,
                "AND"= 8, "CDR"= 15 , "BNZ" = 19)

# Fig 2 of paper - Histogram
# Mast seeding correlation - across all sites with Comparison to permutation analysis
ggplot(sync_perm_df) + 
  geom_histogram(aes(x=r.spearman.perm),alpha=0.1,binwidth = 0.1, boundary=0.1,color="black", fill="black")+
  geom_histogram(aes(x=r.spearman),alpha=0.5, binwidth = 0.1, boundary=0.1,color="black", fill="purple")+
  labs(x="Cross-species synchrony", y = "Frequency")+
  scale_x_continuous(limits=c(-1, 1),breaks=seq(-1,max(1), by = 0.5))+
  scale_y_continuous(limits=c(0, 400),breaks=seq(0,max(400), by = 100))+
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  theme(axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")))

t.test(sync_perm_df$r.spearman, sync_perm_df$r.spearman.perm, paired = TRUE)
ks.test(sync_perm_df$r.spearman, sync_perm_df$r.spearman.perm)

# Spearman at each LTER site - actual data
ggplot(sync_perm_df, aes(x = lter, y = r.spearman))+ 
  geom_jitter(height = 0, width = 0.2, alpha = 0.8, aes(colour=lter))+
  geom_boxplot(outlier.shape = NA, fill=NA)+
  geom_hline(yintercept = 0, linetype = 2)+
  scale_y_continuous(limits=c(-1, 1),breaks=seq(-1,max(1), by = 0.5))+
  theme_classic()+
  coord_flip()+
  ylab("Spearman's r")+
  xlab("LTER site")+
  # scale_colour_manual(values = rev(site.colours))+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        strip.text = element_text(size=12))

# Spearman at each LTER site - based on permuatations (null distributions)
ggplot(sync_perm_df, aes(x = lter, y = r.spearman.perm))+ 
  geom_jitter(height = 0, width = 0.2, alpha = 0.8, aes(colour=lter))+
  geom_boxplot(outlier.shape = NA, fill=NA)+
  geom_hline(yintercept = 0, linetype = 2)+
  scale_y_continuous(limits=c(-1, 1),breaks=seq(-1,max(1), by = 0.5))+
  theme_classic()+
  coord_flip()+
  ylab("Spearman's r - Permutated data")+
  xlab("LTER site")+
  # scale_colour_manual(values = rev(site.colours))+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        strip.text = element_text(size=12))

#Mast seeding correlation - violin plot - across all sites
ggplot(sync_perm_df, aes(x=1, y = r.spearman)) + # fill=name allow to automatically dedicate a color for each group
  geom_hline(yintercept=0) +
  geom_point(alpha = 0.8, colour="grey", position = position_jitter(seed = 1, width = 0.15)) +
  geom_violin(alpha=0.0, width=0.3) +
  geom_boxplot(width=0.25, outlier.shape = NA, alpha=0) +  #makes a boxplot and removes the outlier points
  scale_x_discrete(name="All LTER sites") +
  scale_y_continuous(name="Mast-seeding correlation", limits=c(-1, 1), breaks=seq(-1,1,0.5), expand = c(0,0)) +
  stat_summary(fun=median, geom="point", shape=17,size=4, color="black")+
  theme_classic() +
  theme(text = element_text(size = 20),axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  coord_flip()# +

#Mast seeding correlation - violin plot - sites separated
ggplot(sync_df, aes(x=as.character(lter), y = r.spearman)) + # fill=name allow to automatically dedicate a color for each group
  geom_hline(yintercept=0, lwd=0.5, linetype="solid",colour="black") +
  geom_point(alpha=0.8,aes(colour=lter,shape=lter), size=2, position = position_jitter(seed = 1, width = 0.1)) +
  geom_boxplot(width=0.33, outlier.shape = NA, alpha=0, lwd=0.6) +  #makes a boxplot and removes the outlier points
  scale_fill_manual(values = site.colours)+
  scale_colour_manual(values = site.colours)+
  scale_shape_manual(values=site.shape)+
  scale_x_discrete(name="LTER site", limits=rev) +
  scale_y_continuous(name="Cross-species synchrony", limits=c(-1, 1), breaks=seq(-1,1,0.5), expand = c(0, 0)) +
#  stat_summary(fun=median, geom="point", shape=17,size=2, color="black")+
  theme_classic() +
  theme(text = element_text(size = 20, colour = "black"),axis.title.x = element_text(margin = margin(t = 10), colour="black"),
        axis.title.y = element_text(margin = margin(r = 10), colour="black")) +
  coord_flip()

#Pollinator_similarity
ggplot(sync_df, aes(x = as.character(Pollinator_code_shared), y = r.spearman)) + # fill=name allow to automatically dedicate a color for each group
  geom_hline(yintercept=0) +
  geom_point(alpha = 1,aes(colour=lter), position = position_jitter(seed = 1, width = 0.2)) +
  scale_colour_manual(values = site.colours)+
  geom_violin(alpha=0.0, width=0.5) +
  geom_boxplot(width=0.25, outlier.shape = NA, alpha=0) +  #makes a boxplot and removes the outlier points
  scale_x_discrete(name="Pollinator shared") +
  scale_y_continuous(name="Mast-seeding correlation", limits=c(-1, 1), breaks=seq(-1,1,0.5)) +
  stat_summary(fun.y=median, geom="point", shape=17,size=4, color="black")+
  theme_classic() +
  theme(text = element_text(size = 20),axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  coord_flip()# +
#  facet_wrap(facets = ~reorder(Pollinator_code_shared,-Pollinator_code_shared),ncol=1)


#Pollinator code values
ggplot(sync_df, aes(x = Pollinator_code_values, y = r.spearman))+geom_boxplot()

#Seed development values
ggplot(sync_df, aes(x = Seed_development_values, y = r.spearman))+geom_boxplot()+
  geom_hline(yintercept=0)+
  geom_jitter(height = 0, width = 0.1, aes(colour = lter))+
  scale_colour_manual(values = site.colours)

#Mycorrhiza_similarity
ggplot(sync_df, aes(x = as.character(lter), y = r.spearman)) + # fill=name allow to automatically dedicate a color for each group
  geom_hline(yintercept=0) +
  geom_point(alpha = 1,aes(colour=lter), position = position_jitter(seed = 1, width = 0.2)) +
  scale_colour_manual(values = site.colours)+
  geom_violin(alpha=0.0, width=0.8) +
  geom_boxplot(width=0.25, outlier.shape = NA, alpha=0) +  #makes a boxplot and removes the outlier points
  scale_x_discrete(name="Mycorrhiza shared", limits=rev) +
  scale_y_continuous(name="Mast-seeding correlation", limits=c(-1, 1), breaks=seq(-1,1,0.5)) +
  stat_summary(fun.y=median, geom="point", shape=17,size=4, color="black")+
  theme_classic() +
  theme(text = element_text(size = 14),axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))+
  coord_flip() +
  facet_wrap(facets = ~reorder(Mycorrhiza_shared,-Mycorrhiza_shared),nrow=1)

#Mycorrhiza_values - v1
#ggplot(sync_df, aes(x = Mycorrhiza_values, y = r.spearman))+geom_boxplot()+
#      geom_jitter(height = 0, width = 0.1, aes(colour = lter))+
#      scale_colour_manual(values = site.colours)

#Mycorrhiza_values - v2
ggplot(sync_df, aes(x = Mycorrhiza_values, y = r.spearman)) + # fill=name allow to automatically dedicate a color for each group
  geom_hline(yintercept=0) +
  geom_point(alpha = 1,aes(colour=lter), position = position_jitter(seed = 1, width = 0.2)) +
  scale_colour_manual(values = site.colours)+
  geom_violin(alpha=0.0, width=0.8) +
  geom_boxplot(width=0.25, outlier.shape = NA, alpha=0) +  #makes a boxplot and removes the outlier points
  scale_x_discrete(name="Mycorrhiza values", expand=c(0.2, 0),limits=rev) +
  scale_y_continuous(name="Mast-seeding correlation", limits=c(-1, 1), breaks=seq(-1,1,0.5)) +
  stat_summary(fun.y=median, geom="point", shape=17,size=3, color="black")+
  theme_classic() +
  theme(text = element_text(size = 14),axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))+
  coord_flip()


###More graphing

ggplot(sync_df, aes(x = Needleleaf_Broadleaf_values, y = r.spearman))+geom_boxplot()
ggplot(sync_df, aes(x = Deciduous_Evergreen_values, y = r.spearman))+geom_boxplot()
ggplot(sync_df, aes(x = Dispersal_syndrome_values, y = r.spearman))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggplot(sync_df, aes(x = Sexual_system_values, y = r.spearman))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggplot(sync_df, aes(x = Shade_tolerance_values, y = r.spearman))+geom_boxplot()+
  geom_jitter(height = 0, width = 0.1, aes(colour = lter))+
  scale_colour_manual(values = site.colours)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(sync_df, aes(x = Growth_form_values, y = r.spearman))+geom_boxplot()
ggplot(sync_df, aes(x = Fleshy_fruit_values, y = r.spearman))+geom_boxplot()
ggplot(sync_df, aes(x = Seed_bank_values, y = r.spearman))+geom_boxplot()
ggplot(sync_df, aes(x = Seed_mass_similarity, y = r.spearman,color = lter))+geom_point()+
  facet_wrap(~lter)
ggplot(sync_df, aes(x = CV_similarity, y = r.spearman,color = lter))+geom_point()+
  facet_wrap(~lter)
ggplot(sync_df, aes(x = ACL1_similarity, y = r.spearman,color = lter))+geom_point()+
  facet_wrap(~lter)



# an interesting graph? - made by Rebecca
cor_graph <- sync_df %>%
  select(1:5, contains("shared")) %>%
  pivot_longer(6:16,names_to = "Trait", values_to = "Shared")

cor_graph_summary <- cor_graph %>%
  group_by(lter, Trait, Shared) %>%
  summarise(meanCor.spearman = mean(r.spearman))

ggplot() +
  geom_jitter(data = cor_graph, aes(x = factor(Shared), y = r.spearman, colour = lter),
              height = 0, width = 0.1, alpha = 0.1)+
  geom_line(data = cor_graph_summary, aes(x = factor(Shared), y = meanCor.spearman, colour =lter, group = lter),
            size = 1)+
  geom_point(data = cor_graph_summary, aes(x = factor(Shared), y = meanCor.spearman, fill =lter),
             shape = 21, colour = "black", size = 3)+
  facet_wrap(~Trait)+
  theme_bw()+
  xlab("Shared trait")+
  scale_x_discrete(labels = c("No","Yes"))+
  ylab("Spearmans r")+
  scale_color_manual(values = site.colours)+
  scale_fill_manual(values = site.colours)


##28 March 2023 - Pen
sync_df$pair_similarity_discrete <- rowSums(sync_df[,11:21])/11
ggplot(sync_df, aes(x=pair_similarity_discrete, y=r.spearman))+ 
  geom_jitter(alpha=0.5, width=0.01, height=0) + 
  geom_smooth(method="lm") + 
  xlab("Trait similarity") + 
  ylab("Spearman's correlation")

ggplot(sync_df, aes(x=pair_similarity_discrete, y=r.spearman, colour=lter))+ 
  geom_jitter(alpha=0.5, width=0.01, height=0) + 
  geom_smooth(method="lm") + 
  xlab("Trait similarity") + 
  ylab("Spearman's correlation") + 
  theme_classic() + 
  labs(colour = "LTER site")

###------------------------------
#Exploratory plots
#Simple plots of relationships with cross-species synchrony
boxplot(sync_df$r.spearman~sync_df$Pollinator_code_shared)
boxplot(sync_df$r.spearman~sync_df$Pollinator_code_values)

boxplot(sync_df$r.spearman~sync_df$Mycorrhiza_shared)
boxplot(sync_df$r.spearman~sync_df$Mycorrhiza_values)

boxplot(sync_df$r.spearman~sync_df$Seed_development_shared)
boxplot(sync_df$r.spearman~sync_df$Seed_development_values)

boxplot(sync_df$r.spearman~sync_df$Deciduous_Evergreen_shared)
boxplot(sync_df$r.spearman~sync_df$Deciduous_Evergreen_values)

boxplot(sync_df$r.spearman~sync_df$Dispersal_syndrome_shared)
boxplot(sync_df$r.spearman~sync_df$Dispersal_syndrome_values)

boxplot(sync_df$r.spearman~sync_df$Sexual_system_shared)
boxplot(sync_df$r.spearman~sync_df$Sexual_system_values)

boxplot(sync_df$r.spearman~sync_df$Shade_tolerance_shared)
boxplot(sync_df$r.spearman~sync_df$Shade_tolerance_values)

boxplot(sync_df$r.spearman~sync_df$Growth_form_shared)
boxplot(sync_df$r.spearman~sync_df$Growth_form_values)

boxplot(sync_df$r.spearman~sync_df$Fleshy_fruit_shared)
boxplot(sync_df$r.spearman~sync_df$Fleshy_fruit_values)

boxplot(sync_df$r.spearman~sync_df$Seed_bank_shared)
boxplot(sync_df$r.spearman~sync_df$Seed_bank_values)


#Not in MRM full model as significant - not everything is here
plot(sync_df$r.spearman~sync_df$Seed_mass_similarity)
plot(sync_df$r.spearman~sync_df$Phylogenetic_similarity)

#Histograms of pairwise synchrony at LTER sites. Plots within sites are included.
ggplot(sync_df, aes(x=lter, y=Phylogenetic_similarity))+geom_boxplot()+
  geom_jitter(height = 0, width = 0.1, alpha = 0.2, size = 3)+
  theme_bw()
ggplot(sync_df, aes(x = ACL1_Sp1, y = ACL1_Sp2, colour = ACL1_similarity))+
  geom_point()+
  theme_bw()+
  scale_colour_gradientn(colours = rainbow(10))
ggplot(sync_df, aes(x = CV_Sp1, y = CV_Sp2, colour = CV_similarity))+
  geom_point()+
  theme_bw()+
  scale_colour_gradientn(colours = rainbow(10))
ggplot(sync_df, aes(x = CV_Sp1, y = CV_Sp2, colour = CV_similarity))+
  geom_point()+
  theme_bw()+
  facet_wrap(~lter, scales = "fixed")+
  scale_colour_gradientn(colours = rainbow(10))
ggplot(sync_df, aes(x = log10(Seed_mass_Sp1), y = log10(Seed_mass_Sp2), colour = Seed_mass_similarity))+
  geom_point()+
  theme_bw()+
  facet_wrap(~lter, scales = "free")+
  scale_colour_gradientn(colours = rainbow(10))

library(tidyverse)
#CV1 and CV2 with synchrony - separate out pairs of needleleaf and pairs of broadleaf species
tapply(sync_df$r.spearman,list(sync_df$Growth_form_values,sync_df$lter,sync_df$Needleleaf_Broadleaf_values),length)
sync_df_needleleaf <- subset(sync_df, Needleleaf_Broadleaf_values=="needleleaf-needleleaf")
sync_df_broadleaf_treesOnly_noLUQ <- subset(sync_df, Needleleaf_Broadleaf_values=='broadleaf-broadleaf' & Growth_form_values=='tree-tree' & lter %in% c('CWT','CDR','HBR'))

#Synchrony and Phylogentic distance - needleleaf
sync_df_needleleaf %>%
  arrange(desc(r.spearman)) %>%
  ggplot(aes(x=Phylo_distance, y=r.spearman, color=lter)) +
  geom_point(alpha=0.8, size=3) +
    scale_y_continuous(name="Cross-species synchrony", limits=c(-1, 1), breaks=seq(-1,1,0.5), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  scale_colour_manual(values = site.colours)+
  theme_classic() +
  theme(text = element_text(size = 14))
  
#Synchrony and Phylogentic distance - broadleaf
sync_df_broadleaf_treesOnly_noLUQ %>%
  arrange(desc(r.spearman)) %>%
  ggplot(aes(x=Phylo_distance, y=r.spearman, color=lter)) +
  geom_point(alpha=0.8, size=3) +
  scale_y_continuous(name="Cross-species synchrony", limits=c(-1, 1), breaks=seq(-1,1,0.5), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  theme_classic() +
  theme(text = element_text(size = 14))

library(latticeExtra) 
library(RColorBrewer)
#my.col <- colorRampPalette(brewer.pal(11, "RdBu"))(diff(range(dat$z)))
# showing data points on the same color scale 
#col.l <- colorRampPalette(c('red', 'yellow'))(100)
levelplot(r.spearman ~ CV_Sp1 * CV_Sp2, sync_df_needleleaf, 
          at = seq(-max(abs(sync_df_needleleaf$r.spearman)), max(abs(sync_df_needleleaf$r.spearman)),
                   length = 100),
          panel = panel.levelplot.points, cex = 1.5, par.settings = custom.theme.2(),
          xlim=c(1.11,2.25), 
          ylim=c(0.65,2.235), 
          xlab="CV Sp1", ylab="CV Sp2",
          colorkey = TRUE) + 
  layer_(panel.2dsmoother(..., n = 200))

levelplot(r.spearman ~ CV_Sp1 * CV_Sp2, sync_df_broadleaf_treesOnly_noLUQ, 
          at = seq(-max(abs(sync_df_broadleaf_treesOnly_noLUQ$r.spearman)), 
                   max(abs(sync_df_broadleaf_treesOnly_noLUQ$r.spearman)),
          length = 100),
          panel = panel.levelplot.points, cex = 1.5, par.settings = custom.theme.2(),
          xlim=c(0.34,4.23), ylim=c(0.345,4.23),
          xlab="CV Sp1", ylab="CV Sp2",
          cex.axis = 40,
          colorkey = TRUE) + 
  layer_(panel.2dsmoother(..., n = 200))

#Create symmetrical heatmaps
CV_pairs_sync_needleleaf_df1<-sync_df_needleleaf[,c("lter","Species1","Species2","r.spearman","CV_Sp1","CV_Sp2")]
CV_pairs_sync_needleleaf_df2<-sync_df_needleleaf[,c("lter","Species2","Species1","r.spearman","CV_Sp2","CV_Sp1")]
#change column names in _df2 above to then rbind
colnames(CV_pairs_sync_needleleaf_df2) <-c("lter","Species1","Species2","r.spearman","CV_Sp1","CV_Sp2")
sync_df_needleleaf_CV<-rbind(CV_pairs_sync_needleleaf_df1,CV_pairs_sync_needleleaf_df2)

#Create symmetrical heatmaps
CV_pairs_sync_broadleaf_df1<-sync_df_broadleaf_treesOnly_noLUQ[,c("lter","Species1","Species2","r.spearman","CV_Sp1","CV_Sp2")]
CV_pairs_sync_broadleaf_df2<-sync_df_broadleaf_treesOnly_noLUQ[,c("lter","Species2","Species1","r.spearman","CV_Sp2","CV_Sp1")]
#change column names in _df2 above to then rbind
colnames(CV_pairs_sync_broadleaf_df2) <-c("lter","Species1","Species2","r.spearman","CV_Sp1","CV_Sp2")
sync_df_broadleaf_treesOnly_noLUQ_CV<-rbind(CV_pairs_sync_broadleaf_df1,CV_pairs_sync_broadleaf_df2)


#make Symmetrical plot - needleleaf
levelplot(r.spearman ~ CV_Sp1 * CV_Sp2, sync_df_needleleaf_CV, 
          at = seq(-max(abs(sync_df_needleleaf$r.spearman)), max(abs(sync_df_needleleaf$r.spearman)),
                   length = 100),
          panel = panel.levelplot.points, cex = 1.5, par.settings = custom.theme.2(),
          xlim=c(0.65,2.235), 
          ylim=c(0.65,2.235), 
          xlab="CV Sp1", ylab="CV Sp2",
          colorkey = TRUE) + 
  layer_(panel.2dsmoother(..., n = 200))

levelplot(r.spearman ~ CV_Sp1 * CV_Sp2, sync_df_broadleaf_treesOnly_noLUQ_CV, 
          at = seq(-max(abs(sync_df_broadleaf_treesOnly_noLUQ$r.spearman)), 
                   max(abs(sync_df_broadleaf_treesOnly_noLUQ$r.spearman)),
                   length = 100),
          panel = panel.levelplot.points, cex = 1.5, par.settings = custom.theme.2(),
          xlim=c(0.42,4.23), ylim=c(0.42,4.23),
          xlab="CV Sp1", ylab="CV Sp2",
          cex.axis = 40,
          colorkey = TRUE) + 
  layer_(panel.2dsmoother(..., n = 200))

#Heatmap code based on a couple websites:
#https://r-graph-gallery.com/201-levelplot-with-latticeextra.html
#https://stackoverflow.com/questions/6181653/creating-a-more-continuous-color-palette-in-r-ggplot2-lattice-or-latticeextra
#dat <- data.frame(x = rnorm(1000), y = rnorm(1000), z = sample(0:40, 1000, TRUE))
#my.col <- colorRampPalette(brewer.pal(11, "RdBu"))(diff(range(dat$z)))
#dat <- data.frame(x = rnorm(1000), y = rnorm(1000), z = rnorm(1000, mean = 1))
## for centering the colour key around zero
#levelplot(z ~ x * y, dat, at = seq(-1, 1, length = 100), 
#          panel = panel.levelplot.points, par.settings = custom.theme.2())

