library(lme4)
library(tidyverse)
library(MuMIn)
library(lmerTest)
library(googledrive)
library(purrr)

#download data
syncdata_url<-"https://drive.google.com/drive/u/0/folders/1c7M1oMaCtHy-IQIJVcuyrKvwlpryM2vL"
folder <- drive_get(as_id(syncdata_url), shared_drive = "LTER-WG_Plant_Reproduction")
## identify the csv files in that folder
csv_files <- drive_ls(folder, type = "csv")

walk(csv_files$id, ~ drive_download(as_id(.x), overwrite=TRUE))

pc_clim_sync_df<-read.csv("synchrony_pcoa_climate_combination.csv")
pc_clim_sync_df_climsite<-pc_clim_sync_df%>%
  select(AET,CWD_log,TraitSimilarityJaccardVariant,Phylogenetic_similarity,speciespair,climatesite,r.spearman)%>%
  group_by(speciespair,climatesite)%>%
  summarise(AET=mean(AET),CWD_log=mean(CWD_log),TraitSimilarityJaccardVariant=mean(TraitSimilarityJaccardVariant),Phylogenetic_similarity=mean(Phylogenetic_similarity),r.spearman=mean(r.spearman))

fullmod<-lmer(r.spearman~scale(AET)+scale(CWD_log)+scale(TraitSimilarityJaccardVariant)+scale(Phylogenetic_similarity)+(1|climatesite)+(1|speciespair),data=pc_clim_sync_df_climsite,na.action=na.fail,REML=TRUE)
summary(fullmod)

#climate variables only mod
climateonlymod<-lmer(r.spearman~(AET)+(CWD_log)+(1|climatesite)+(1|speciespair),data=pc_clim_sync_df_climsite)
#climateonlymod<-lmer(r.spearman~(AET)+(CWD_log)+(1|Plot.ID)+(1|speciespair),data=pc_clim_sync_df)
summary(climateonlymod)
