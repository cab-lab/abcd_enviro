if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('mediation')) install.packages('mediation'); library(mediation)
if (!require('Hmisc')) install.packages('Hmisc'); library(Hmisc)


## import data 
abcd_data_final <- read_csv("abcd_data_final.csv")

set.seed(1992)

nrand <- 100
n_sites <- length(unique(abcd_data_final$abcd_site))
n_site_split <- n_sites/2
site_list <- as.character(unique(abcd_data_final$abcd_site))
site_splits_rand <- data.frame(matrix(nrow = n_site_split, ncol = nrand))

for (i in 1:nrand){
  tmp_sitesplit <- sample(site_list, n_site_split, replace = FALSE, prob = NULL)
  site_splits_rand[,i] <- tmp_sitesplit
  
  rm(tmp_sitesplit)
}

for (i in 1:nrand){
  
  site1_data <- abcd_data_final %>%
    filter(abcd_site %in% site_splits_rand[,i]) %>%
    standardize()
  
  site2_data <- abcd_data_final %>%
    filter(abcd_site %nin% site_splits_rand[,i]) %>%
    standardize()
  
  site1_culture_PC_list[[i]]    <- PCA(site1_data[CultureEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site1_home_PC_list[[i]]    <- PCA(site1_data[HomeEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site1_school_PC_list[[i]]    <- PCA(site1_data[SchoolEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site1_neighborhood_PC_list[[i]]    <- PCA(site1_data[NeighborhoodEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site1_sociodemo_PC_list[[i]]    <- PCA(site1_data[SociodemoVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site1_neurocog_PC_list[[i]]    <- PCA(site1_data[NeurocogVar_list$V1], scale.unit = TRUE, graph = FALSE)
  
  site2_culture_PC_list[[i]]    <- PCA(site2_data[CultureEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site2_home_PC_list[[i]]    <- PCA(site2_data[HomeEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site2_school_PC_list[[i]]    <- PCA(site2_data[SchoolEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site2_neighborhood_PC_list[[i]]    <- PCA(site2_data[NeighborhoodEnvVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site2_sociodemo_PC_list[[i]]    <- PCA(site2_data[SociodemoVar_list$V1], scale.unit = TRUE, graph = FALSE)
  site2_neurocog_PC_list[[i]]    <- PCA(site2_data[NeurocogVar_list$V1], scale.unit = TRUE, graph = FALSE)
  
}



#Site one empty lists
site1pc1_home_mediation <- vector("list", nrand)
site1pc1_school_mediation <- vector("list", nrand)
site1pc1_neighborhood_mediation <- vector("list", nrand)
site1pc1_culture_mediation <- vector("list", nrand)

site1pc1_school_demenviro_patha <- vector("list", nrand)
site1pc1_home_demenviro_patha <- vector("list", nrand)
site1pc1_culture_demenviro_patha <- vector("list", nrand)
site1pc1_neighborhood_demenviro_patha <- vector("list", nrand)

site1pc1_school_envirocog_pathb <- vector("list", nrand)
site1pc1_home_envirocog_pathb <- vector("list", nrand)
site1pc1_culture_envirocog_pathb <- vector("list", nrand)
site1pc1_neighborhood_envirocog_pathb <- vector("list", nrand)


site1pc2_home_mediation <- vector("list", nrand)
site1pc2_school_mediation <- vector("list", nrand)
site1pc2_neighborhood_mediation <- vector("list", nrand)
site1pc2_culture_mediation <- vector("list", nrand)

site1pc2_school_demenviro_patha <- vector("list", nrand)
site1pc2_home_demenviro_patha <- vector("list", nrand)
site1pc2_culture_demenviro_patha <- vector("list", nrand)
site1pc2_neighborhood_demenviro_patha <- vector("list", nrand)

site1pc2_school_envirocog_pathb <- vector("list", nrand)
site1pc2_home_envirocog_pathb <- vector("list", nrand)
site1pc2_culture_envirocog_pathb <- vector("list", nrand)
site1pc2_neighborhood_envirocog_pathb <- vector("list", nrand)


site1pc3_home_mediation <- vector("list", nrand)
site1pc3_school_mediation <- vector("list", nrand)
site1pc3_neighborhood_mediation <- vector("list", nrand)
site1pc3_culture_mediation <- vector("list", nrand)

site1pc3_school_demenviro_patha <- vector("list", nrand)
site1pc3_home_demenviro_patha <- vector("list", nrand)
site1pc3_culture_demenviro_patha <- vector("list", nrand)
site1pc3_neighborhood_demenviro_patha <- vector("list", nrand)

site1pc3_school_envirocog_pathb <- vector("list", nrand)
site1pc3_home_envirocog_pathb <- vector("list", nrand)
site1pc3_culture_envirocog_pathb <- vector("list", nrand)
site1pc3_neighborhood_envirocog_pathb <- vector("list", nrand)


#Site 2 empty lists
site2pc1_home_mediation <- vector("list", nrand)
site2pc1_school_mediation <- vector("list", nrand)
site2pc1_neighborhood_mediation <- vector("list", nrand)
site2pc1_culture_mediation <- vector("list", nrand)

site2pc1_school_demenviro_patha <- vector("list", nrand)
site2pc1_home_demenviro_patha <- vector("list", nrand)
site2pc1_culture_demenviro_patha <- vector("list", nrand)
site2pc1_neighborhood_demenviro_patha <- vector("list", nrand)

site2pc1_school_envirocog_pathb <- vector("list", nrand)
site2pc1_home_envirocog_pathb <- vector("list", nrand)
site2pc1_culture_envirocog_pathb <- vector("list", nrand)
site2pc1_neighborhood_envirocog_pathb <- vector("list", nrand)


site2pc2_home_mediation <- vector("list", nrand)
site2pc2_school_mediation <- vector("list", nrand)
site2pc2_neighborhood_mediation <- vector("list", nrand)
site2pc2_culture_mediation <- vector("list", nrand)

site2pc2_school_demenviro_patha <- vector("list", nrand)
site2pc2_home_demenviro_patha <- vector("list", nrand)
site2pc2_culture_demenviro_patha <- vector("list", nrand)
site2pc2_neighborhood_demenviro_patha <- vector("list", nrand)

site2pc2_school_envirocog_pathb <- vector("list", nrand)
site2pc2_home_envirocog_pathb <- vector("list", nrand)
site2pc2_culture_envirocog_pathb <- vector("list", nrand)
site2pc2_neighborhood_envirocog_pathb <- vector("list", nrand)


site2pc3_home_mediation <- vector("list", nrand)
site2pc3_school_mediation <- vector("list", nrand)
site2pc3_neighborhood_mediation <- vector("list", nrand)
site2pc3_culture_mediation <- vector("list", nrand)

site2pc3_school_demenviro_patha <- vector("list", nrand)
site2pc3_home_demenviro_patha <- vector("list", nrand)
site2pc3_culture_demenviro_patha <- vector("list", nrand)
site2pc3_neighborhood_demenviro_patha <- vector("list", nrand)

site2pc3_school_envirocog_pathb <- vector("list", nrand)
site2pc3_home_envirocog_pathb <- vector("list", nrand)
site2pc3_culture_envirocog_pathb <- vector("list", nrand)
site2pc3_neighborhood_envirocog_pathb <- vector("list", nrand)


for (i in 1:nrand){
  #### Site Group 1 Mediations ####  
  site1_data <- abcd_data_final %>%
    filter(abcd_site %in% site_splits_rand[,i]) %>%
    mutate(culture_PC1 = site1_culture_PC_list[[i]]$ind$coord[,1],
           home_PC1 = site1_home_PC_list[[i]]$ind$coord[,1],
           school_PC1 = site1_school_PC_list[[i]]$ind$coord[,1],
           neighborhood_PC1 = site1_neighborhood_PC_list[[i]]$ind$coord[,1],
           sociodemo_PC1 = site1_sociodemo_PC_list[[i]]$ind$coord[,1],
           neurocog_PC1 = site1_neurocog_PC_list[[i]]$ind$coord[,1],
           neurocog_PC2 = site1_neurocog_PC_list[[i]]$ind$coord[,2],
           neurocog_PC3 = site1_neurocog_PC_list[[i]]$ind$coord[,3])
  
  #Site group 1: school neurocog dim 1 
  site1pc1_school_demenviro_patha <- lm(school_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc1_school_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + school_PC1, data = site1_data)
  site1pc1_school_mediation[[i]] <- mediate(site1pc1_school_demenviro_patha, site1pc1_school_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'school_PC1',
                                            boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: school neurocog dim 2 
  site1pc2_school_demenviro_patha <- lm(school_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc2_school_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + school_PC1, data = site1_data)
  site1pc2_school_mediation[[i]] <- mediate(site1pc2_school_demenviro_patha, site1pc2_school_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'school_PC1',
                                            boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: school neurocog dim 3 
  site1pc3_school_demenviro_patha <- lm(school_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc3_school_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + school_PC1, data = site1_data)
  site1pc3_school_mediation[[i]] <- mediate(site1pc3_school_demenviro_patha, site1pc3_school_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'school_PC1',
                                            boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: home neurocog dim 1 
  site1pc1_home_demenviro_patha <- lm(home_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc1_home_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + home_PC1, data = site1_data)
  site1pc1_home_mediation[[1]] <- mediate(site1pc1_home_demenviro_patha, site1pc1_home_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'home_PC1',
                                          boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: home neurocog dim 2 
  site1pc2_home_demenviro_patha <- lm(home_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc2_home_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + home_PC1, data = site1_data)
  site1pc2_home_mediation[[i]] <- mediate(site1pc2_home_demenviro_patha, site1pc2_home_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'home_PC1',
                                          boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: home neurocog dim 3 
  site1pc3_home_demenviro_patha <- lm(home_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc3_home_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + home_PC1, data = site1_data)
  site1pc3_home_mediation[[i]] <- mediate(site1pc3_home_demenviro_patha, site1pc3_home_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'home_PC1',
                                          boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: culture neurocog dim 1 
  site1pc1_culture_demenviro_patha <- lm(culture_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc1_culture_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + culture_PC1, data = site1_data)
  site1pc1_culture_mediation[[i]] <- mediate(site1pc1_culture_demenviro_patha, site1pc1_culture_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'culture_PC1',
                                             boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: culture neurocog dim 2
  site1pc2_culture_demenviro_patha <- lm(culture_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc2_culture_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + culture_PC1, data = site1_data)
  site1pc2_culture_mediation[[i]] <- mediate(site1pc2_culture_demenviro_patha, site1pc2_culture_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'culture_PC1',
                                             boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: culture neurocog dim 3
  site1pc3_culture_demenviro_patha <- lm(culture_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc3_culture_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + culture_PC1, data = site1_data)
  site1pc3_culture_mediation[[i]] <- mediate(site1pc3_culture_demenviro_patha, site1pc3_culture_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'culture_PC1',
                                             boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: neighborhood neurocog dim 1 
  site1pc1_neighborhood_demenviro_patha <- lm(neighborhood_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc1_neighborhood_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + neighborhood_PC1, data = site1_data)
  site1pc1_neighborhood_mediation[[i]] <- mediate(site1pc1_neighborhood_demenviro_patha, site1pc1_neighborhood_envirocog_pathb, treat = 'sociodemo_PC1', 
                                                  mediator = 'neighborhood_PC1', boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: neighborhood neurocog dim 2
  site1pc2_neighborhood_demenviro_patha <- lm(neighborhood_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc2_neighborhood_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + neighborhood_PC1, data = site1_data)
  site1pc2_neighborhood_mediation[[i]] <- mediate(site1pc2_neighborhood_demenviro_patha, site1pc2_neighborhood_envirocog_pathb, treat = 'sociodemo_PC1', 
                                                  mediator = 'neighborhood_PC1', boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 1: neighborhood neurocog dim 3
  site1pc3_neighborhood_demenviro_patha <- lm(neighborhood_PC1 ~ sociodemo_PC1, data = site1_data)
  site1pc3_neighborhood_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + neighborhood_PC1, data = site1_data)
  site1pc3_neighborhood_mediation[[i]] <- mediate(site1pc3_neighborhood_demenviro_patha, site1pc3_neighborhood_envirocog_pathb, treat = 'sociodemo_PC1', 
                                                  mediator = 'neighborhood_PC1', boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  
  #### Site Group 2 Mediations ####  
  site2_data <- abcd_data_final %>%
    filter(abcd_site %nin% site_splits_rand[,i]) %>%
    mutate(culture_PC1 = site2_culture_PC_list[[i]]$ind$coord[,1],
           home_PC1 = site2_home_PC_list[[i]]$ind$coord[,1],
           school_PC1 = site2_school_PC_list[[i]]$ind$coord[,1],
           neighborhood_PC1 = site2_neighborhood_PC_list[[i]]$ind$coord[,1],
           sociodemo_PC1 = site2_sociodemo_PC_list[[i]]$ind$coord[,1],
           neurocog_PC1 = site2_neurocog_PC_list[[i]]$ind$coord[,1],
           neurocog_PC2 = site2_neurocog_PC_list[[i]]$ind$coord[,2],
           neurocog_PC3 = site2_neurocog_PC_list[[i]]$ind$coord[,3])
  
  #Site group 2: school neurocog dim 1 
  site2pc1_school_demenviro_patha <- lm(school_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc1_school_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + school_PC1, data = site2_data)
  site2pc1_school_mediation[[i]] <- mediate(site2pc1_school_demenviro_patha, site2pc1_school_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'school_PC1',
                                            boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: school neurocog dim 2 
  site2pc2_school_demenviro_patha <- lm(school_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc2_school_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + school_PC1, data = site2_data)
  site2pc2_school_mediation[[i]] <- mediate(site2pc2_school_demenviro_patha, site2pc2_school_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'school_PC1',
                                            boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: school neurocog dim 3 
  site2pc3_school_demenviro_patha <- lm(school_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc3_school_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + school_PC1, data = site2_data)
  site2pc3_school_mediation[[i]] <- mediate(site2pc3_school_demenviro_patha, site2pc3_school_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'school_PC1',
                                            boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: home neurocog dim 1 
  site2pc1_home_demenviro_patha <- lm(home_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc1_home_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + home_PC1, data = site2_data)
  site2pc1_home_mediation[[1]] <- mediate(site2pc1_home_demenviro_patha, site2pc1_home_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'home_PC1',
                                          boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: home neurocog dim 2 
  site2pc2_home_demenviro_patha <- lm(home_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc2_home_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + home_PC1, data = site2_data)
  site2pc2_home_mediation[[i]] <- mediate(site2pc2_home_demenviro_patha, site2pc2_home_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'home_PC1',
                                          boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: home neurocog dim 3 
  site2pc3_home_demenviro_patha <- lm(home_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc3_home_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + home_PC1, data = site2_data)
  site2pc3_home_mediation[[i]] <- mediate(site2pc3_home_demenviro_patha, site2pc3_home_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'home_PC1',
                                          boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: culture neurocog dim 1 
  site2pc1_culture_demenviro_patha <- lm(culture_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc1_culture_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + culture_PC1, data = site2_data)
  site2pc1_culture_mediation[[i]] <- mediate(site2pc1_culture_demenviro_patha, site2pc1_culture_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'culture_PC1',
                                             boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: culture neurocog dim 2
  site2pc2_culture_demenviro_patha <- lm(culture_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc2_culture_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + culture_PC1, data = site2_data)
  site2pc2_culture_mediation[[i]] <- mediate(site2pc2_culture_demenviro_patha, site2pc2_culture_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'culture_PC1',
                                             boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: culture neurocog dim 3
  site2pc3_culture_demenviro_patha <- lm(culture_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc3_culture_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + culture_PC1, data = site2_data)
  site2pc3_culture_mediation[[i]] <- mediate(site2pc3_culture_demenviro_patha, site2pc3_culture_envirocog_pathb, treat = 'sociodemo_PC1', mediator = 'culture_PC1',
                                             boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: neighborhood neurocog dim 1 
  site2pc1_neighborhood_demenviro_patha <- lm(neighborhood_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc1_neighborhood_envirocog_pathb <- lm(Neurocog_BPPC1_GenAbility ~ sociodemo_PC1 + neighborhood_PC1, data = site2_data)
  site2pc1_neighborhood_mediation[[i]] <- mediate(site2pc1_neighborhood_demenviro_patha, site2pc1_neighborhood_envirocog_pathb, treat = 'sociodemo_PC1', 
                                                  mediator = 'neighborhood_PC1', boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: neighborhood neurocog dim 2
  site2pc2_neighborhood_demenviro_patha <- lm(neighborhood_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc2_neighborhood_envirocog_pathb <- lm(Neurocog_BPPC2_ExecFunc ~ sociodemo_PC1 + neighborhood_PC1, data = site2_data)
  site2pc2_neighborhood_mediation[[i]] <- mediate(site2pc2_neighborhood_demenviro_patha, site2pc2_neighborhood_envirocog_pathb, treat = 'sociodemo_PC1', 
                                                  mediator = 'neighborhood_PC1', boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  #Site group 2: neighborhood neurocog dim 3
  site2pc3_neighborhood_demenviro_patha <- lm(neighborhood_PC1 ~ sociodemo_PC1, data = site2_data)
  site2pc3_neighborhood_envirocog_pathb <- lm(Neurocog_BPPC3_LearnMem ~ sociodemo_PC1 + neighborhood_PC1, data = site2_data)
  site2pc3_neighborhood_mediation[[i]] <- mediate(site2pc3_neighborhood_demenviro_patha, site2pc3_neighborhood_envirocog_pathb, treat = 'sociodemo_PC1', 
                                                  mediator = 'neighborhood_PC1', boot = TRUE, sims = 5000, boot.ci.type = 'bca')
  
  saveRDS(list = c(site1pc1_school_mediation, site1pc2_school_mediation, site1pc3_school_mediation, site1pc1_home_mediation,
                   site1pc2_home_mediation, site1pc3_home_mediation, site1pc1_culture_mediation, site1pc2_culture_mediation,
                   site1pc3_culture_mediation, site1pc1_neighborhood_mediation, site1pc2_neighborhood_mediation, 
                   site1pc3_neighborhood_mediation, site2pc1_school_mediation, site2pc2_school_mediation, site2pc3_school_mediation, site2pc1_home_mediation,
                   site2pc2_home_mediation, site2pc3_home_mediation, site2pc1_culture_mediation, site2pc2_culture_mediation,
                   site2pc3_culture_mediation, site2pc1_neighborhood_mediation, site2pc2_neighborhood_mediation, 
                   site2pc3_neighborhood_mediation), file = "sitesplit_forwardmodel.RDS")
  
  saveRDS(list = c(site1pc1_school_demenviro_patha, site1pc2_school_demenviro_patha, site1pc3_school_demenviro_patha,
                   site1pc1_home_demenviro_patha, site1pc2_home_demenviro_patha, site1pc3_home_demenviro_patha,
                   site1pc1_culture_demenviro_patha, site1pc2_culture_demenviro_patha, site1pc3_culture_demenviro_patha,
                   site1pc1_neighborhood_demenviro_patha, site1pc2_neighborhood_demenviro_patha, site1pc3_neighborhood_demenviro_patha,
                   site2pc1_school_demenviro_patha, site2pc2_school_demenviro_patha, site2pc3_school_demenviro_patha,
                   site2pc1_home_demenviro_patha, site2pc2_home_demenviro_patha, site2pc3_home_demenviro_patha,
                   site2pc1_culture_demenviro_patha, site2pc2_culture_demenviro_patha, site2pc3_culture_demenviro_patha,
                   site2pc1_neighborhood_demenviro_patha, site2pc2_neighborhood_demenviro_patha, site2pc3_neighborhood_demenviro_patha), 
          file = "sitesplit_forwardmodel_patha.RDS")
  
  save(list = c(site1pc1_school_envirocog_pathb, site1pc2_school_envirocog_pathb, site1pc3_school_envirocog_pathb,
                site1pc1_home_envirocog_pathb, site1pc2_home_envirocog_pathb, site1pc3_home_envirocog_pathb,
                site1pc1_culture_envirocog_pathb, site1pc2_culture_envirocog_pathb, site1pc2_culture_envirocog_pathb,
                site1pc1_neighborhood_envirocog_pathb, site1pc2_neighborhood_envirocog_pathb, site1pc3_neighborhood_envirocog_pathb,
                site2pc1_school_envirocog_pathb, site2pc2_school_envirocog_pathb, site2pc3_school_envirocog_pathb,
                site2pc1_home_envirocog_pathb, site2pc2_home_envirocog_pathb, site2pc3_home_envirocog_pathb,
                site2pc1_culture_envirocog_pathb, site2pc2_culture_envirocog_pathb, site2pc2_culture_envirocog_pathb,
                site2pc1_neighborhood_envirocog_pathb, site2pc2_neighborhood_envirocog_pathb, site2pc3_neighborhood_envirocog_pathb), 
       file = "sitesplit_forwardmodel_pathb.RDS")
  
  rm(site1_data, site2_data) 
  
}
