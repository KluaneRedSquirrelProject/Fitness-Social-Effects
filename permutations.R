
library(tidyverse)
library(data.table)
library(lme4)
library(spatsoc)

#load data
load("./data/Social_Fitness_Data.RData")

## convert to data.table object 
setDT(census_final)

## observed model
mod_obs <-glmer(survived ~ 
                         age + 
                         I(age^2) + 
                         grid + 
                         std_soc_surv3 + 
                         mast +
                         mast * std_soc_surv3 +
                         (1|year) + 
                         (1|squirrel_id), 
                       data=census_final, 
                       family=binomial, 
                       na.action=na.exclude, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs)


## observed model
mod_obs_repro <-glmer(survived ~ 
                  age + 
                  I(age^2) + 
                  grid + 
                  std_soc_surv3 + 
                  std_soc_repro +
                  mast +
                  mast * std_soc_surv3 +
                  mast * std_soc_repro +
                  (1|year) + 
                  (1|squirrel_id), 
                data=census_final, 
                family=poisson, 
                na.action=na.exclude, 
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs_repro)
################################
## Calculating social effects ##
################################

source("functions/get_social_perm.R")

census_final$gr_year <- as.factor(paste(census_final$grid, census_final$year, sep = "_"))
census_final <- census_final[, c("social_survival", "social_repro", "std_soc_repro", "std_soc_surv", "std_soc_surv2") := NULL]
census_final$locx <- census_final$locx*30
census_final$locy <- census_final$locy*30

yr <- data.table(gr_year = as.character(census_final$gr_year),
                 squirrel_id = as.character(census_final$squirrel_id))


df_nn <- edge_dist(census_final, id = "squirrel_id", coords = c("locx", "locy"), 
                   timegroup = NULL, threshold = 10000, returnDist = T, 
                   splitBy = "gr_year")

## number of permutations (Note, running 100 permutations will take awhile)
perms <- 100

## blank output file
out <- c()
ptm <- proc.time()
## run the loop over the glmer
for (i in 1:perms){
  
  ## generate 
  soc_rdm <- get_social(data1 = census_final, 
                              n = length(census_final$squirrel_id),
                              yr = yr,
                              dist = d_distance,
                              nn = df_nn)

  soc_rdm$perm.social.surv <- soc_rdm$social_survival
  
  soc_rdm$iter <- i
  
  out[[i]] <- soc_rdm
  
}
proc.time() - ptm

  
perm_out <- rbindlist(out)

saveRDS(perm_out, "output/social-perms-100.RDS")

## read permutation file back in

perm_out <- readRDS("output/social-perms-100.RDS")

perm_out[, perm_std_soc_surv1 := scale(social_survival), by = c("grid", "year")]
perm_out[, perm_std_soc_surv2 := scale(social_survival2), by = c("grid", "year")]
perm_out[, perm_std_soc_surv3 := scale(social_survival3), by = c("grid", "year")]
perm_out[, perm_std_soc_repro := scale(social_repro), by = c("grid", "year")]

mod_out <- c()
for (i in 1:length(unique(perm_out$iter))){
  
  rdm <- perm_out[iter == i]

  ## run models
  mod_perm_soc_surv <-glmer(survived ~ 
                             age + 
                             I(age^2) + 
                             grid + 
                             perm_std_soc_surv3 + 
                             mast +
                             mast * perm_std_soc_surv3 +
                             (1|year) + 
                             (1|squirrel_id), 
                           data=rdm, 
                           family=binomial, 
                           na.action=na.exclude, 
                           control=glmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e5)))

  # get the fixed effect coefficient you want to test
  coefs <- data.table(intercept = fixef(mod_perm_soc_surv)[1], # intercept
                     age = fixef(mod_perm_soc_surv)[2],    # age
                     age2 = fixef(mod_perm_soc_surv)[3],    # age2
                     grid = fixef(mod_perm_soc_surv)[4],  
                     perm_std_soc_surv = fixef(mod_perm_soc_surv)[5],
                     mast = fixef(mod_perm_soc_surv)[6],
                     mast_perm_std_soc_surv = fixef(mod_perm_soc_surv)[7],
                     iter = i)


  mod_out[[i]] <- coefs
  
}


## the output will be a list, so turn it back into a DT
mod_out <- rbindlist(mod_out)
saveRDS(mod_out, "output/model_soc3.RDS")

mod_out_surv <- readRDS("output/model_soc3.RDS")


data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                        "mast", "mast_perm_soc_surv"),
           avg = c(mean(mod_out_surv$intercept),  
                   mean(mod_out_surv$age), 
                   mean(mod_out_surv$age2), 
                   mean(mod_out_surv$grid),  
                   mean(mod_out_surv$perm_std_soc_surv), 
                   mean(mod_out_surv$mast), 
                   mean(mod_out_surv$mast_perm_std_soc_surv)),
           lwrCI = c(quantile(mod_out_surv$intercept, c(0.025)), 
                     quantile(mod_out_surv$age, c(0.025)), 
                     quantile(mod_out_surv$age2, c(0.025)),
                     quantile(mod_out_surv$grid, c(0.025)), 
                     quantile(mod_out_surv$perm_std_soc_surv, c(0.025)), 
                     quantile(mod_out_surv$mast, c(0.025)), 
                     quantile(mod_out_surv$mast_perm_std_soc_surv, c(0.025))), 
           uprCI = c(quantile(mod_out_surv$intercept, c(0.975)), 
                     quantile(mod_out_surv$age, c(0.975)), 
                     quantile(mod_out_surv$age2, c(0.975)),
                     quantile(mod_out_surv$grid, c(0.975)), 
                     quantile(mod_out_surv$perm_std_soc_surv, c(0.975)), 
                     quantile(mod_out_surv$mast, c(0.975)), 
                     quantile(mod_out_surv$mast_perm_std_soc_surv, c(0.975))))



mod_out <- c()
for (i in 1:length(unique(perm_out$iter))){
  
  rdm <- perm_out[iter == i]
  
  ## run models
  mod_perm_soc_repro <-glmer(all_litters_fit ~ 
                              age + 
                              I(age^2) + 
                              grid + 
                              perm_std_soc_surv3 + 
                              perm_std_soc_repro + 
                              mast +
                              mast * perm_std_soc_surv3 +
                              mast * perm_std_soc_repro +
                              (1|year) + 
                              (1|squirrel_id), 
                            data=rdm, 
                            family=poisson, 
                            na.action=na.exclude, 
                            control=glmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
  
  # get the fixed effect coefficient you want to test
  coefs <- data.table(intercept = fixef(mod_perm_soc_repro)[1], # intercept
                      age = fixef(mod_perm_soc_repro)[2],    # age
                      age2 = fixef(mod_perm_soc_repro)[3],    # age2
                      grid = fixef(mod_perm_soc_repro)[4],  
                      perm_std_soc_surv = fixef(mod_perm_soc_repro)[5],
                      perm_std_soc_repro = fixef(mod_perm_soc_repro)[6],
                      mast = fixef(mod_perm_soc_repro)[7],
                      mast_perm_std_soc_surv = fixef(mod_perm_soc_repro)[8],
                      mast_perm_std_soc_repro = fixef(mod_perm_soc_repro)[9],
                      iter = i)
  
  
  mod_out[[i]] <- coefs
  
}


## the output will be a list, so turn it back into a DT
mod_out2 <- rbindlist(mod_out)

saveRDS(mod_out2, "output/model_repro.RDS")

data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                        "perm_soc_repro", "mast", "mast_perm_soc_surv", "mast_perm_soc_repro"),
           avg = c(mean(mod_out2$intercept),  mean(mod_out2$age), 
           mean(mod_out2$age2), mean(mod_out2$grid),  mean(mod_out2$perm_std_soc_surv), 
           mean(mod_out2$perm_std_soc_repro),  mean(mod_out2$mast), 
           mean(mod_out2$mast_perm_std_soc_surv), mean(mod_out2$mast_perm_std_soc_repro)),
           lwrCI = c(quantile(mod_out2$intercept, c(0.025)), 
                  quantile(mod_out2$age, c(0.025)), 
                  quantile(mod_out2$age2, c(0.025)),
                  quantile(mod_out2$grid, c(0.025)), 
                  quantile(mod_out2$perm_std_soc_surv, c(0.025)), 
                  quantile(mod_out2$perm_std_soc_repro, c(0.025)), 
                  quantile(mod_out2$mast, c(0.025)), 
                  quantile(mod_out2$mast_perm_std_soc_surv, c(0.025)), 
                  quantile(mod_out2$mast_perm_std_soc_repro, c(0.025))),
           uprCI = c(quantile(mod_out2$intercept, c(0.975)), 
                     quantile(mod_out2$age, c(0.975)), 
                     quantile(mod_out2$age2, c(0.975)),
                     quantile(mod_out2$grid, c(0.975)), 
                     quantile(mod_out2$perm_std_soc_surv, c(0.975)), 
                     quantile(mod_out2$perm_std_soc_repro, c(0.975)), 
                     quantile(mod_out2$mast, c(0.975)), 
                     quantile(mod_out2$mast_perm_std_soc_surv, c(0.975)), 
                     quantile(mod_out2$mast_perm_std_soc_repro, c(0.975))))


## plot figure
ggplot() +
                  geom_histogram(data = mod_out2, aes(mast_perm_std_soc_surv)) +
                  geom_vline(aes(xintercept = fixef(mod_obs_repro)[8]), 
                             color = "red",
                             lwd = 1) +
                  ylab("Frequency") +
                  xlab("Coefficient for the effect of survival of others") +
                  theme(legend.position = c(0.75,0.8),
                        legend.title = element_blank(),
                        legend.key = element_blank(),
                        legend.text = element_text(size = 10),
                        axis.title = element_text(size = 14, color = 'black'),
                        axis.text = element_text(size = 12, color = 'black'),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(), 
                        panel.border = element_rect(colour = "black", fill=NA, size = 1))



