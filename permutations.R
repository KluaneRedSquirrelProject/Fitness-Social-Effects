
library(tidyverse)
library(data.table)
library(lme4)
library(spatsoc)
library(Rmisc)

#load data
load("./data/Social_Fitness_Data.RData")

## convert to data.table object 
setDT(census_final)

## observed model
mod_obs3 <-glmer(survived ~ 
                         age + 
                         I(age^2) + 
                         grid + 
                         std_soc_surv3 + 
                         mast +
                         mast * std_soc_surv3 +
                         (std_soc_surv3||year) + 
                         (1|squirrel_id), 
                       data=census_final, 
                       family=binomial, 
                       na.action=na.exclude, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs3)

## observed model
mod_obs2 <-glmer(survived ~ 
                   age + 
                   I(age^2) + 
                   grid + 
                   std_soc_surv2 + 
                   mast +
                   mast * std_soc_surv2 +
                   (std_soc_surv2||year) + 
                   (1|squirrel_id), 
                 data=census_final, 
                 family=binomial, 
                 na.action=na.exclude, 
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs2)

## observed model
mod_obs1 <-glmer(survived ~ 
                   age + 
                   I(age^2) + 
                   grid + 
                   std_soc_surv + 
                   mast +
                   mast * std_soc_surv +
                   (std_soc_surv||year) + 
                   (1|squirrel_id), 
                 data=census_final, 
                 family=binomial, 
                 na.action=na.exclude, 
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs1)

## observed model (survival coded as -0.5/0.5)
mod_obs_repro <-glmer(all_litters_fit ~ 
                  age + 
                  I(age^2) + 
                  grid + 
                  std_soc_surv3 + 
                  std_soc_repro +
                  mast +
                  mast * std_soc_surv3 +
                  mast * std_soc_repro +
                  (std_soc_surv3+std_soc_repro||year) + 
                  (1|squirrel_id), 
                data=census_final, 
                family=poisson, 
                na.action=na.exclude, 
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs_repro)

## observed model (survival coded as -1/0)
mod_obs_repro2 <-glmer(all_litters_fit ~ 
                        age + 
                        I(age^2) + 
                        grid + 
                        std_soc_surv2 + 
                        std_soc_repro +
                        mast +
                        mast * std_soc_surv2 +
                        mast * std_soc_repro +
                        (std_soc_surv2+std_soc_repro||year) + 
                        (1|squirrel_id), 
                      data=census_final, 
                      family=poisson, 
                      na.action=na.exclude, 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs_repro2)

## observed model (survival coded as 1/0)
mod_obs_repro1 <-glmer(all_litters_fit ~ 
                         age + 
                         I(age^2) + 
                         grid + 
                         std_soc_surv + 
                         std_soc_repro +
                         mast +
                         mast * std_soc_surv +
                         mast * std_soc_repro +
                         (std_soc_surv+std_soc_repro||year) + 
                         (1|squirrel_id), 
                       data=census_final, 
                       family=poisson, 
                       na.action=na.exclude, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(mod_obs_repro1)

################################
## Calculating social effects ##
################################

## load function
source("functions/get_social_perm.R")


## prep data for get_social_perm function 
census_final2 <- census_final

census_final2$gr_year <- as.factor(paste(census_final2$grid, census_final2$year, sep = "_"))
## remove all of the "observed" social survival/repro values
census_final2 <- census_final2[, c("social_survival", 
                                   "social_survival2",
                                   "social_survival3",
                                   "social_repro", 
                                   "std_soc_repro", 
                                   "std_soc_surv", 
                                   "std_soc_surv2",
                                   "std_soc_surv3") := NULL]

## calculate all pairwise combinations of distance 
df_nn <- edge_dist(census_final2, id = "squirrel_id", coords = c("locx", "locy"), 
                   timegroup = NULL, threshold = 10000, returnDist = T, 
                   splitBy = c("grid", "year"))

## convert locx/locy distance to metres
df_nn$distance <- df_nn$distance*30

## check histogram of distance 
hist(df_nn$distance)

## remove any distances to between self
df_nn <- df_nn[!c(ID1 == ID2)]

## number of permutations (Note, running 100 permutations will take awhile)
perms <- 100

## blank output file
out <- c()

## run the loop over the glmer
for (i in 1:perms){
  
  ## generate 
  soc_rdm <- get_social_perm(data1 = census_final2, 
                              n = length(census_final2$squirrel_id),
                              dist = d_distance,
                              nn = df_nn)

  soc_rdm$iter <- i
  
  out[[i]] <- soc_rdm
  
}

## convert list to DT  
perm_out <- rbindlist(out)

## save file 
saveRDS(perm_out, "output/social-perms-100.RDS")

## read permutation file back in
perm_out <- readRDS("output/social-perms-100.RDS")

## scale social survival and social repro values by grid, year, iter
perm_out[, perm_std_soc_surv1 := scale(social_survival), by = c("grid", "year", "iter")]
perm_out[, perm_std_soc_surv2 := scale(social_survival2), by = c("grid", "year", "iter")]
perm_out[, perm_std_soc_surv3 := scale(social_survival3), by = c("grid", "year", "iter")]
perm_out[, perm_std_soc_repro := scale(social_repro), by = c("grid", "year", "iter")]

## run survival 3 (-0.5/0.5)  
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
                             (perm_std_soc_surv3||year) + 
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


## run survival 1 (0/1)
mod_out <- c()
for (i in 1:length(unique(perm_out$iter))){
  
  rdm <- perm_out[iter == i]
  
  ## run models
  mod_perm_soc_surv <-glmer(survived ~ 
                              age + 
                              I(age^2) + 
                              grid + 
                              perm_std_soc_surv1 + 
                              mast +
                              mast * perm_std_soc_surv1 +
                              (perm_std_soc_surv1||year) + 
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
saveRDS(mod_out, "output/model_soc1.RDS")


## run survival 2 (0/-1)
mod_out <- c()
for (i in 1:length(unique(perm_out$iter))){
  
  rdm <- perm_out[iter == i]
  
  ## run models
  mod_perm_soc_surv <-glmer(survived ~ 
                              age + 
                              I(age^2) + 
                              grid + 
                              perm_std_soc_surv2 + 
                              mast +
                              mast * perm_std_soc_surv2 +
                              (perm_std_soc_surv2||year) + 
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
mod_out2 <- rbindlist(mod_out)
saveRDS(mod_out2, "output/model_soc2.RDS")


## load permtation models that use social_survival3
mod_out_surv <- readRDS("output/model_soc3.RDS")

## pull average coefficients from permuted models
model_surv3 <- data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                        "mast", "mast_perm_soc_surv"),
           avg = c(mean(mod_out_surv$intercept),  
                   mean(mod_out_surv$age), 
                   mean(mod_out_surv$age2), 
                   mean(mod_out_surv$grid),  
                   mean(mod_out_surv$perm_std_soc_surv), 
                   mean(mod_out_surv$mast), 
                   mean(mod_out_surv$mast_perm_std_soc_surv)),
           lwrCI = c(CI(mod_out_surv$intercept)[3], 
                     CI(mod_out_surv$age)[3], 
                     CI(mod_out_surv$age2)[3],
                     CI(mod_out_surv$grid)[3], 
                     CI(mod_out_surv$perm_std_soc_surv)[3], 
                     CI(mod_out_surv$mast)[3], 
                     CI(mod_out_surv$mast_perm_std_soc_surv)[3]), 
           uprCI = c(
             CI(mod_out_surv$intercept)[1], 
             CI(mod_out_surv$age)[1], 
             CI(mod_out_surv$age2)[1],
             CI(mod_out_surv$grid)[1], 
             CI(mod_out_surv$perm_std_soc_surv)[1], 
             CI(mod_out_surv$mast)[1], 
             CI(mod_out_surv$mast_perm_std_soc_surv)[1]))

model_surv3

## load permtation models that use social_survival2
mod_out_sur2 <- readRDS("output/model_soc2.RDS")

## pull average coefficients from permuted models
model_surv2 <- data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                                       "mast", "mast_perm_soc_surv"),
                          avg = c(mean(mod_out_sur2$intercept),  
                                  mean(mod_out_sur2$age), 
                                  mean(mod_out_sur2$age2), 
                                  mean(mod_out_sur2$grid),  
                                  mean(mod_out_sur2$perm_std_soc_surv), 
                                  mean(mod_out_sur2$mast), 
                                  mean(mod_out_sur2$mast_perm_std_soc_surv)),
                          lwrCI = c(CI(mod_out_sur2$intercept)[3], 
                                    CI(mod_out_sur2$age)[3], 
                                    CI(mod_out_sur2$age2)[3],
                                    CI(mod_out_sur2$grid)[3], 
                                    CI(mod_out_sur2$perm_std_soc_surv)[3], 
                                    CI(mod_out_sur2$mast)[3], 
                                    CI(mod_out_sur2$mast_perm_std_soc_surv)[3]), 
                          uprCI = c(
                            CI(mod_out_sur2$intercept)[1], 
                            CI(mod_out_sur2$age)[1], 
                            CI(mod_out_sur2$age2)[1],
                            CI(mod_out_sur2$grid)[1], 
                            CI(mod_out_sur2$perm_std_soc_surv)[1], 
                            CI(mod_out_sur2$mast)[1], 
                            CI(mod_out_sur2$mast_perm_std_soc_surv)[1]))

model_surv2

## load permtation models that use social_survival1
mod_out_sur1 <- readRDS("output/model_soc1.RDS")

## pull average coefficients from permuted models
model_surv1 <- data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                                       "mast", "mast_perm_soc_surv"),
                          avg = c(mean(mod_out_sur1$intercept),  
                                  mean(mod_out_sur1$age), 
                                  mean(mod_out_sur1$age2), 
                                  mean(mod_out_sur1$grid),  
                                  mean(mod_out_sur1$perm_std_soc_surv), 
                                  mean(mod_out_sur1$mast), 
                                  mean(mod_out_sur1$mast_perm_std_soc_surv)),
                          lwrCI = c(CI(mod_out_sur1$intercept)[3], 
                                    CI(mod_out_sur1$age)[3], 
                                    CI(mod_out_sur1$age2)[3],
                                    CI(mod_out_sur1$grid)[3], 
                                    CI(mod_out_sur1$perm_std_soc_surv)[3], 
                                    CI(mod_out_sur1$mast)[3], 
                                    CI(mod_out_sur1$mast_perm_std_soc_surv)[3]), 
                          uprCI = c(
                            CI(mod_out_sur1$intercept)[1], 
                            CI(mod_out_sur1$age)[1], 
                            CI(mod_out_sur1$age2)[1],
                            CI(mod_out_sur1$grid)[1], 
                            CI(mod_out_sur1$perm_std_soc_surv)[1], 
                            CI(mod_out_sur1$mast)[1], 
                            CI(mod_out_sur1$mast_perm_std_soc_surv)[1]))

model_surv1


## run model 
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
                              (perm_std_soc_surv3+perm_std_soc_repro||year) + 
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
saveRDS(mod_out2, "output/model_repro3.RDS")


## run model with soc_surv2
mod_out <- c()
for (i in 1:length(unique(perm_out$iter))){
  
  rdm <- perm_out[iter == i]
  
  ## run models
  mod_perm_soc_repro <-glmer(all_litters_fit ~ 
                               age + 
                               I(age^2) + 
                               grid + 
                               perm_std_soc_surv2 + 
                               perm_std_soc_repro + 
                               mast +
                               mast * perm_std_soc_surv2 +
                               mast * perm_std_soc_repro +
                               (perm_std_soc_surv2+perm_std_soc_repro||year) + 
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
saveRDS(mod_out2, "output/model_repro2.RDS")


## run model with soc_surv1
mod_out <- c()
for (i in 1:length(unique(perm_out$iter))){
  
  rdm <- perm_out[iter == i]
  
  ## run models
  mod_perm_soc_repro <-glmer(all_litters_fit ~ 
                               age + 
                               I(age^2) + 
                               grid + 
                               perm_std_soc_surv1 + 
                               perm_std_soc_repro + 
                               mast +
                               mast * perm_std_soc_surv1 +
                               mast * perm_std_soc_repro +
                               (perm_std_soc_surv1+perm_std_soc_repro||year) + 
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
saveRDS(mod_out2, "output/model_repro1.RDS")

mod_repro3 <- readRDS("output/model_repro3.RDS")
mod_repro2 <- readRDS("output/model_repro2.RDS")
mod_repro1 <- readRDS("output/model_repro1.RDS")


model_repro3 <- data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                        "perm_soc_repro", "mast", "mast_perm_soc_surv", "mast_perm_soc_repro"),
           avg = c(mean(mod_repro3$intercept),  mean(mod_repro3$age), 
           mean(mod_repro3$age2), mean(mod_repro3$grid),  mean(mod_repro3$perm_std_soc_surv), 
           mean(mod_repro3$perm_std_soc_repro),  mean(mod_repro3$mast), 
           mean(mod_repro3$mast_perm_std_soc_surv), mean(mod_repro3$mast_perm_std_soc_repro)),
           lwrCI = c(CI(mod_repro3$intercept)[3], 
                     CI(mod_repro3$age)[3], 
                     CI(mod_repro3$age2)[3],
                     CI(mod_repro3$grid)[3], 
                     CI(mod_repro3$perm_std_soc_surv)[3], 
                     CI(mod_repro3$perm_std_soc_repro)[3], 
                     CI(mod_repro3$mast)[3], 
                     CI(mod_repro3$mast_perm_std_soc_surv)[3], 
                     CI(mod_repro3$mast_perm_std_soc_repro)[3]),
           uprCI = c(
             CI(mod_repro3$intercept)[1], 
             CI(mod_repro3$age)[1], 
             CI(mod_repro3$age2)[1],
             CI(mod_repro3$grid)[1], 
             CI(mod_repro3$perm_std_soc_surv)[1], 
             CI(mod_repro3$perm_std_soc_repro)[1], 
             CI(mod_repro3$mast)[1], 
             CI(mod_repro3$mast_perm_std_soc_surv)[1], 
             CI(mod_repro3$mast_perm_std_soc_repro)[1]))

model_repro3



model_repro2 <- data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                                        "perm_soc_repro", "mast", "mast_perm_soc_surv", "mast_perm_soc_repro"),
                           avg = c(mean(mod_repro2$intercept),  mean(mod_repro2$age), 
                                   mean(mod_repro2$age2), mean(mod_repro2$grid),  mean(mod_repro2$perm_std_soc_surv), 
                                   mean(mod_repro2$perm_std_soc_repro),  mean(mod_repro2$mast), 
                                   mean(mod_repro2$mast_perm_std_soc_surv), mean(mod_repro2$mast_perm_std_soc_repro)),
                           lwrCI = c(CI(mod_repro2$intercept)[3], 
                                     CI(mod_repro2$age)[3], 
                                     CI(mod_repro2$age2)[3],
                                     CI(mod_repro2$grid)[3], 
                                     CI(mod_repro2$perm_std_soc_surv)[3], 
                                     CI(mod_repro2$perm_std_soc_repro)[3], 
                                     CI(mod_repro2$mast)[3], 
                                     CI(mod_repro2$mast_perm_std_soc_surv)[3], 
                                     CI(mod_repro2$mast_perm_std_soc_repro)[3]),
                           uprCI = c(
                             CI(mod_repro2$intercept)[1], 
                             CI(mod_repro2$age)[1], 
                             CI(mod_repro2$age2)[1],
                             CI(mod_repro2$grid)[1], 
                             CI(mod_repro2$perm_std_soc_surv)[1], 
                             CI(mod_repro2$perm_std_soc_repro)[1], 
                             CI(mod_repro2$mast)[1], 
                             CI(mod_repro2$mast_perm_std_soc_surv)[1], 
                             CI(mod_repro2$mast_perm_std_soc_repro)[1]))

model_repro2



model_repro1 <- data.table(variable = c("intercept", "age", "age2", "grid", "perm_soc_surv", 
                                        "perm_soc_repro", "mast", "mast_perm_soc_surv", "mast_perm_soc_repro"),
                           avg = c(mean(mod_repro1$intercept),  mean(mod_repro1$age), 
                                   mean(mod_repro1$age2), mean(mod_repro1$grid),  mean(mod_repro1$perm_std_soc_surv), 
                                   mean(mod_repro1$perm_std_soc_repro),  mean(mod_repro1$mast), 
                                   mean(mod_repro1$mast_perm_std_soc_surv), mean(mod_repro1$mast_perm_std_soc_repro)),
                           lwrCI = c(CI(mod_repro1$intercept)[3], 
                                     CI(mod_repro1$age)[3], 
                                     CI(mod_repro1$age2)[3],
                                     CI(mod_repro1$grid)[3], 
                                     CI(mod_repro1$perm_std_soc_surv)[3], 
                                     CI(mod_repro1$perm_std_soc_repro)[3], 
                                     CI(mod_repro1$mast)[3], 
                                     CI(mod_repro1$mast_perm_std_soc_surv)[3], 
                                     CI(mod_repro1$mast_perm_std_soc_repro)[3]),
                           uprCI = c(
                             CI(mod_repro1$intercept)[1], 
                             CI(mod_repro1$age)[1], 
                             CI(mod_repro1$age2)[1],
                             CI(mod_repro1$grid)[1], 
                             CI(mod_repro1$perm_std_soc_surv)[1], 
                             CI(mod_repro1$perm_std_soc_repro)[1], 
                             CI(mod_repro1$mast)[1], 
                             CI(mod_repro1$mast_perm_std_soc_surv)[1], 
                             CI(mod_repro1$mast_perm_std_soc_repro)[1]))

model_repro1






## plot figure
ggplot() +
                  geom_histogram(data = mod_out2, aes(mast_perm_std_soc_surv)) +
                  geom_vline(aes(xintercept = fixef(mod_obs_repro)[8]), 
                             color = "red",
                             lwd = 1) +
                  geom_vline(aes(xintercept = CI(mod_out2$mast_perm_std_soc_surv)[1])) +
                  geom_vline(aes(xintercept = CI(mod_out2$mast_perm_std_soc_surv)[3])) +
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


