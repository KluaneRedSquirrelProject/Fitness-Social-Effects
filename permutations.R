
library(tidyverse)
library(data.table)
library(lme4)

#load data
load("./data/Social_Fitness_Data.RData")

## convert to data.table object 
setDT(census_final)

## observed model
mod_obs <-glmer(survived ~ 
                         age + 
                         I(age^2) + 
                         grid + 
                         std_soc_surv2 + 
                         mast +
                         mast * std_soc_surv2 +
                         (1|year) + 
                         (1|squirrel_id), 
                       data=census_final, 
                       family=binomial, 
                       na.action=na.exclude, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))



################################
## Calculating social effects ##
################################

source("functions/get_social.R")

census_final$gr_year <- as.factor(paste(census_final$grid, census_final$year, sep = "_"))
census_final <- census_final[, c("social_survival", "social_repro") := NULL]
yr <- data.table(gr_year = as.character(census_final$gr_year),
                 squirrel_id = as.character(census_final$squirrel_id))


df_nn <- edge_dist(df2, id = "squirrel_id", coords = c("locx", "locy"), 
                   timegroup = NULL, threshold = 10000, returnDist = T, 
                   splitBy = "gr_year")


## number of permutations (Note, running 100 permutations will take awhile)
perms <- 2

## blank output file
out <- c()

## run the loop over the glmer
for (i in 1:perms){
  
  ## generate 
  census_final2 <- get_social(data1 = census_final, 
                              n = length(census_final$squirrel_id),
                              yr = yr,
                              dist = d_distance)
  
  ## randomly re-assign values of std_soc_surv2 within grid-years
  census_final[, perm.social := sample(std_soc_surv2), 
             by = c("grid", "year")] ## this line ensures that sampling occurs for individuals WITHIN grid-year combinations

  ## run models
  mod_perm <-glmer(survived ~ 
                             age + 
                             I(age^2) + 
                             grid + 
                             perm.social + 
                             mast +
                             mast * perm.social +
                             (1|year) + 
                             (1|squirrel_id), 
                           data=census_final, 
                           family=binomial, 
                           na.action=na.exclude, 
                           control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

  # get the fixed effect coefficient you want to test
  coefs <- data.table(intercept = fixef(mod_perm)[1], # intercept
                     age = fixef(mod_perm)[2],    # age
                     age2 = fixef(mod_perm)[3],    # age2
                     grid = fixef(mod_perm)[4], 
                     perm.social = fixef(mod_perm)[5],
                     mast = fixef(mod_perm)[6],
                     mast_perm.social = fixef(mod_perm)[7],
                     iter = i)


  out[[i]] <- coefs
  
}

## the output will be a list, so turn it back into a DT
out <- rbindlist(out)

## plot figure
fig_perm <- ggplot() +
                  geom_histogram(data = out, aes(perm.social)) +
                  geom_vline(aes(xintercept = fixef(mod_obs)[5]), 
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



