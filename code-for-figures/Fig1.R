

library(ggplot2)
library(gridExtra)
library(visreg)
library(lme4)
library(dplyr)

## load "census_final file from file from data/ 

## subset to mast and non-mast years
census_final_nonmast <- subset(census_final, mast == "n")
census_final_mast <- subset(census_final, mast == "y")

## run models
summary(fit_nonmast<-glmer(survived~age+I(age^2)+grid+std_soc_surv3+(1|year)+(1|squirrel_id), 
                           data=census_final_nonmast, 
                           family=binomial, 
                           na.action=na.exclude, 
                           control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))))
summary(fit_mast<-glmer(survived~age+I(age^2)+grid+std_soc_surv3+(1|year)+(1|squirrel_id), 
                        data=census_final_mast, 
                        family=binomial, 
                        na.action=na.exclude, 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))


## FIGURE 2
vis_nonmast <- visreg(fit_nonmast, "std_soc_surv3", xlab="Survival of others", ylab="log odds (survival)", ylim=c(-1, 3), xlim=c(-3.2, 3.2))
vis_mast <- visreg(fit_mast, "std_soc_surv3", xlab="Survival of others", ylab="log odds (survival)", ylim=c(-1, 3), xlim=c(-3.2, 3.2))


fig1a <- ggplot(filter(vis_nonmast$fit), aes(std_soc_surv3, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_nonmast$res),
             aes(std_soc_surv3, visregRes), 
             alpha = 0.8, 
             color = "#d8b365") +
  xlab('Survival of others') +
  ylab('log odds (survival)') + 
  ggtitle('(A) Non-mast years') +
  theme(legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

fig1b <- ggplot(filter(vis_mast$fit), aes(std_soc_surv3, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_mast$res),
             aes(std_soc_surv3, visregRes), 
             alpha = 0.8, 
             color = "#5ab4ac") +
  xlab('Survival of others') +
  ylab('log odds (survival)') + 
  ggtitle('(B) Mast years') +
  theme(legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

png("figures/Fig1.png", width = 6000, height = 3000, units = "px", res = 1000)
grid.arrange(fig1a,fig1b,nrow = 1)
dev.off()
