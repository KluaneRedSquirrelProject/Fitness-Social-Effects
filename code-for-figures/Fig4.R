


library(ggplot2)
library(gridExtra)
library(visreg)
library(lme4)
library(dplyr)

## load data
census_final_nonmast <- readRDS("output/census_final_nonmast.RDS")
census_final_mast <- readRDS("output/census_final_mast.RDS")

## run models
summary(fit5_mast<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv2+std_soc_repro+(1|year)+(1|squirrel_id), 
                         data=census_final_mast, 
                         family=poisson, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))

summary(fit5_nonmast<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv2+std_soc_repro+(1|year)+(1|squirrel_id), 
                            data=census_final_nonmast,  #subset=!(year==2008&grid=="SU"), 
                            family=poisson, 
                            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))



## FIGURE 4
vis_mast_surv <- visreg(fit5_mast, "std_soc_surv2", xlab="Survival of others", ylab="ARS - partial", xlim=c(-3.5, 3), ylim=c(-3.2, 2))
vis_nonmast_surv <- visreg(fit5_nonmast, "std_soc_surv2", xlab="Survival of others", ylab="ARS - partial", xlim=c(-3.5, 3), ylim=c(-3.2, 2))


png("graphics/Fig4.png", width = 6000, height = 3000, units = "px", res = 600)
aa <- ggplot(filter(vis_nonmast_surv$fit), aes(std_soc_surv2, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_nonmast_surv$res),
             aes(std_soc_surv2, visregRes), 
             alpha = 0.8, 
             color = "#d8b365") +
  xlab('Survival of others') +
  ylab('ARS - partial') + 
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

bb <- ggplot(filter(vis_mast_surv$fit), aes(std_soc_surv2, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_mast_surv$res),
             aes(std_soc_surv2, visregRes), 
             alpha = 0.8, 
             color = "#5ab4ac") +
  xlab('Survival of others') +
  ylab('ARS - partial') + 
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
grid.arrange(aa,bb,nrow = 1)

dev.off()
