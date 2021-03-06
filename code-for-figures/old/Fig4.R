


library(ggplot2)
library(gridExtra)
library(visreg)
library(lme4)
library(dplyr)

## load Social_Fitness_Data.RData file from file from data/ 

## subset to mast and non-mast years
census_final_nonmast <- subset(census_final, mast == "n")
census_final_mast <- subset(census_final, mast == "y")

## run models
summary(fit5_mast<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv3+std_soc_repro+(1|year)+(1|squirrel_id), 
                         data=census_final_mast, 
                         family=poisson, 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))

summary(fit5_nonmast<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv3+std_soc_repro+(1|year)+(1|squirrel_id), 
                            data=census_final_nonmast,  #subset=!(year==2008&grid=="SU"), 
                            family=poisson, 
                            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))



## FIGURE 4
vis_mast_surv <- visreg(fit5_mast, "std_soc_surv3", xlab="Survival of others", ylab="ARS - partial", xlim=c(-3.5, 3), ylim=c(-3.2, 2))
vis_nonmast_surv <- visreg(fit5_nonmast, "std_soc_surv3", xlab="Survival of others", ylab="ARS - partial", xlim=c(-3.5, 3), ylim=c(-3.2, 2))


fig4a <- ggplot(filter(vis_nonmast_surv$fit), aes(std_soc_surv3, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_nonmast_surv$res),
             aes(std_soc_surv3, visregRes), 
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

fig4b <- ggplot(filter(vis_mast_surv$fit), aes(std_soc_surv3, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_mast_surv$res),
             aes(std_soc_surv3, visregRes), 
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

png("figures/Fig4.png", width = 6000, height = 3000, units = "px", res = 600)
grid.arrange(fig4a,fig4b,nrow = 1)
dev.off()
