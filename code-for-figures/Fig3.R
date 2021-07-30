



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
summary(fit4_mast<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv3+std_soc_repro+(1|year)+(1|squirrel_id), 
                    data=census_final_mast, 
                    family=poisson, 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))

summary(fit4_nonmast<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv3+std_soc_repro+(1|year)+(1|squirrel_id), 
                         data=census_final_nonmast,  #subset=!(year==2008&grid=="SU"), 
                         family=poisson,  
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))

vis_mast_repro <- visreg(fit4_mast, "std_soc_repro", xlab="ARS of others", ylab="ARS - partial", xlim=c(-2.5, 6), ylim=c(-3.3, 1.7))
vis_nonmast_repro <- visreg(fit4_nonmast, "std_soc_repro", xlab="ARS of others", ylab="ARS - partial") #, xlim=c(-2.5, 6), ylim=c(-3.3, 1.7))


fig3a <- ggplot(filter(vis_nonmast_repro$fit), aes(std_soc_repro, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_nonmast_repro$res),
             aes(std_soc_repro, visregRes), 
             alpha = 0.8, 
             color = "#d8b365") +
  xlab('ARS of others') +
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

fig3b <- ggplot(filter(vis_mast_repro$fit), aes(std_soc_repro, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_point(data = filter(vis_mast_repro$res),
             aes(std_soc_repro, visregRes), 
             alpha = 0.8, 
             color = "#5ab4ac") +
  xlab('ARS of others') +
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

png("figures/Fig3.png", width = 6000, height = 3000, units = "px", res = 600)
grid.arrange(fig3a,fig3b,nrow = 1)
dev.off()
