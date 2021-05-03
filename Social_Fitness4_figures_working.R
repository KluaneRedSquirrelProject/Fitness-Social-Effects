
# libraries
library (tidyverse)
library (visreg)
library (viridis)
library (lme4)

load("./data/Social_Fitness_Data.RData")


# Histogram of Dispersal Distances
length(d_distance)
min(results$year)
max(results$year)

hist(d_distance, main="Dispersal distance by red squirrels", xlab="Distance (m)", breaks=40,)
text(x=550, y=50, "n = 1012")


## Descriptive Figures
df <- data.frame(x1 = 15.0, x2 = 18.0, y1 = 0.1, y2 = 0.1)

census_final<-census_final %>% 
  mutate(survived_f=as.factor(survived))

fig_2015_surv_dead<-census_final %>% 
  filter(year==2015,
         grid=="KL")%>% 
  ggplot(aes(x=locx, y=locy, color=factor(survived))) +
  geom_point(size=4) +
  scale_color_viridis(discrete=TRUE, labels=c("dead", "survived")) +
  xlim(0,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=12),
    legend.position="right")

png(file = './figures/KL2015_survived_dead.png', width = 500, height = 300, units = "px")
fig_2015_surv_dead
dev.off()

fig_2015<-census_final %>% 
  filter(year==2015,
         grid=="KL")%>% 
  ggplot(aes(x=locx, y=locy)) +
  geom_point(size=4, color = "#404788FF")+
  xlim(0,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=0),
    legend.position=NULL)

png(file = './figures/KL2015.png', width = 500, height = 300, units = "px")
fig_2015
dev.off()

fig_2015_repro<-census_final %>% 
  filter(year==2015,
         grid=="KL")%>% 
  ggplot(aes(x=locx, y=locy, color=all_litters_fit)) +
  geom_point(size=4) +
  scale_color_viridis() +
  xlim(0,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=12),
    legend.position="right")

png(file = './figures/KL2015_repro.png', width = 500, height = 300, units = "px")
fig_2015_repro
dev.off()


fig_2002_repro<-census_final %>% 
  filter(year==2002,
         grid=="KL")%>% 
  ggplot(aes(x=locx, y=locy, color=all_litters_fit)) +
  geom_point(size=4) +
  scale_color_viridis() +
  xlim(-7,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=12),
    legend.position="right")

png(file = './figures/KL2002_repro.png', width = 500, height = 300, units = "px")
fig_2002_repro
dev.off()

fig_2002_surv_dead<-census_final %>% 
  filter(year==2002,
         grid=="KL",
         !is.na(survived))%>% 
  ggplot(aes(x=locx, y=locy, color=factor(survived))) +
  geom_point(size=4) +
  scale_color_viridis(discrete=TRUE, labels=c("dead", "survived")) +
  xlim(-7,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=12),
    legend.position="right")

png(file = './figures/KL2002_survived_dead.png', width = 500, height = 300, units = "px")
fig_2002_surv_dead
dev.off()



##  2002 Figures
census_final %>% 
  filter(year==2002,
         grid=="KL",
         !is.na(survived))%>% 
  ggplot(aes(x=locx, y=locy, color=factor(survived))) +
  geom_point(size=4) +
  scale_color_viridis(discrete=TRUE, labels=c("dead", "survived")) +
  xlim(-7,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=0),
    legend.position="none")

#  2003

census_final %>% 
  filter(year==2003,
         grid=="KL",
         !is.na(survived))%>% 
  ggplot(aes(x=locx, y=locy)) +
  geom_point(size=4, color = "#FDE725FF") +
  xlim(-7,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0))

census_final %>% 
  filter(year==2003,
         grid=="KL",
         !is.na(survived))%>% 
  mutate(byear_c=cut(byear, c(1980, 2001, 2003), labels=c("adult", "recruit")),
         byear_c = relevel(byear_c, ref="recruit")) %>% 
  ggplot(aes(x=locx, y=locy, color=factor(byear_c))) +
  geom_point(size=4) +
  scale_color_viridis(discrete=TRUE) +
  xlim(-7,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.position="none")

#############
## Figures ##
#############
census_final_nonmast<-census_final %>% 
  filter(mast=="n")

census_final_mast<-census_final %>% 
  filter(mast=="y")


### Distance-weighted mortality
fit<-glmer(survived~age+I(age^2)+grid+std_soc_surv2+(1|year)+(1|squirrel_id), data=census_final_nonmast, family=binomial, na.action=na.exclude, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

visreg(fit, "age", ylab="log odds (survival)")

visreg(fit, "std_soc_surv2", xlab="Survival of others", ylab="log odds (survival)", ylim=c(-1, 3), xlim=c(-3.2, 3.2))

### Distance-weighted Survival
fit3<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv2+std_soc_repro+(1|year)+(1|squirrel_id), data=census_final_nonmast, family=poisson, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

fit4<-glmer(all_litters_fit~age+I(age^2)+grid+std_soc_surv2+std_soc_repro+(1|year)+(1|squirrel_id), data=census_final_mast, family=poisson, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

visreg(fit3, "age", ylab="ARS - partial")

visreg(fit3, "std_soc_repro", xlab="ARS of others", ylab="ARS - partial", xlim=c(-2.5, 6), ylim=c(-3.3, 1.7))

visreg(fit4, "std_soc_repro", xlab="ARS of others", ylab="ARS - partial", xlim=c(-2.5, 6), ylim=c(-3.3, 1.7))

visreg(fit3, "std_soc_surv2", xlab="Survival of others", ylab="ARS - partial", xlim=c(-3.5, 3), ylim=c(-3.2, 2))
visreg(fit4, "std_soc_surv2", xlab="Survival of others", ylab="ARS - partial", xlim=c(-3.5, 3), ylim=c(-3.2, 2))



### Reproduction plots
# fit_all
#visreg(fit_all, "age", ylab="ARS - partial")


###
## Supplemental Figure - Unusual value for std_soc_repro  
census_final %>% 
  filter(year==2008,
         grid=="SU",
         !is.na(survived))%>% 
  ggplot(aes(x=locx, y=locy, color=all_litters_fit)) +
  geom_point(size=4) +
  scale_color_viridis() +
  xlim(-7,18)+
  ylim(-1, 21)+
  theme_void() +
  theme(
    axis.text=element_text(size=0),
    axis.title=element_text(size=0), 
    plot.title=element_blank(), 
    legend.title = element_text(size=0),
    legend.text = element_text(size=12),
    legend.position="right")


