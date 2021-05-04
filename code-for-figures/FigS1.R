
library(data.table)
library(ggplot)
library(viridis)

census_final_nonmast <- readRDS("output/census_final_nonmast.RDS")


aa <- census_final_nonmast %>% 
  filter(year==2008,
         grid=="SU",
         !is.na(survived)) 

## convert male territories to 2
aa$all_litters_fit[is.na(aa$all_litters_fit)] <- 5


bb <- census_final_nonmast %>% 
  filter(year==2008,
         grid=="KL",
         !is.na(survived)) 
## convert male territories to 2
bbM <- subset(bb, sex == "M") 
bbM$all_litters_fit[is.na(bbM$all_litters_fit)] <- 5
bbM$all_litters_fit[bbM$all_litters_fit == 0] <- 5

cc <- rbind(bbM, subset(bb, sex == "F"))

ggplot(cc) +
  geom_histogram(aes(all_litters_fit, fill = sex))

allDF <- rbind(aa,cc)

png("graphics/FigS1.png", height = 4000, width = 6000, units = "px", res = 600)

ggplot(data = allDF, 
       aes(x=locx, y=locy, color=factor(all_litters_fit))) +
  geom_point(size=4, alpha = 0.75) +
  scale_color_viridis(discrete=TRUE, labels=c("Females (no recruitement)", 
                                              "Females (1 recruitement)", 
                                              "Females (2 recruitements)", 
                                              "Females (3 recruitements)",
                                              "Males (no litter)")) +
  xlim(-7,24) +
  ylim(-1, 24) +
  ylab("") + xlab("") +
  #theme_void() +
  theme(#legend.position = "above",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 10),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_rect(fill="white", color = "black", size = 1),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  facet_wrap(~grid, ncol = 1)


dev.off()
