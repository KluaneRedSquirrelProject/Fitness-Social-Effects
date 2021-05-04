
library(tidyverse)

#results <- readRDS("output/results.RDS")

## Load Social_distance_Data.RData file from data/ 

fig1<-ggplot(d_distances_KLSU) +
  geom_histogram(aes(distance), bins = 40) +
  xlab("Distance (m)") +
  ylab("Frequency") +
  theme(legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

png("figures/Fig1.png", width = 4000, height = 4000, units = "px", res = 600)
print(fig1)
dev.off()




hist(d_distance, main="Dispersal distance by red squirrels", xlab="Distance (m)", breaks=40,)
text(x=550, y=50, "n = 1012")
#abline(v=300)


# Proportion of dispersals beyond 300m
length(subset(d_distance, d_distance >300))/length(d_distance)
