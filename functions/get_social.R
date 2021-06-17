


################################
### social effects function ###
################################

get_social <- function(data1, n, yr, dist) {
  
  ## data1 = census_final data frame
  ## n = n in first loop
  ## yr = data.table with list of unique gr_year levels
  ## dist = d_distance
  
  data1$social_survival <- c()
  data1$social_repro <- c()
  
for (j in 1:n) {
  
  k = data.table(yr[j])
  
  temp <- data1[gr_year == k$gr_year] #consider only those observations from the same grid and year
  
  temp$distance <- sqrt((30*(temp$locx - data1$locx[j]))^2 + (30*(temp$locy - data1$locy[j]))^2)
  
  n2<-length(temp$squirrel_id)
  
  for (i in 1:n2) {        
    temp$fraction[i] <- length(subset(dist, dist > temp$distance[i]))/length(dist)
  }
  
  temp$surv_frac <- temp$survived*temp$fraction
  temp$surv_frac2 <- temp$survived2*temp$fraction
  temp$repro_frac <- temp$all_litters_fit*temp$fraction
  
  data1$social_survival[j]<-sum(temp$surv_frac, na.rm=T)
  data1$social_survival2[j]<-sum(temp$surv_frac2, na.rm=T)
  data1$social_repro[j]<-sum(temp$repro_frac, na.rm=T)
}

  data1

}


