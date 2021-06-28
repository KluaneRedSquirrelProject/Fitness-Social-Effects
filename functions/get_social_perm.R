


################################
### social effects function ###
################################

get_social_perm <- function(data1, n, dist, nn) {
  
  ## data1 = census_final data frame
  ## n = n in first loop
  ## dist = d_distance
  ## nn = vector of distance between all individuals
  
  data1$social_survival <- c()
  data1$social_repro <- c()
  
for (j in 1:n) {
  
  temp<-census_final %>% 
    filter (year == year[j],
            grid == grid[j],
            squirrel_id != squirrel_id[j])
  
  n2<-length(temp$squirrel_id)  
  
  temp2 <- nn %>% 
    filter (year == year[j],
            grid == grid[j])
  
  temp$distance <- sample(temp2$distance, n2) #sqrt((30*(temp$locx - data1$locx[j]))^2 + (30*(temp$locy - data1$locy[j]))^2)
  
  for (i in 1:n2) {        
    temp$fraction[i] <- length(subset(dist, dist > temp$distance[i]))/length(dist)
  }
  
  temp$surv_frac <- temp$survived*temp$fraction
  temp$surv_frac2 <- temp$survived2*temp$fraction
  temp$surv_frac3 <- temp$survived3*temp$fraction
  temp$repro_frac <- temp$all_litters_fit*temp$fraction
  
  data1$social_survival[j]<-sum(temp$surv_frac, na.rm=T)
  data1$social_survival2[j]<-sum(temp$surv_frac2, na.rm=T)
  data1$social_survival3[j]<-sum(temp$surv_frac3, na.rm=T)
  data1$social_repro[j]<-sum(temp$repro_frac, na.rm=T)
}

  data1

}


