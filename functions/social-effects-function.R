


################################
### social effects function ###
################################

get_social <- function(df, n, dist) {
  
  ## df = census_final data frame
  ## n = n in first loop
  ## dist = d_distance
  
  
for (j in 1:n) {
  
  df$social_survival<-NULL
  df$social_repro<-NULL
  
  temp<-subset(df, df$grid==df$grid[j]&df$year==df$year[j]&df$squirrel_id!=df$squirrel_id[j]) # consider only those observations from the same grid and year
  
  temp$distance<-sqrt((30*(temp$locx-df$locx[j]))^2+(30*(temp$locy-df$locy[j]))^2)
  
  n2<-length(temp$squirrel_id)
  
  for (i in 1:n2) {        
    temp$fraction[i]<-length(subset(dist, dist > temp$distance[i]))/length(dist)
  }
  
  temp$surv_frac <- temp$survived*temp$fraction
  temp$surv_frac2 <- temp$survived2*temp$fraction
  temp$repro_frac <- temp$all_litters_fit*temp$fraction
  
  df$social_survival[j]<-sum(temp$surv_frac, na.rm=T)
  df$social_survival2[j]<-sum(temp$surv_frac2, na.rm=T)
  df$social_repro[j]<-sum(temp$repro_frac, na.rm=T)
}

  df

}


