####  Data Compilation for McAdam et al. J Heredity paper on social effects on fitness

###############
## Libraries ##
###############
library (plyr)  #Causes conflicts with dplyr
library (tidyverse)
library (krsp)
library (lubridate)
library (RCurl)

############################
## connection to database ##
############################
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

##########################################################
## Run scripts to build data from Github KRSP functions ##
##########################################################
# Lifetime Data
script <- getURL("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/lifetime.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Cone Data
script <- getURL("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/cone_count_summaries.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Density Data
script <- getURL("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/density.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

grids_cone_data <- cones_grids_years %>%
  ungroup() %>% 
  mutate(Year=as.integer(Year)) %>% 
  mutate(cones_t = cone_index_t) %>% 
  mutate(cones_tm1 = cone_index_tm1) %>% 
  mutate (cones_tm1 = ifelse(Year == 2005 & Grid == "AG", 1.045008523, cones_tm1)) %>% 
  #No cone data available for AG in 2004 so this needs to be added - assumed to be equal to cone index on LL
  select (Year, Grid, cones_t, cones_tm1, mast, Exp, EXP_label)

KLSU_cone_data <- yearly_cones_klsu %>%
  select (year=Year, cones_t=cone_index_t, cones_tm1=cone_index_tm1)

fla<-read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/generations3.csv", header=T, stringsAsFactors = FALSE) %>% 
  mutate(squirrel_id=as.factor(squirrel_id)) 

#Density data
KLSU_density <- SUKL_yearly_density %>% 
  select (year, spr_density)


# Flastall
flastall2<- tbl(con, "flastall2") %>% 
  collect() %>% 
  filter(gr %in% c("SU", "KL"),
         !is.na(sex)) %>% 
  dplyr::select(squirrel_id, sex, byear, bcert, datee) %>% 
  mutate (sex=as.factor(sex),
          datee=as.Date(datee))

#################
## Census Data ##
#################
# Census Data
# bring in census/dbaMidden fields
midden <- tbl(con, "dbamidden") %>% 
  filter(!is.null(fate), !is.null(reflo),
         !(fate == 3)) %>% 
  select(squirrel_id, grid, census_date = date,
         census_reflo = reflo, census_locx = locX, census_locy = locY,
         census_fate = fate, 
         # def used to break ties, i.e. which is main midden
         census_def = def) %>% 
  collect()
census <- tbl(con, "census") %>% 
  filter(!is.null(sq_fate), !is.null(reflo),
         !(sq_fate %in% c(3, 4, 5, 7, 9, 11, 12))) %>% 
  select(squirrel_id, grid = gr, census_date,
         census_reflo = reflo, census_locx = locx, census_locy = locy,
         census_fate = sq_fate) %>% 
  collect() %>% 
  # def used to break ties, i.e. which is main midden
  mutate(census_def = -census_fate)
# combine sources
census <- bind_rows(midden, census) %>% 
  # remove duplicates, probably accidentally added twice on data entry
  distinct() %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         census_year = year(census_date) %>% as.integer(),
         census_month = month(census_date) %>% as.integer(),
         census_day = day(census_date) %>% as.integer(),
         # distinguish between sources
         census_source = if_else(census_year <= 2011, "midden", "census"),
         # convert locs to numeric
         census_locx = loc_to_numeric(census_locx),
         census_locy = loc_to_numeric(census_locy))


# identify new middens in census table
midden_all <- tbl(con, "dbamidden") %>% 
  select(grid, census_reflo = reflo, census_date = date) %>% 
  collect()
census_all <- tbl(con, "census") %>% 
  select(grid = gr, census_reflo = reflo, census_date) %>% 
  collect()
mid_new <- bind_rows(midden_all, census_all) %>% 
  mutate(census_year = year(census_date) %>% as.integer()) %>% 
  # list of distinct middens in each year
  distinct(grid, census_reflo, census_year) %>%
  # check for valid grids and reflos
  filter(str_detect(grid, "^[A-Z]{2}$"),
         str_detect(census_reflo, "^-?[A-Z0-9][.]?[0-9]{1,2}[.]?$")) %>% 
  group_by(grid, census_reflo) %>% 
  arrange(grid, census_reflo, census_year) %>% 
  # years since last occurrence of this midden
  mutate(census_previous = census_year - lag(census_year)) %>% 
  ungroup() %>% 
  # new middens are those not see in more than 2 years
  mutate(new_midden = (is.na(census_previous) | census_previous > 2)) %>% 
  select(grid, census_reflo, census_year, census_previous, new_midden)

# bring back in to main census dataset
census <- census %>% 
  left_join(mid_new, by = c("grid", "census_reflo", "census_year")) %>% 
  #select(-grid) %>% 
  # for old midden census, new middens have fate 5 or 6
  mutate(new_midden = if_else(census_source == "midden",
                              census_fate %in% c(5, 6), new_midden)) %>% 
  mutate(new_midden = coalesce(new_midden, FALSE)) %>% 
  select(squirrel_id, grid, date=census_date, reflo=census_reflo, locx=census_locx, locy=census_locy, year=census_year, month=census_month, day=census_day,
         census_fate, census_def, census_source, census_previous, new_midden)

# for each squirrel, find first record
census_first <- results %>% 
  # match on squirrel and birth year
  distinct(squirrel_id) %>% 
  inner_join(census, by = "squirrel_id") %>% 
  # now select earliest record
  group_by(squirrel_id) %>% 
  top_n(-1, date)

# still some squirrels with more than one first midden, so use def field to break ties
census_first <- census_first %>% 
  mutate(census_def=ifelse(census_def==5, -99, census_def)) %>% #temporarily assign census_def equal to 5 to -99
  top_n(1, census_def) %>% 
  # for remaining ~150 duplicates, just choose a midden randomly
  filter(row_number() == 1) %>% 
  mutate(census_def=ifelse(census_def==-99, 5, census_def)) %>%  #return census_def to original value
  ungroup()


##  Need to select only a subset of variables to bring in.
census_first_temp<-census_first %>% 
  select(squirrel_id, census_date=date, census_year=year, census_month=month, census_locx=locx, census_locy=locy, census_source, new_midden)


results <- left_join(results, census_first_temp, by = "squirrel_id")

# add new census-based fields
results <- results %>% 
  mutate(
    # calculate distance between recruited location and natal midden
    distance = 30 * sqrt((locx - census_locx) ^ 2 + (locy - census_locy) ^ 2),
    philopatric = (distance <= 30))

results$fitness_coding <- case_when(
  results$survived_200d == 0 ~ "dead",
  results$philopatric ~ "philo",
  results$new_midden ~ "new",
  # has a midden by august of birth year
  results$census_year == results$year & results$census_month <= 8 ~ "compete",
  # has a midden, but after august census in birth year or in next year
  results$census_year == results$year & results$census_month > 8 ~ "floater",
  (results$census_year - results$year) == 1 & results$census_month <= 5 ~ "floater",
  TRUE ~ "unknown")

#####################################################
## Create Census file with one record per squirrel ##
#####################################################
# The primary midden was picked based on defense in most cases.
# In cases where the primary midden could not be determined in any other way 
# the first midden in the list was arbitrarily picked. 

selected_grids <- c("AG", "CH", "JO", "KL", "LL", "SU")

census_all_temp <- filter(census, grid %in% selected_grids, year>=1989) %>% 
  mutate(grid=factor(grid, levels = c("AG", "CH", "JO", "KL", "LL", "SU")))

selected_grids2 <- c("CH", "JO", "KL", "SU")

suchjokl_core_may_census_temp<-filter(census_all_temp, month==5, day==15,
                                      grid %in% selected_grids2, 
                                      locx>=-0.2, locx<=20.8, 
                                      locy>=-0.2, locy<=20.8)#39.69ha

ag_core_may_census_temp<-filter(census_all_temp, month==5, day==15,
                                grid =="AG", 
                                locx>=-0.2, locx<=20.8, 
                                locy>=-0.2, locy<=23.8) #45.36ha

ll2_core_may_census_temp<-filter(census_all_temp, month==5, day==15,
                                 grid =="LL", 
                                 year>2005, 
                                 locx>=-10.2, locx<=22.8, 
                                 locy>=-0.2, locy<=8.8) #26.73ha

core_may_census_temp<-bind_rows(suchjokl_core_may_census_temp, ag_core_may_census_temp, ll2_core_may_census_temp)

census_unique<-core_may_census_temp %>% 
  # filter by core area?
  filter(!is.na(squirrel_id)) %>% 
  #select(year, grid, squirrel_id) %>% 
  group_by (squirrel_id, year) %>% 
  mutate(census_def=ifelse(census_def==5, -99, census_def)) %>% #temporarily assign census_def equal to 5 to -99
  top_n(1, census_def) %>% 
  # for remaining ~284 duplicates, just choose a midden randomly
  filter(row_number() == 1) %>% 
  mutate(census_def=ifelse(census_def==-99, 5, census_def)) %>%  #return census_def to original value
  ungroup() %>% 
  mutate (date=as_date(date))

# NOTE: Numbers will be different than the 'density' estimates because the numbers here do not include 
# individuals that are not defending a midden.  
# See midden fate=3, census  !(sq_fate %in% c(3, 4, 5, 7, 9, 11, 12))) above.

######################################################
## Filter data to consider only the years 1989-2019 ##
######################################################
# results is for the dispersal distance distribution
results<-results %>% 
  filter(year> 1988,
         year < 2020)

####################################
## Calculate Dispersal Distances ##
####################################

d_distances_KLSU<-results %>% 
  filter(philopatric==0,
         grid%in%c("KL", "SU")) %>% 
  select(distance)

d_distance<-d_distances_KLSU$distance

#############################
## Consider only KL and SU ##
#############################
census_unique_KL_SU<-census_unique %>% 
  filter(grid %in% c("KL", "SU")) 

#########################
## Create Litter Table ##
#########################

litter2<-litter %>%
  filter (year>1991, # census data available since 1991
          year<2020, 
          ln==1,
          grid %in% c("SU", "KL")) %>% 
  mutate (last_year = year-1) %>% 
  droplevels() %>% 
  select(squirrel_id, litter_id, grid, Julian, std_Julian, year, last_year, cones_t, cones_tm1, mast, spr_density, age, litter_fit, all_litters_fit)

#Standardize parturition dates relative to the annual and grid mean
litter2<-ddply(litter2, c("year", "grid"), transform, std_Julian = scale(Julian))

litter_locs<-tbl(con, "litter") %>% 
  dplyr::select(litter_id=id, litter_locx=locx, litter_locy=locy) %>% 
  collect() %>% 
  mutate(litter_locx = loc_to_numeric(litter_locx),
         litter_locy = loc_to_numeric(litter_locy))

litter2<-litter2 %>% 
  left_join(litter_locs, by="litter_id")

litter_temp<-litter2 %>% 
  select(squirrel_id, year, grid, litter_locx, litter_locy, std_Julian, Julian, litter_fit, all_litters_fit) %>% 
  filter(!is.na(litter_locx),
         !is.na(litter_locy)) 


####  Better for all recruits
litter_x <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU")) %>% 
  select(litter_id = id, squirrel_id,
         year = yr, grid, locx, locy)

litter_x<-collect(litter_x)

dam_fit2<-results %>% #Sum of all offspring from that year regardless of litter number
  group_by(mother_id, year) %>% 
  dplyr::summarize(all_litters_fit=sum(survived_200d))

litter_x<-litter_x %>% 
  left_join(dam_fit2, by=c("squirrel_id"="mother_id", "year"))

litter3<-litter_x %>% 
  select(squirrel_id, year, grid, litter_locx=locx, litter_locy=locy, all_litters_fit) %>%
  mutate(litter_date = as.Date(paste(year, 5, 15,sep="-"), "%Y-%m-%d"),
         litter_locx = loc_to_numeric(litter_locx),
         litter_locy = loc_to_numeric(litter_locy),
         all_litters_fit = ifelse(is.na(all_litters_fit), 0, all_litters_fit)) %>% #assumes that females with no juveniles in the results table did not have any survive
  distinct() # makes sure there is only one record per squirrel

###########################
## Join Litter to Census ##
###########################
census_unique_KL_SU<-census_unique_KL_SU %>% 
  filter(year>1991,
         year<2020) %>% 
  full_join(litter3, by=c("squirrel_id", "year", "grid"))

###########################
## Clean up census table ##
###########################
# Merge locations between census and litter
# Exclude missing locations for merged locations
# Assign  all_litter_fit to be zero for females that were not in the litter table (likely missing litter record for nonbreeders).

census_unique_KL_SU<-census_unique_KL_SU %>% 
  mutate(locx=coalesce(locx, litter_locx),
         locy=coalesce(locy, litter_locy),
         date=coalesce(date, litter_date)) %>% 
  filter(!is.na(locx),
         !is.na(locy))

census_unique_KL_SU<-census_unique_KL_SU %>% 
  left_join(flastall2, by="squirrel_id") %>% 
  mutate (survived = ifelse(datee-date>200, 1, 0),
          survived2 = ifelse(datee-date>200, 0, -1),
          all_litters_fit=ifelse(is.na(all_litters_fit)&sex=="F", 0, all_litters_fit),
          grid=as.factor(grid))

census_final<-census_unique_KL_SU %>% 
  select(squirrel_id, grid, locx, locy, year, sex, byear, survived, survived2, all_litters_fit, bcert)

################################
## Calculating social effects ##
################################

source("functions/get_social.R")

census_final$gr_year <- as.factor(paste(census_final$grid, census_final$year, sep = "_"))
yr <- data.table(gr_year = as.character(census_final$gr_year),
                 squirrel_id = as.character(census_final$squirrel_id))

census_final <- get_social(df = census_final, 
                           n = length(census_final$squirrel_id),
                           yr = yr,
                           dist = d_distance)

####################
## Link Mast Data ##
####################
cones_temp<-cones_grids_years %>% 
  select (year=Year, grid=Grid, mast, cone_index_t, cone_index_tm1)

census_final<-census_final %>% 
  left_join(cones_temp, by=c("year", "grid"))

####################################################################
## Calculate age and standardize social effects within grid-years ##
####################################################################
# Also exclude 2020
census_final<-census_final %>% 
  mutate(age=year-byear) %>% 
  filter(year<2020)

#Standardize within grid-years
census_final<-ddply(census_final, c("year", "grid"), transform, std_soc_repro = scale(social_repro))
census_final<-ddply(census_final, c("year", "grid"), transform, std_soc_surv = scale(social_survival))
census_final<-ddply(census_final, c("year", "grid"), transform, std_soc_surv2 = scale(social_survival2))



# exclude 41 observations with missing ages.
# This helps with model diagnostics later because NA for age leads to exclusions of these obs anyway.
census_final<-census_final %>% 
  filter(!is.na(age))


#############
# Save Data #
#############

save.image("./data/Social_Fitness_Data.RData")

