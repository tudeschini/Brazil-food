source(paste0(sourcedir, "Food_init.R"))

# Comments: Now this can be run sequentially after Food_init.R.

# Creator: NRao, Sept 2016
# This code takes the food consumption data from the Oracle DB (shown below), right now for India,
# and calculates the macro- and micro-nutrient content of each food item and creates a summary by state/urb-rur of totals

#extract tables from Oracle. 
  
en_ints=selectDBdata(COUNTRY, ICP_CODE, MEAN, tables='PRI_E_INT')

#create female and female minor counts and consumption equiv units for later use
# create consumption unit equivalent for hh, using: 1 for male (avg of sedentary and moderate work), 0.77 for females
# and 0.60 for children, both m and f. The source is Nat Nutrition Medical Board, 1981. (Text box 6.1.1)
hh_sum_prep = hh_sum %>%
  mutate(female_adult=hh_size-minor-male_adult,female_minor=minor-male_minor)%>%
  # mutate(cu_eq=(male_adult+female_adult*0.77+minor*0.60))%>%
  mutate(cu_eq=(male_adult*getcu("male_adult")+female_adult*getcu("female_adult")+male_minor*getcu("male_minor")+female_minor*getcu("female_minor"))) %>%
  left_join(states)%>%
  mutate(cluster=paste0(zone,as.character(urban)))

#summary data by cluster, for presentation
hh_dem_cts = hh_sum_prep %>%
  select(cluster,cu_eq, weight)%>%
  group_by(cluster) %>%
  summarise(cu_eq_avg=weighted.mean(cu_eq, weight, na.rm=TRUE))

food_nutrients_grp = food_nutrients %>%
    select(item, food_grp, energy,protein, vita, iron, zinc)


 #calculate avg prices and get energy intensity by cluster     
food_avgs = food_items %>%
 filter(is.finite(qty_tot)) %>%
 mutate(avg_price=val_tot/qty_tot)%>%
 inner_join(hh_sum_prep%>% select(survey, id, hh_size, cluster,weight, urban, inc_grp, region)) %>%
 group_by(cluster, inc_grp, icp_code, item, code) %>%
 summarise_each(.,funs(weighted.mean(., weight=qty_tot,na.rm=T)),avg_price) %>%
 left_join(en_ints%>%filter(country=='IND')%>%select(-country))%>%
  rename(en_int=mean)%>%
 left_join(food_nutrients_grp)

  #calculate total nutrients per food item per person per day based on unit consumption and per unit nutrient content. 
  #Note that nutrient data are always given per 100 g, while the survey data are per kg. 
food_items = food_items %>%
  filter(!is.na(qty_tot)) %>%
   left_join(food_nutrients_grp) %>%  
 # inner_join(hh_sum_prep%>% select(survey, cluster, id, hh_size, weight, urban, as.integer(inc_grp), region, cu_eq)) %>%
  inner_join(hh_sum_prep%>% select(survey, cluster, id, hh_size, weight, urban, inc_grp, region, cu_eq)) %>%
  mutate(energy_tot=qty_tot*energy*10/365/cu_eq, 
         protein_tot=qty_tot*protein*10/365/cu_eq,
         vita_tot=qty_tot*vita*10/365/cu_eq, 
         iron_tot=qty_tot*iron*10/365/cu_eq, 
         zinc_tot=qty_tot*zinc*10/365/cu_eq,
         food_exp_percap=val_tot/cu_eq)

# this gives us total total nutrients by household 
food_total = food_items %>%
  group_by(survey, cluster, id, region, urban, weight, inc_grp, food_grp) %>%
  summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, val_tot) %>%
  dplyr::rename(food_exp=val_tot)

food_total = food_total %>%
  group_by(survey, id) %>%
  summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, food_exp)

# subset those who eat out, and mark those whose eat out share of total exp is >20%.
outeaters = food_items %>%
  filter(., grepl("Cooked|processed food",item)) %>%
  select(survey, id, item, val_tot) %>%
  group_by(survey, id) %>%
  summarise_each(.,funs(sum(., na.rm=TRUE)), val_tot) %>%
  dplyr::rename(eatout_exp=val_tot) %>%
  left_join(food_total%>%select(survey, id,food_exp)) %>%
  mutate(eatout_share=eatout_exp/food_exp) %>%
  ##Don't want to include HH in nutritional stats if they eat out a lot. Include if (expenditure) share is < 20% 
  mutate(eatout_exclude=ifelse(eatout_share>0.2,1,0))


item_anal_cluster=food_items %>%
  left_join(outeaters) %>%
  filter(!eatout_exclude) %>%
  group_by(cluster,inc_grp, item) %>%
  summarise_each(funs(weighted.mean(., w=weight, na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot) %>%
  filter(energy_tot>200| protein_tot>5| iron_tot>2| zinc_tot>1|vita_tot>60)


hh_sum_prep= hh_sum_prep %>%
  left_join(food_total) %>%
  left_join(outeaters) %>%
  #make sure those who don't eat out are not NA, so that they are included in future selections
  transform(eatout_exclude=ifelse(is.na(eatout_exclude),0,eatout_exclude))  %>%
  mutate(energy_tot_weatout=ifelse(is.na(eatout_share),energy_tot, energy_tot/(1-eatout_share)),
   protein_tot_weatout=ifelse(is.na(eatout_share),protein_tot, protein_tot/(1-eatout_share)),
   zinc_tot_weatout=ifelse(is.na(eatout_share),zinc_tot, zinc_tot/(1-eatout_share)),
   iron_tot_weatout=ifelse(is.na(eatout_share),iron_tot, iron_tot/(1-eatout_share)),
   vita_tot_weatout=ifelse(is.na(eatout_share),vita_tot, vita_tot/(1-eatout_share)))

# Varying levels of RDA (to tackle the review comment)
gap_threshold <- 0.05 # 0.33 # 0.1 # RDA% 
hh_sum_prep=hh_sum_prep%>%
  filter(!eatout_exclude) %>%
  mutate(cal_gap_ma=get_gap("male_adult","calorie",energy_tot_weatout),
         cal_gap_fa= get_gap("female_adult","calorie",energy_tot_weatout),
         cal_gap_mm=get_gap("male_minor","calorie",energy_tot_weatout),
         cal_gap_fm=get_gap("female_minor","calorie",energy_tot_weatout))%>%
  mutate(cal_gap=(cal_gap_ma+cal_gap_fa+ cal_gap_mm+cal_gap_fm)/4,
         cal_def=ifelse(cal_gap>gap_threshold,1,0),
         cal_surp=ifelse(cal_gap<0,1,0))%>%
  
  mutate(zinc_gap_ma=get_gap("male_adult","zinc",zinc_tot_weatout),
         zinc_gap_fa= get_gap("female_adult","zinc",zinc_tot_weatout),
         zinc_gap_mm=get_gap("male_minor","zinc",zinc_tot_weatout),
         zinc_gap_fm=get_gap("female_minor","zinc",zinc_tot_weatout))%>%
  mutate(zinc_gap=(zinc_gap_ma+zinc_gap_fa+ zinc_gap_mm+zinc_gap_fm)/4,
         zinc_def=ifelse(zinc_gap>gap_threshold,1,0),
         zinc_surp=ifelse(zinc_gap<0,1,0))%>%
  
  mutate(iron_gap_ma=get_gap("male_adult","iron",iron_tot_weatout),
         iron_gap_fa= get_gap("female_adult","iron",iron_tot_weatout),
         iron_gap_mm=get_gap("male_minor","iron",iron_tot_weatout),
         iron_gap_fm=get_gap("female_minor","iron",iron_tot_weatout))%>%
  mutate(iron_gap=(iron_gap_ma+iron_gap_fa+ iron_gap_mm+iron_gap_fm)/4,
         iron_def=ifelse(iron_gap>gap_threshold,1,0),
         iron_surp=ifelse(iron_gap<0,1,0))%>%
  
  mutate(protein_gap_ma=get_gap("male_adult","protein",protein_tot_weatout),
         protein_gap_fa= get_gap("female_adult","protein",protein_tot_weatout),
         protein_gap_mm=get_gap("male_minor","protein",protein_tot_weatout),
         protein_gap_fm=get_gap("female_minor","protein",protein_tot_weatout))%>%
  mutate(protein_gap=(protein_gap_ma+protein_gap_fa+ protein_gap_mm+protein_gap_fm)/4,
         protein_def=ifelse(protein_gap>gap_threshold,1,0),
         protein_surp=ifelse(protein_gap<0,1,0))%>%
  
  mutate(vita_gap_ma=get_gap("male_adult","vita",vita_tot_weatout),
         vita_gap_fa= get_gap("female_adult","vita",vita_tot_weatout),
         vita_gap_mm=get_gap("male_minor","vita",vita_tot_weatout),
         vita_gap_fm=get_gap("female_minor","vita",vita_tot_weatout))%>%
  mutate(vita_gap=(vita_gap_ma+vita_gap_fa+ vita_gap_mm+vita_gap_fm)/4,
         vita_def=ifelse(vita_gap>gap_threshold,1,0),
         vita_surp=ifelse(vita_gap<0,1,0))


food_energy_byitem = food_items %>%
  left_join(hh_sum_prep%>%select(survey, id,cal_def, protein_def,iron_def,zinc_def,vita_def))%>%
  filter(cal_def==1 | protein_def==1 | iron_def==1 | zinc_def==1 | vita_def==1)%>%
  select(cluster, inc_grp, id, item,  energy_tot)%>%
  spread(item,energy_tot)

food_energy_byitem[is.na(food_energy_byitem)]<-0
# mutate(tot_energy=food_energy_byitem%>%select(-cluster,-id)%>%rowSums(.,na.rm=T))%>%

food_energy_byitem = food_energy_byitem %>%
  select(-id)%>%
  group_by(cluster, inc_grp) %>%
summarise_each(.,funs(mean(., na.rm=TRUE)))%>%
  left_join(hh_dem_cts)
 # filter(is.finite(energy_tot))%>%

food_exp_byitem = food_items %>%
  select(cluster, inc_grp, id, item,  food_exp_percap)%>%
  spread(item,food_exp_percap)

food_exp_byitem[is.na(food_exp_byitem)]<-0
# mutate(tot_energy=food_energy_byitem%>%select(-cluster,-id)%>%rowSums(.,na.rm=T))%>%

food_exp_byitem = food_exp_byitem %>%
  select(-id)%>%
  group_by(cluster, inc_grp) %>%
  summarise_each(.,funs(mean(., na.rm=TRUE)))


#now create representative diets by cluster, weighted average of members
#     food_byclust=food_total%>% 
#     group_by(cluster) %>%
#     summarise_each(.,funs(mean(.,na.rm=TRUE)),energy_tot, protein_tot, iron_tot, zinc_tot, food_exp_percap)
#     
    write.csv(food_avgs,'../food_item_details-inc_95.csv')
    write.csv(food_energy_byitem,'../cluster_diets_energy-inc_95.csv')
    write.csv(food_exp_byitem,'../cluster_diets_exp-inc_95.csv')
    write.csv(item_anal_cluster,'../cluster_item_diagnostics-inc_95.csv')
    # write.csv(food_avgs,'../food_item_details-inc_66.csv')
    # write.csv(food_energy_byitem,'../cluster_diets_energy-inc_66.csv')
    # write.csv(food_exp_byitem,'../cluster_diets_exp-inc_66.csv')
    # write.csv(item_anal_cluster,'../cluster_item_diagnostics-inc_66.csv')
    # write.csv(food_avgs,'../food_item_details-inc_90.csv')
    # write.csv(food_energy_byitem,'../cluster_diets_energy-inc_90.csv')
    # write.csv(food_exp_byitem,'../cluster_diets_exp-inc_90.csv')
    # write.csv(item_anal_cluster,'../cluster_item_diagnostics-inc_90.csv')
    # write.csv(food_avgs,'../food_item_details-inc_100.csv')
    # write.csv(food_energy_byitem,'../cluster_diets_energy-inc_100.csv')
    # write.csv(food_exp_byitem,'../cluster_diets_exp-inc_100.csv')
    # write.csv(item_anal_cluster,'../cluster_item_diagnostics-inc_100.csv')
