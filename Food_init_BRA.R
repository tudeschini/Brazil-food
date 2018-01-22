###########################################
### Initialization + Import Parameters  ###
###########################################

options(java.parameters = "-Xmx96g")  # Arbitrarily set to 96GB; doesn't seem to matter if too high
source("P:/ene.general/DecentLivingEnergy/Surveys/Generic function to access database.R")

# Working directory is set by .Rprofile when loading this project
workdir <- paste0(getwd(), '/')
# datadir <- "../../../Data/Food-BRA/"
datadir <- "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/Food/Brazil/Mapping collaboration/"
# gamsdir <- "../diet_gms/"

# Keep .gms file from other temporary intermediate files (.gdx and .log) for the sake of OneDrive (trying to synch all the files..)
old_path <- "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/"   # Old diet_gms folder for keeping temp files
interim_path <- "H:/MyDocuments/Analysis/Food/Brazil/GAMS_runfiles/"

#connect to database
require(RColorBrewer)
require(foreign)
require(RJDBC)
require(readxl)
require(Hmisc)
require(rms)
require(doParallel)
require(data.table)
require(plyr)  # Needed by 'caret'; must be loaded before dplyr
require(dplyr)
require(tidyr)
require(caret)
require(gbm)
require(ggplot2)
require(gridExtra)
require(stringr)
require(stringi)
require(grid)
require(scales)
require(readxl)
require(xlsx)


### POF data is not in DLE DB. Instead, it is under the survey drive as raw files.
# setwd("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/")
setwd("H:/MyDocuments/Analysis/Food/Brazil")
path_POF <- "P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Data/"
source("POF_data_read_in.R")










### After this point, it is placeholder (originally from India analysis)
### Don't run for now (22/Jan/2018)





### Indian Dietary Reference Intakes (DRI)
DRI <- read.csv("../DRI-India.csv", header=TRUE)  # India specific
DRI_wide <- reshape(DRI, idvar = "Group", timevar = "Nutrient", v.names="DRI", direction = "wide") %>% 
  mutate_at(vars(contains('DRI')), funs(yr=.*365))


### Consumption units
CU <- read.csv("../cu_eq.csv", header=TRUE)  

### State and zones
states=read.csv("../states.csv")

### Average household size (in CU)
# states <- read.csv(paste0(workdir, "../states.csv"), header=TRUE) %>% rename(State=region)
a <- xlsx::read.xlsx("../NSS_food_diagnostics_VitA.xlsx", 2, startRow = 3 , endRow = 272) %>% rename(region=State) %>%
  left_join(states) %>% select(region, Urban..Rural, Male.Adults, Female.Adults, Male.minors, Female.minors, Total.Pop, zone)
hh_size <- a %>% mutate(cluster=paste0(zone, Urban..Rural)) %>% select(-zone, -Urban..Rural) %>% group_by(cluster) %>% 
  summarise(MA=weighted.mean(Male.Adults,Total.Pop), FA=weighted.mean(Female.Adults,Total.Pop),
            MM=weighted.mean(Male.minors,Total.Pop), FM=weighted.mean(Female.minors,Total.Pop)) 

# Average consumption unit by cluster and by hh member
cu <- data.frame(cluster= hh_size$cluster, as.matrix(hh_size[,-1]) %*% diag(CU$cu_eq))

# Income groups to analyze
inc_grps <- c(1,2,3,4)  # all groups


########################
### From NRao's Init ###
########################

hh_sum=selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, MALE_ADULT, MALE_MINOR, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')
hh_sum=hh_sum%>%
  mutate(exp_percap = expenditure/hh_size)

#create income groups, which is used to join cluster assignments to items
urb_grp=hh_sum %>%
  filter(urban==1) %>%
  filter(is.finite(exp_percap)) %>%
  arrange(exp_percap) %>%
  mutate(inc_grp=cut(exp_percap,breaks=c(0,1.4*365,2.8*365,5.6*365,max(exp_percap)*365),labels=FALSE))

rur_grp=hh_sum %>%
  filter(urban==0) %>%
  filter(is.finite(exp_percap)) %>%
  arrange(exp_percap) %>%
  mutate(inc_grp=cut(exp_percap,breaks=c(0,0.95*365,1.9*365,3.8*365,max(exp_percap)*365),labels=FALSE))

hh_sum<-rbind(urb_grp,rur_grp) 

hh_sum_init = hh_sum %>%
  mutate(female_adult=hh_size-minor-male_adult,female_minor=minor-male_minor)%>%
  mutate(cu_eq_MA=getcu("male_adult")*male_adult, cu_eq_FA=female_adult*getcu("female_adult"), 
         cu_eq_MM=male_minor*getcu("male_minor"), cu_eq_FM=female_minor*getcu("female_minor"))%>%
  left_join(states)%>%
  mutate(cluster=paste0(zone,as.character(urban)))

hh_dem_cts = hh_sum_init %>%
  # select(cluster,cu_eq_MA,cu_eq_FA,cu_eq_MM,cu_eq_FM, weight)%>%
  group_by(cluster, inc_grp) %>%
  summarise(cu_eq_MA_avg=weighted.mean(cu_eq_MA, weight, na.rm=TRUE),
            cu_eq_FA_avg=weighted.mean(cu_eq_FA, weight, na.rm=TRUE),
            cu_eq_MM_avg=weighted.mean(cu_eq_MM, weight, na.rm=TRUE),
            cu_eq_FM_avg=weighted.mean(cu_eq_FM, weight, na.rm=TRUE),
            MA_avg=weighted.mean(male_adult, weight, na.rm=TRUE),
            FA_avg=weighted.mean(female_adult, weight, na.rm=TRUE),
            MM_avg=weighted.mean(male_minor, weight, na.rm=TRUE),
            FM_avg=weighted.mean(female_minor, weight, na.rm=TRUE),
            MA_tot=sum(male_adult*weight, na.rm=TRUE),
            FA_tot=sum(female_adult*weight, na.rm=TRUE),
            MM_tot=sum(male_minor*weight, na.rm=TRUE),
            FM_tot=sum(female_minor*weight, na.rm=TRUE),
            n_hh = sum(weight)) %>%
  filter(inc_grp %in% inc_grps)%>%
  mutate(clsname=paste0(cluster,"_",as.character(inc_grp))) 

hh_size_cls <- hh_dem_cts %>% ungroup(cluster) %>% select(clsname, MA_avg, FA_avg, MM_avg, FM_avg)  # Num persons



########################
### Food information ###
########################


### Price data by food item (By-cluster)

# price is in $/kg (by 8 clusters (region-urbanity))
food_by_cluster <- read.csv("../food_item_details-CU.csv", 
                            header=TRUE, stringsAsFactors = FALSE)[,-1] %>% filter(!is.na(energy))
n_cluster <- length(unique(food_by_cluster$cluster))   # 8 for now
# We run optimization for these 114 items (exclude all-NA nutrition items).
items_to_optimize <- food_by_cluster %>% filter(cluster=="E0") %>% select(item, code) %>% arrange(item)  
price_by_cls <- food_by_cluster %>% select(cluster, code, item, avg_price) %>% 
  spread(key=cluster, value = avg_price, sep='_p') %>% arrange(item) %>% select(-code)
names(price_by_cls)[-1] <- paste0("price_", unique(food_by_cluster$cluster))

# price is in $/kg (by 16 clusters (region-urbanity-income))
food_by_cls_inc <- read.csv("../food_item_details-inc.csv", 
                            header=TRUE, stringsAsFactors = FALSE)[,-1] %>% filter(!is.na(energy)) %>% 
  mutate(cls_inc = paste0(cluster,"_",inc_grp)) %>% filter(inc_grp %in% inc_grps)    # Only top & bottom income group
n_cls_inc <- length(unique(food_by_cls_inc$cls_inc))  # 16 for now
price_by_cls_inc <- food_by_cls_inc %>% select(cls_inc, code, item, avg_price) %>% 
  spread(key=cls_inc, value = avg_price, sep='_p') %>% arrange(item) %>% select(-code)
names(price_by_cls_inc)[-1] <- paste0("price_", unique(food_by_cls_inc$cls_inc))

# How to treat zero or negligible items
# Set price to zero and set cnosumptions to zero
# And turn on the cell in the 'ignore' list
price_by_cls_inc[is.na(price_by_cls_inc)] <- 0   # could be INF but it appears that GAMS does not process Inf


### Define food groups (two tiers)
# 1. 20 or so food groups (less aggregate) by N. Rao
food_group <- food_by_cluster %>%  filter(cluster=="E0") %>% select(item, code, food_grp) %>% right_join(items_to_optimize)
food_group$food_grp[is.na(food_group$food_grp)] <- "etc"

# 2. Five 'wide' food groups
food_wgrp <- openxlsx::read.xlsx("../NSS_food_items-VitA.xlsx", 1) %>% 
  select(item, group=food_grp_wdds) %>% arrange(item) %>% filter(item %in% items_to_optimize$item) %>%
  mutate(groupa=ifelse(is.na(group), "OTH", group)) %>%
  mutate(groupa=ifelse(groupa %in% c("OFV", "VAFV", "DLGV"), "FV", groupa)) %>%
  mutate(groupa=ifelse(groupa %in% c("LNS", "E"), "PRTN", groupa)) %>% select(-group) %>% rename(group=groupa)

# Fix some items (garlic, onion, ginger, radish)
food_wgrp <- food_wgrp %>% mutate(group=replace(group, item %in% c("Ginger", "Onion", "Garlic", "Singara", "Khesari"), "OTH")) %>%
  mutate(group=replace(group, item == "Radish", "FV")) %>%
  mutate(group=replace(group, item == "Groundnut", "PRTN"))

grp_names <- unique(food_wgrp$group)
group_map <- data.frame(food_wgrp, grp=matrix(0, ncol=length(grp_names), nrow=dim(food_wgrp)[1]))
for(i in 1:length(grp_names)) {
  group_map[,2+i] <- as.numeric(group_map$group==grp_names[i])
}
group_map <- group_map %>% select(-group)
names(group_map)[-1] <- grp_names

group_map_pt <- group_map %>% mutate(PRTN = PRTN+MF) %>% mutate(MF=0)
group_map_kh <- group_map_pt %>% mutate_cond(item=="Khesari", PRTN = 1, OTH = 0)


### Nutritional data by food item (By-cluster)

# Main scenario
food_nutrients = xlsx::read.xlsx('../NSS_food_items-VitA.xlsx', sheetName='NSS_food_items_2017', endRow=152)

# Sensitivity with better rice nutrition
# food_nutrients = read_excel(paste0(workdir, NSS_food_items-VitA.xlsx'),
#                                 sheet='NSS_food_items_2017_rice')
valid_column_names <- make.names(names=names(food_nutrients), unique=TRUE, allow_ = TRUE)
names(food_nutrients) <- valid_column_names

food_nutrients = food_nutrients %>% select(item, food_grp, food_grp_wdds, energy, protein, vita, iron, zinc) %>% right_join(food_wgrp) %>%
  mutate_cond(item=="Kharbooza", zinc=0) %>% arrange(item) %>% mutate_if(is.numeric, funs(.*10)) %>%  # nutrients per kg
  select(item, food_grp, food_grp_wdds, energy, protein, iron, zinc, vita)

# All food item-specific information
food_summary <- food_nutrients %>% left_join(food_wgrp) # nutri per kg
n_fooditem <- dim(food_nutrients)[1]   # 114 for now


#calculate avg prices and get energy intensity by cluster
food_items = selectDBdata(SURVEY, ID, ITEM, CODE, UNIT, QTY_TOT, VAL_TOT, tables='IND1_FOOD')
hh_map=selectDBdata(CODE, ICP_CODE, tables='IND1_MAP')

#For food items that are entered by number, alter quantity to kg based on IND_FOOD_AVG_WT look up table
food_items = food_items %>%
  left_join(avg_wt) %>%
  transform(qty_tot=ifelse(is.na(avg_wt), qty_tot, qty_tot*avg_wt))

food_items = food_items %>%
  left_join(hh_map)

food_avgs_national=food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot/qty_tot)%>%
  inner_join(hh_sum_init%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) %>%
  group_by(item, code) %>%
  summarise(avg_price=weighted.mean(avg_price, qty_tot, na.rm=T), qty_tot=sum(weight * qty_tot, na.rm=T) / sum(hh_sum_init$weight)) %>%
  left_join(food_summary)

food_avgs_grp=food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot/qty_tot)%>%
  inner_join(hh_sum_init%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) %>%
  left_join(food_group%>%select(-code)) %>%
  group_by(urban, cluster, inc_grp, food_grp, item) %>%
  summarise_each(.,funs(weighted.mean(., weight=qty_tot, na.rm=T)),avg_price) %>% filter(!is.na(food_grp))

food_price_indiv <- food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot/qty_tot)%>%
  inner_join(hh_sum_init%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region))


###########################
### HH food consumption ###
###########################

### Cluster expenditure import (by 8 cluster)
exp_by_cls_org <- data.frame(t(read.csv("../cluster_diets_exp-CU.csv", 
                                        header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)[,c(-1,-2)])) 
exp_by_cls_org <- exp_by_cls_org %>% mutate(item=row.names(exp_by_cls_org)) %>% select(item, everything()) 
exp_by_cls <- exp_by_cls_org %>% filter(item %in% items_to_optimize$item) 
names(exp_by_cls)[-1] <- paste0("expen_", unique(food_by_cluster$cluster))
exp_by_cls[price_by_cls==0] <- 0

### Cluster expenditure import (by 16 cluster)
exp_by_cls_inc <- data.frame(t(read.csv("../cluster_diets_exp-inc_90.csv",  # For 90 or 100% RDA as threshold for deficiency
                                        header=TRUE, check.names = FALSE, stringsAsFactors = FALSE)[,c(-1:-2)] %>% 
                                 filter(inc_grp %in% inc_grps) %>% select(-1) )) 
exp_by_cls_inc <- exp_by_cls_inc %>% mutate(item=row.names(exp_by_cls_inc)) %>% select(item, everything()) 
exp_by_cls_inc <- exp_by_cls_inc %>% filter(item %in% items_to_optimize$item) 
names(exp_by_cls_inc)[-1] <- paste0("expen_", unique(food_by_cls_inc$cls_inc))
exp_by_cls_inc[price_by_cls_inc==0] <- 0

### kg consumption (by 8 cluster)
kg_by_cls <- exp_by_cls 
kg_by_cls[,-1] <- kg_by_cls[,-1] / select(price_by_cls, contains("price_"))
names(kg_by_cls)[-1] <- paste0("kg_", unique(food_by_cluster$cluster))
kg_by_cls[price_by_cls==0] <- 0

### kg consumption (by 16 cluster)
kg_by_cls_inc <- exp_by_cls_inc
kg_by_cls_inc[,-1] <- kg_by_cls_inc[,-1] / select(price_by_cls_inc, contains("price_"))
names(kg_by_cls_inc)[-1] <- paste0("kg_", unique(food_by_cls_inc$cls_inc))
kg_by_cls_inc[price_by_cls_inc==0] <- 0


### Combine all by-cluster information into one
kg_by_cls <- kg_by_cls %>% left_join(kg_by_cls_inc)
exp_by_cls <- exp_by_cls %>% left_join(exp_by_cls_inc)
# kg_by_cls[is.na(kg_by_cls)] <- 0
# kg_by_cls[is.infinite(kg_by_cls)] <- 0

save(kg_by_cls, file="kg_by_cls_90RDA.Rda")
# save(kg_by_cls, file="kg_by_cls_100RDA.Rda")

cal_base <- kg_by_cls %>% left_join(food_nutrients%>%select(item, energy)) %>% left_join(food_wgrp)
# calcalc <- cal_base %>%
#   mutate_at(vars(starts_with("kg_")), funs(.*energy)) %>% group_by(group) %>% summarise_if(is.numeric, sum) 

# calorie share by food group
# calshare <- cal_base %>% 
#   mutate(kcal=energy*kg_E0) %>% group_by(group) %>%
#   summarise(kcalsum=sum(kcal)) %>% mutate(share=kcalsum/sum(kcalsum))

### Deal with the negligible food items in each diet
# calculate calorie shares within SS-C group
# set shares as percentage integer to ignore <1% items
# If this pct==0, we mark them unavailable in the region.

calsum_grp <- cal_base %>% 
  mutate_at(vars(starts_with("kg")), funs("kcal"=energy*.)) %>% group_by(group) %>% select(item, group, ends_with("kcal")) %>%
  summarise_if(is.numeric, funs(sum=sum)) 
calpct_grp <- cal_base %>% 
  mutate_at(vars(starts_with("kg")), funs("kcal"=energy*.)) %>% group_by(group) %>% select(item, group, ends_with("kcal")) %>% 
  left_join(calsum_grp) %>% arrange(group) 
n_cls <- (dim(calpct_grp)[2]-2)/2
calpct_grp[,3:(n_cls+2)] <- round(calpct_grp[,3:(n_cls+2)] / calpct_grp[,(n_cls+3):length(calpct_grp)] *100)
calpct_grp <- calpct_grp %>% select(-ends_with("sum"))



# Both poor & rich population within the same zone end up with the same 'not available' food items.

a <- data.frame( t(calpct_grp[,-1:-10]) )
a <- cbind(do.call("rbind", lapply(strsplit(substr(row.names(a), 4, 7), ""), '[', -3)), a) 
names(a)[1:3] <- c("state", "urban", "inc")
a <- a %>% group_by(state, urban) %>% summarise_all(mean)
a <- a[rep(seq(1,dim(a)[1]), each=4),]
a <- data.frame(t(a[,-1:-3]))
names(a) <- names(calpct_grp[,-1:-10])
calpct_grp[,-1:-10] <- a


calpct_grp <- calpct_grp %>% arrange(item) %>% ungroup(group) %>% select(-group)

# Identify those items to ignore in the optimization
idx_nocon <- which(calpct_grp ==0, arr.ind = TRUE)

price_by_cls <- price_by_cls %>% left_join(price_by_cls_inc)
ignore <- price_by_cls
ignore[,-1] <- 0
ignore[idx_nocon] <- 1
names(ignore)[-1] <- c(paste0("ign_", unique(food_by_cluster$cluster)), paste0("ign_", unique(food_by_cls_inc$cls_inc)))
# View(ignore %>% left_join(food_wgrp) %>% select(item, group, everything()))

##########################
### Emission from food ###
##########################

### Non-CO2 EF from Hugo before Feb 2017 (wrong values)
ef_crop_old <- xlsx::read.xlsx(paste0(datadir, 'Emission_factors_ESM.xlsx'), 1, startRow=2,
                               colIndex = c(2,5), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X5)
ef_lvstock_old <- data.frame(xlsx::read.xlsx(paste0(datadir, "Emission_factors_ESM.xlsx"), 2,
                                             colIndex = 8, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                             xlsx::read.xlsx(paste0(datadir, "Emission_factors_ESM.xlsx"), 2,
                                             colIndex = 5, header = FALSE, rowIndex=c(29:34))) %>% rename(item=X8, ef=X5)

### Non-CO2 EF from Hugo after Feb 2017 (corrected values) w/ new fertilizer values
ef_crop <- xlsx::read.xlsx(paste0(datadir, "Em_factors_Narasimha_new_25jan17.xlsx"), 2, startRow=2,
                           colIndex = c(2,10), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X10)
ef_lvstock <- data.frame(xlsx::read.xlsx(paste0(datadir, "Em_factors_Narasimha_new_25jan17.xlsx"), 3,
                                         colIndex = 11, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                         xlsx::read.xlsx(paste0(datadir, "Em_factors_Narasimha_new_25jan17.xlsx"), 3,
                                         colIndex = 6, header = FALSE, rowIndex=c(30:35)))
names(ef_lvstock) <- c("item", "ef")

rice_ch4 <- 1.0 # 0.66  kgCO2e/kg rice
ef_esm <- rbind.fill(ef_crop, ef_lvstock)
ef_esm <- ef_esm %>% mutate(ef = ifelse(grepl("paddy",item, ignore.case = TRUE), ef+rice_ch4, ef)) %>% arrange(item) %>% rename(item_esm=item, ef_nonco2=ef)


### EF mapping with extraction rate and edible rate w/ updated mapping (incorporating )
ef_all <- xlsx::read.xlsx(paste0(datadir, "Emission_factors_mapping.xlsx"), 2, colIndex=c(1:6),
                          stringsAsFactors=FALSE) %>% left_join(ef_esm) 
ef_all <- ef_all %>% mutate(ef_nonco2 = ifelse(grepl("Fish",item), 1.13, ef_nonco2))   # Based on Tilman et al. and 227g/serving (fish) http://www.health.state.mn.us/divs/eh/fish/eating/serving.html
ef_all <- ef_all %>% mutate(ef_per_kg_purchased=ef_nonco2/extraction, 
                            ef_per_kg_eaten=ef_nonco2/extraction/edible) %>% rename(ef_base = ef_nonco2)  # CO2e per edible kg

# write.csv(ef_all, paste0(workdir, "NSS_food_emission_factors.csv"))

food_summary <- food_summary %>% left_join(ef_all %>% select(item, extraction, edible, ef_per_kg_eaten))  # nutri per kg
price_by_cls_ed <- price_by_cls %>% left_join(ef_all %>% select(item, edible)) %>% 
  mutate_at(vars(starts_with("price")), funs(./edible))   # price per edible kg



### Emission data comparison (for Rice/Wheat)

# Rice/Wheat total consumption from NSS
# rice_wheat <- (IND_FOOD_Alldata %>% filter(str_detect(item, 'Rice') | str_detect(item, 'Wheat'))) %>% select(id, item, contains("tot")) %>%
#   rename(hhid=id)
# rice_wheat <- rice_wheat %>% left_join(IND_HH %>% select(hhid, weight)) %>% 
#   group_by(item) %>% summarise(tot_kg=sum(qty_tot*weight, na.rm=TRUE) / scaler_IND, tot_usd=sum(val_tot_org*weight, na.rm=TRUE)/ scaler_IND) %>%
#   mutate(tot_usd = tot_usd * PPP_IND / CPI_ratio_IND / EXR_IND) %>% mutate(price = tot_usd/tot_kg)
# 
# # Emission intensity from EXIO
# IND_em_int <- data.frame(indirect_em_int[,IND_idx_ex] * EXR_EUR$r / 1000) # kg/M.EUR to g/USD2007
# IND_rice_wheat <- IND_em_int[,c(1,2)] 
# names(IND_rice_wheat) <- c("Rice", "Wheat")  # Paddy rice and wheat. 
# row.names(IND_rice_wheat) <- GHG_item       # g/USD2007
# IND_rice_wheat[c(2,5),] <- IND_rice_wheat[c(2,5),] * 25  # Converting to CO2e/USD (CH4)
# IND_rice_wheat[c(3,6),] <- IND_rice_wheat[c(3,6),] * 298 # Converting to CO2e/USD (N2O)
# em_per_kg <- IND_rice_wheat %>% mutate(em_rc = Rice*rice_wheat$price[1], em_wh = Wheat*rice_wheat$price[3])  # g emission/kg grain
# colSums(em_per_kg)


### Derive effectiveness metrics

food_corr <- food_nutrients %>% left_join(food_avgs_national %>% select(item, avg_price, qty_tot)) %>% 
  left_join(ef_all %>% select(item, abbr, ef_per_kg_eaten, edible, group)) 
# mutate(usd_per_kg_eaten=avg_price/edible)  # $ per edible kg

nutri_cost <- food_corr %>% mutate(p_protein = protein/avg_price, p_iron = iron/avg_price,
                                   p_zinc = zinc/avg_price, p_vita  = vita/avg_price , p_cal = energy/avg_price,
                                   e_protein = protein/ef_per_kg_eaten, e_iron = iron/ef_per_kg_eaten,
                                   e_zinc = zinc/ef_per_kg_eaten, e_vita  = vita/ef_per_kg_eaten , e_cal = energy/ef_per_kg_eaten) %>%
  mutate(calintake = energy * qty_tot * edible/365)


