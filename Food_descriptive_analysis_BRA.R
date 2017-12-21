source("Food_init.R")

runname <- "devmin" #"devmin100" #  "fert" # 


################
### Plotting ###
################

# Figure 5 of the manuscript "Food item emissions and cost per nutrient."

### Plot cost & emission effectiveness of food items

nutri_cost <- nutri_cost %>% filter(group != "OTH") %>% left_join(food_avgs_national %>% select(item, avg_price, qty_tot))
gram <- nutri_cost %>% filter(grepl("Gram", item)) %>% mutate(abbr="Gram") %>% group_by(abbr) %>%
  summarise(avg_price=weighted.mean(avg_price, qty_tot), qty_tot=sum(qty_tot), calintake=sum(calintake)) #%>% summarise_if(is.numeric, first)
nutri_cost[grepl("Gram", nutri_cost$item), c("abbr", "avg_price", "qty_tot", "calintake")] <- gram
nutri_cost <- nutri_cost %>% mutate_cond(abbr=="Gram", 
                           p_protein = protein/avg_price, p_iron = iron/avg_price,
                           p_zinc = zinc/avg_price, p_vita = vita/avg_price , p_cal = energy/avg_price)
nutri_cost <- nutri_cost[!grepl("Gram, split|Gram, whole", nutri_cost$item),] 

calrange <- c(min((nutri_cost$calintake)), max((nutri_cost$calintake)))
n_slice <- 30

library("ggrepel")
library(colorRamps)

pl1 <- ggplot(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(x=1/p_protein, y=1000/e_protein)) +
  geom_point(size=3, aes(color=calintake)) + 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) +
  # theme(legend.position="right") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/g Protein',y="gCO2e/g Protein") +
  geom_text_repel(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl2 <- ggplot(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(x=1/p_zinc, y=1000/e_zinc)) +
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/mg Zinc',y="gCO2e/mg Zinc")+
  geom_text_repel(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl3 <- ggplot(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(x=1/p_iron, y=1000/e_iron))+
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/mg Iron',y="gCO2e/mg Iron")+
  geom_text_repel(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl4 <- ggplot(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(x=1/p_vita, y=1000/e_vita))+
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/ug Vitamin A',y="gCO2e/ug Vitamin A")+
  geom_text_repel(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl5 <- ggplot(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(x=1/p_cal, y=1000/e_cal))+
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/kcal',y="gCO2e/kcal")+
  geom_text_repel(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

# grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow=2, ncol=3)
pdf(file = paste0(workdir, "Figures/Food item effectiveness-tot cal-", runname, ".pdf"), width = 17, height = 10)
grid_arrange_shared_legend(pl1, pl2, pl3, pl4, pl5, nrow=2, ncol=3, position="right")
dev.off()



p1 <- ggplot(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(x=1/p_protein, y=1000/e_protein)) +
  geom_point(size=3, aes(color=protein))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "g Protein/kg food", breaks=c(150, 200, 250)) + 
  theme(legend.position="bottom") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/g Protein',y="gCO2e/g Protein")+
  geom_text_repel(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p2 <- ggplot(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(x=1/p_zinc, y=1000/e_zinc))+
  geom_point(size=3, aes(color=zinc))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "mg zinc/kg food", breaks=c(20, 40, 60)) + 
  theme(legend.position="bottom") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/mg Zinc',y="gCO2e/mg Zinc")+
  geom_text_repel(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p3 <- ggplot(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(x=1/p_iron, y=1000/e_iron))+
  geom_point(size=3, aes(color=iron))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "mg iron/kg food", breaks=c(30, 60, 90)) + 
  theme(legend.position="bottom") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/mg Iron',y="gCO2e/mg Iron")+
  geom_text_repel(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p4 <- ggplot(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(x=1/p_vita, y=1000/e_vita))+
  geom_point(size=3, aes(color=vita))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans='log', name = "ug Vitamin A/kg food", breaks=c(250, 1000, 4000, 16000)) + 
  theme(legend.position="bottom") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/ug Vitamin A',y="gCO2e/ug Vitamin A")+
  geom_text_repel(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p5 <- ggplot(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(x=1/p_cal, y=1000/e_cal))+
  geom_point(size=3, aes(color=energy))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "kcal/kg food", breaks=c(4000, 5000, 6000)) + 
  theme(legend.position="bottom") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/kcal',y="gCO2e/kcal")+
  geom_text_repel(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

pdf(file = paste0(workdir, "Figures/Food item effectiveness-nutr content-", runname, ".pdf"), width = 18, height = 10)
grid.arrange(p1, p2, p3, p4, p5, nrow=2, ncol=3)
# grid.arrange(p1, p2, p4, p3, nrow=2, ncol=2)
dev.off()


# Figure 6 of the manuscript "Actual food prices paid by households for major food items."

### Price summary plot by cluster
items_to_plot <- c("Rice, non-PDS", "Rice, PDS", "Wheat/atta, non-PDS", "Wheat/atta, PDS", "Milk, liquid", "Potato", "Coconut",
                    "Arhar, tur", "Jowar and its products", "Bajra and its products", "Maize and its products",
                    "Masur", "Moong", "Gram, split", "Gram, whole", "Banana", "Fish, prawn", "Chicken", "Beef/buffalo meat")
# items_to_plot <- c("whole wheat", "rice", "pulses & legumes", "milk products", "roots & tubers", "Meat and poultry", "leafy vegetables")
# price_plot <- food_avgs_grp %>% filter(item %in% itemss_to_plot) %>% mutate(cls=paste0(cluster,"_",inc_grp))

price_plot <- food_price_indiv %>% mutate(grouped = item) %>% left_join(food_wgrp) %>% 
  # mutate_cond(grepl("Rice", item), grouped="Rice") %>%
  # mutate_cond(grepl("Wheat", item), grouped="Wheat") %>%
  mutate_cond(grepl("Gram", item), grouped="Gram") %>%
  filter(item %in% items_to_plot) 
price_plot <- price_plot %>% group_by(id, grouped) %>% 
  summarise(price = weighted.mean(avg_price, qty_tot), item=first(item), group=first(group)) %>% 
  left_join(hh_sum%>% select(id, hh_size, cluster, weight, urban, inc_grp, region)) 
rm(food_price_indiv)
gc()
# Specify orders x-axis
price_plot$item_fac <- factor(price_plot$grouped, 
                              levels=c("Rice, non-PDS", "Rice, PDS", "Wheat/atta, non-PDS", "Wheat/atta, PDS", "Milk, liquid", "Potato", "Coconut",
                                       "Arhar, tur", "Jowar and its products", "Bajra and its products", "Maize and its products",
                                       "Masur", "Gram", "Moong", "Banana", "Fish, prawn", "Chicken", "Beef/buffalo meat"),
                              labels=c("Rice,nPDS", "Rce,PDS", "Wheat,nPDS", "Wht,PDS", "Milk", "Potato", "Coconut", "Arhar", "Jowar", 
                                       "Bajra", "Maize", "Masur", "Gram", "Moong", "Banana", "Fish", "Chicken", "Beef"))

pr_plot <- ggplot(price_plot, aes(x = item_fac, weight=weight, fill=group))
pr_plot <- pr_plot + geom_boxplot(aes(y = price), outlier.size=NA) + ylim(0, 12) +
  labs(x="Major food items", y="$ per kg (PPP)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  # facet_grid(.~urban)
pr_plot
ggsave(paste0(workdir, "Figures/Food price compare-", runname, ".pdf"), 
       pr_plot, width=20, height=13, unit="cm")


# compare rice/Wheat PDS price
rice_wheat <- selectDBdata(ID, ITEM, VAL_TOT, VAL_TOT, VAL_OWN, VAL_FREE, VAL_BAR, 
                           QTY_TOT, QTY_OWN, QTY_FREE, QTY_BAR, tables="IND1_FOOD") %>% 
  filter(grepl('Rice|Wheat', item) ) %>% 
  mutate(price_org = val_tot/qty_tot, price_adj = val_tot/qty_tot, 
         price_mkt = (val_tot - val_own - val_free - val_bar)/(qty_tot - qty_own - qty_free - qty_bar)) %>% 
  left_join(selectDBdata(ID, WEIGHT, tables='IND1_HH'))
rice_wheat_price <-  rice_wheat %>% group_by(item) %>% summarise(price_avg=weighted.mean(price_org, weight, na.rm=TRUE),
                                                                 price_mkt=weighted.mean(price_mkt, weight, na.rm=TRUE))

ggplot(rice_wheat %>% filter(grepl(' PDS', item)), aes(x = item, weight=weight)) + 
  geom_boxplot(aes(y = price_org)) + ylab("USD/kg")


########################
### Total national nutrient requirement (for Kyle Davis (columbia))
########################

pop.IND.2011 <- 1.247e9 # from Google search (WB) for 2011
hh_dri <- selectDBdata(SURVEY, ID, HH_SIZE, MALE, MALE_ADULT, MALE_MINOR, MINOR, WEIGHT, tables='IND1_HH') %>% 
  mutate(female_adult = hh_size-minor-male_adult, female_minor = minor-male_minor) %>%
  mutate(zinc.DRI = male_adult*getDRI("male_adult", "zinc") + male_minor*getDRI("male_minor", "zinc") + female_adult*getDRI("female_adult", "zinc") + female_minor*getDRI("female_minor", "zinc"),
         iron.DRI = male_adult*getDRI("male_adult", "iron") + male_minor*getDRI("male_minor", "iron") + female_adult*getDRI("female_adult", "iron") + female_minor*getDRI("female_minor", "iron"),
         cal.DRI  = male_adult*getDRI("male_adult", "calorie") + male_minor*getDRI("male_minor", "calorie") + female_adult*getDRI("female_adult", "calorie") + female_minor*getDRI("female_minor", "calorie"),
         prtn.DRI = male_adult*getDRI("male_adult", "protein") + male_minor*getDRI("male_minor", "protein") + female_adult*getDRI("female_adult", "protein") + female_minor*getDRI("female_minor", "protein"),
         vitA.DRI = male_adult*getDRI("male_adult", "vita") + male_minor*getDRI("male_minor", "vita") + female_adult*getDRI("female_adult", "vita") + female_minor*getDRI("female_minor", "vita"))
# Nutrients in g/year, calorie in kcal/yr
national_dri <- hh_dri %>% 
  summarise(zinc.tot = sum(365*zinc.DRI*weight)/1e3, iron.tot = sum(365*iron.DRI*weight)/1e3, 
            cal.tot = sum(365*cal.DRI*weight), prtn.tot = sum(365*prtn.DRI*weight), vitA.tot = sum(365*vitA.DRI*weight)/1e6,
            pop = sum(hh_size*weight)) %>%
  mutate_all(funs(.*pop.IND.2011/pop))
view(national_dri)

pop.NSS <- as.numeric(hh_dri %>% summarise(sum(weight*hh_size)))

### Total national nutrient intake
options(scipen = -4)
national_supply <- hh_sum %>% select(id, hh_size, weight, cu_eq, ends_with('tot_weatout')) %>% 
  summarise_at(vars(ends_with('tot_weatout')), funs(sum(.*cu_eq*weight*365))) %>%
  mutate(zinc_tot_weatout = zinc_tot_weatout/1e3, iron_tot_weatout = iron_tot_weatout/1e3, vita_tot_weatout =  vita_tot_weatout/1e6) %>%
  mutate_all(funs(.*pop.IND.2011/pop.NSS)) %>% select(zinc_tot_weatout, iron_tot_weatout, energy_tot_weatout, protein_tot_weatout, vita_tot_weatout)
names(national_supply) <- c("zinc_consum", "iron_consum", "cal_consum", "prtn_consum", "vita_consum")
view(national_supply)
options(scipen = 0)


########################################
## Nutrition supply by food item/group #
########################################

nutri_by_food <- food_items %>% left_join(food_summary) %>% left_join(hh_sum %>% select(id, weight)) %>% 
  mutate(kg=ifelse(is.na(avg_wt), qty_tot, avg_wt*qty_tot)) %>% 
  mutate(iron.tot=weight*kg*edible*iron, zinc.tot=weight*kg*edible*zinc, 
         vita.tot=weight*kg*edible*vita, prtn.tot=weight*kg*edible*protein, cal.tot=weight*kg*edible*energy) %>% 
  group_by(item) %>% arrange(item) %>% 
  summarise(iron.tot=sum(iron.tot, na.rm=TRUE), zinc.tot=sum(zinc.tot, na.rm=TRUE), 
            vita.tot=sum(vita.tot, na.rm=TRUE), prtn.tot=sum(prtn.tot, na.rm=TRUE), cal.tot=sum(cal.tot, na.rm=TRUE), group=first(group)) %>%
  mutate(iron.sh = iron.tot/sum(iron.tot), zinc.sh = zinc.tot/sum(zinc.tot), vita.sh = vita.tot/sum(vita.tot), 
         prtn.sh = prtn.tot/sum(prtn.tot), cal.sh = cal.tot/sum(cal.tot)) %>% filter(!is.na(group))

nutri_by_group <- nutri_by_food %>% group_by(group) %>% summarise_if(is.numeric, funs(formatC(sum(.), digits=2))) %>% select(-contains("tot"))
