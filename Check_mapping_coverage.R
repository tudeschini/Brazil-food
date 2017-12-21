##############################################
### Check mapping coverage
### Based on various potential mappings
##############################################



### 0. Link the nutri info both from TACO (code7) + Claudia (code5) and see the coverage (Most up-to-date) on 21/12/2017
food.master %>% mutate(covered = !is.na(kcal)) %>% group_by(covered) %>% 
  summarise(val.tot=sum(val_tot*weight, na.rm=TRUE), val.share=sum(val_tot*weight)/sum(food.master$val_tot*food.master$weight), 
            kcal.tot=sum(qty_tot*weight*kcal*10, na.rm=TRUE), 
            kcal.pcap.day=sum(qty_tot*weight*kcal*10, na.rm=TRUE)/sum(hh$hh_size*hh$weight)/365)



### 1. Link the nutri info from Claudia and see the coverage 
# This use 'food' table from POF_test.R
# giving a flawed number because of redundancy in the mapping
a <- food %>% mutate(covered = code7 %in% POF.covered) %>% left_join(POF.nutri.p %>% select(-code5)) %>% arrange(id, code7) 
a <- food %>% mutate(covered = code5 %in% POF.covered5) %>% left_join(POF.nutri.p %>% select(-code7)) %>% arrange(id, code5)
a %>% group_by(covered) %>% summarise(val.tot=sum(value*weight, na.rm=TRUE), val.share=sum(value*weight)/sum(a$value*a$weight), 
                                      kcal.tot=sum(kg*weight*kcal*10, na.rm=TRUE), 
                                      kcal.pcap.day=sum(kg*weight*kcal*10, na.rm=TRUE)/sum(hh$hh_size*hh$weight)/365)



### 2. Other mappings
# No need to run this any more (OBSOLETE)

sum_nutri <- nutri %>% 
  left_join(ce_code)
names(sum_nutri) <- make.names(names(sum_nutri))

length(unique(sum_nutri$code7))
length(unique(sum_nutri$code5))
sum_nutri5 <- sum_nutri %>% group_by(code5) %>% arrange(CÓDIGO............DA.PREPARAÇÃO) %>% slice(1) %>% ungroup()
sum_nutri %>% group_by(code5) %>% summarise(count=n())
sum_nutri <- sum_nutri %>% group_by(code7) %>% arrange(CÓDIGO............DA.PREPARAÇÃO) %>% slice(1) %>% ungroup()
# sum_nutri <- sum_nutri %>% filter(CÓDIGO............DA.PREPARAÇÃO==99 | CÓDIGO............DA.PREPARAÇÃO==1)



# Link the nutri info based on code7 and see the coverage
a <- food %>% left_join(sum_nutri %>% select(code7, DESCRIÇÃO.DO.ALIMENTO, ENERGIA..kcal.)) %>% mutate(kcal=as.numeric(ENERGIA..kcal.)) %>%
  select(code5, code7, product, DESCRIÇÃO.DO.ALIMENTO, item, item3, value, kg, prepared, kcal, weight, hh_size) %>% 
  data.table(key="item") %>%
  mutate(no_nutri = is.na(DESCRIÇÃO.DO.ALIMENTO))
a <- a %>% filter(item=="Baked Products" | item=="Dairy" | item=="Cereals and legumes" | grepl("meat", item, ignore.case=TRUE) | 
                    grepl("vegetables", item, ignore.case=TRUE) | grepl("fish", item, ignore.case=TRUE) | item=="Poultry and eggs" | 
                    item=="Flour, starch and pasta" | grepl("nuts", item, ignore.case=TRUE) | grepl("fruit", item, ignore.case=TRUE) )
exp <- a %>% group_by(item) %>% summarize(val_tot=sum(value), qty_tot=sum(kg)) %>% 
  mutate(share=val_tot/sum(val_tot)) %>% filter(val_tot>0 | is.na(val_tot))
a %>% group_by(item3, no_nutri) %>% summarise(sum(value))
a %>% group_by(no_nutri) %>% summarise(sum(value*weight))  
a %>% filter(no_nutri==FALSE) %>% summarise(kcal.tot=sum(kg*weight*kcal*10), 
                                            kcal.pcap=sum(kg*weight*kcal*10)/sum(hh$hh_size*hh$weight)) # Nutrition info per 100g of food.
view(a %>% filter(item == "Cereals and legumes") %>% group_by(code7) %>% 
       summarise(exp = sum(value*weight), qty_tot=sum(kg*weight), no_nutri=first(no_nutri), prod=first(product)) %>% 
       group_by(no_nutri) %>% summarise(exp=sum(exp), kg=sum(qty_tot, na.rm=TRUE)))

dim(a)
# sum(is.na(a$code7))
no_nutri_info7 <- a %>% filter(no_nutri)
table(no_nutri_info7$item)
no_nutri_info7 %>% filter(item=="Cereals and legumes") %>% group_by(product) %>% summarise(code5=first(code5), code7=first(code7), count=n()) %>% arrange(code7)

# Link the nutri info based on code5 and see the coverage
b <- food %>% left_join(sum_nutri5 %>% select(code5, DESCRIÇÃO.DO.ALIMENTO)) %>% 
  select(code5, code7, product, DESCRIÇÃO.DO.ALIMENTO, item, item3, value, kg) %>% 
  data.table(key="item")
b <- b %>% filter(item=="Baked Products" | item=="Dairy" | item=="Cereals and legumes" | grepl("meat", item, ignore.case=TRUE) | 
                    grepl("vegetables", item, ignore.case=TRUE) | grepl("fish", item, ignore.case=TRUE) | item=="Poultry and eggs" | 
                    item=="Flour, starch and pasta" | grepl("nuts", item, ignore.case=TRUE) | grepl("fruit", item, ignore.case=TRUE) ) %>%
  mutate(no_nutri = is.na(DESCRIÇÃO.DO.ALIMENTO))
View(b %>% group_by(item3, no_nutri) %>% summarise(exp=sum(value)) %>% group_by(item3) %>% mutate(share=exp/sum(exp)))
b %>% group_by(no_nutri) %>% summarise(sum(value))


dim(b)
# sum(is.na(b$code7))
no_nutri_info5 <- b %>% filter(no_nutri)
table(no_nutri_info5$item)
no_nutri_info5 %>% filter(item=="Cereals and legumes") %>% group_by(product) %>% summarise(code5=first(code5), code7=first(code7), count=n()) %>% arrange(code7)



# IBGE_map <- read_xls("H:/MyDocuments/Data/Food-BRA/POF_2008-2009_Codigos_de_alimentacao.xls", skip=2, n_max=2339) 
# names(IBGE_map) <- c("code5", "level1", "item1", "level2", "item2", "level3", "item3")
# IBGE_map <- IBGE_map %>% #rename(code5=Código)
#   mutate_cond(is.na(item3), item3=item2, level3=level2)

# IBGE_map_EN <- read_xlsx("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/BRA POF 2008-2009 CE Codes.xlsx") %>% 
#   filter(main=="Food and beverage") %>% rename(code7=code) %>% mutate(code7=as.numeric(code7))

