setwd("H:/MyDocuments/Analysis/Food/Brazil")

BRA2_hh <- selectDBdata(ID, WEIGHT, INCOME, CONSUMPTION, EXPENDITURE, tables='BRA2_HH')
BRA2_fd <- selectDBdata(tables='BRA2_FOOD') %>% select(-survey) %>% rename(item_food = item, val_food = val_tot, qty_food = qty_tot, unit_food = unit) %>%
  group_by(id) %>% summarise(val_food=sum(val_food, na.rm = TRUE), qty_food=sum(qty_food, na.rm = TRUE))
BRA2_fuel <- selectDBdata(tables='BRA2_FUEL') %>% select(-survey) %>% rename(item_fuel = fuel, val_fuel = val_tot, qty_fuel = qty_tot, unit_fuel = unit)%>%
  group_by(id) %>% summarise(val_fuel=sum(val_fuel, na.rm = TRUE), qty_fuel=sum(qty_fuel, na.rm = TRUE))
BRA2_oth <- selectDBdata(tables='BRA2_OTHCON') %>% select(-survey) %>% rename(item_oth = item, val_oth = val_tot)%>%
  group_by(id) %>% summarise(val_oth=sum(val_oth, na.rm = TRUE))
BRA2 <- BRA2_hh %>% left_join(BRA2_fd) %>% left_join(BRA2_fuel) %>% left_join(BRA2_oth) %>% rowwise() %>% mutate(valsum=sum(val_oth, val_food, val_fuel, na.rm = TRUE)) %>% 
  arrange(val_food)
BRA2_nf <- BRA2 %>% filter(is.na(val_food))

hist(BRA2$income, 1000, xlim=c(0, 1e5))
hist(BRA2_nf$income, 1000, xlim=c(0, 1e5))

BRA1_hh <- selectDBdata(ID, WEIGHT, INCOME, CONSUMPTION, EXPENDITURE, tables='BRA1_HH')
BRA1_fd <- selectDBdata(tables='BRA1_FOOD') %>% select(-survey) %>% rename(item_food = item, val_food = val_tot, qty_food = qty_tot, unit_food = unit) %>%
  group_by(id) %>% summarise(val_food=sum(val_food, na.rm = TRUE), qty_food=sum(qty_food, na.rm = TRUE))
BRA1_fuel <- selectDBdata(tables='BRA1_FUEL') %>% select(-survey) %>% rename(item_fuel = fuel, val_fuel = val_tot, qty_fuel = qty_tot, unit_fuel = unit)%>%
  group_by(id) %>% summarise(val_fuel=sum(val_fuel, na.rm = TRUE), qty_fuel=sum(qty_fuel, na.rm = TRUE))
BRA1_oth <- selectDBdata(tables='BRA1_OTHCON') %>% select(-survey) %>% rename(item_oth = item, val_oth = val_tot)%>%
  group_by(id) %>% summarise(val_oth=sum(val_oth, na.rm = TRUE))
BRA1 <- BRA1_hh %>% left_join(BRA1_fd) %>% left_join(BRA1_fuel) %>% left_join(BRA1_oth) %>% rowwise() %>% mutate(valsum=sum(val_oth, val_food, val_fuel, na.rm = TRUE)) %>% 
  arrange(val_food)
BRA1_nf <- BRA1 %>% filter(is.na(val_food))
BRA1_fd <- selectDBdata(tables='BRA1_FOOD') %>% select(-survey) %>% group_by(item) %>% mutate()

hist(BRA1$income, 1000, xlim=c(0, 1e5))
hist(BRA1_nf$income, 1000, xlim=c(0, 1e5))

a <- describe(BRA1_nf%>%select(-c(33:38)))
b <- describe(BRA1_hh)

# Expenditure by item
BRA1_fd_sum <- selectDBdata(tables='BRA1_FOOD') %>% select(-survey) %>% group_by(item) %>% summarise(val=sum(val_tot))

BRA0_hh <- selectDBdata(ID, WEIGHT, INCOME, CONSUMPTION, EXPENDITURE, HH_SIZE, URBAN, tables='BRA0_HH') %>% group_by(urban) %>%
  mutate(income.pcap=income/hh_size) %>% arrange(urban, income.pcap) %>%
  mutate(cumpop = cumsum(hh_size*weight)/sum(weight*hh_size)) %>%
  mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1), labels=1:10, include.lowest = TRUE, ordered=TRUE)) %>% 
  mutate(group=paste0(ifelse(urban, "U", "R"), str_pad(decile, 2, pad = "0"))) 
BRA0_decile <- BRA0_hh %>% group_by(group) %>% summarise(pop=sum(hh_size*weight))

BRA0_fd <- selectDBdata(tables='BRA0_FOOD') %>% select(-survey) %>% rename(item_food = item, val_food = val_tot, qty_food = qty_tot, unit_food = unit) %>%
  group_by(id) %>% summarise(val_food=sum(val_food, na.rm = TRUE), qty_food=sum(qty_food, na.rm = TRUE))
BRA0_fuel <- selectDBdata(tables='BRA0_FUEL') %>% select(-survey) %>% rename(item_fuel = fuel, val_fuel = val_tot, qty_fuel = qty_tot, unit_fuel = unit)%>%
  group_by(id) %>% summarise(val_fuel=sum(val_fuel, na.rm = TRUE), qty_fuel=sum(qty_fuel, na.rm = TRUE))
BRA0_oth <- selectDBdata(tables='BRA0_OTHCON') %>% select(-survey) %>% rename(item_oth = item, val_oth = val_tot)%>%
  group_by(id) %>% summarise(val_oth=sum(val_oth, na.rm = TRUE))
BRA0 <- BRA0_hh %>% left_join(BRA0_fd) %>% left_join(BRA0_fuel) %>% left_join(BRA0_oth) %>% rowwise() %>% mutate(valsum=sum(val_oth, val_food, val_fuel, na.rm = TRUE)) %>% 
  arrange(val_food)
BRA0_nf <- BRA0 %>% filter(is.na(val_food))

ICP_cat <- selectDBdata(tables='ICP_CAT') 
BRA0_fd_hh <- selectDBdata(tables='BRA0_FOOD') %>% select(-survey) %>% rename(item_food = item, val_food = val_tot, qty_food = qty_tot, unit_food = unit) %>%
  left_join(BRA0_hh) %>% left_join(ICP_cat %>% rename(item_food=icp_name)) %>% 
  group_by(icp_code, item_food, unit_food) %>% summarise(val_food=sum(val_food*weight), qty_food=sum(qty_food*weight)) %>%
  filter(!is.na(icp_code))


IND1_hh <- selectDBdata(ID, WEIGHT, CONSUMPTION, EXPENDITURE, tables='IND1_HH')
IND1_fd <- selectDBdata(tables='IND1_FOOD') %>% select(-survey) %>% rename(item_food = item, val_food = val_tot, qty_food = qty_tot, unit_food = unit) %>%
  group_by(id) %>% summarise(val_food=sum(val_food, na.rm = TRUE), qty_food=sum(qty_food, na.rm = TRUE))
IND1_fuel <- selectDBdata(tables='IND1_FUEL') %>% select(-survey) %>% rename(item_fuel = fuel, val_fuel = val_tot, qty_fuel = qty_tot, unit_fuel = unit)%>%
  group_by(id) %>% summarise(val_fuel=sum(val_fuel, na.rm = TRUE), qty_fuel=sum(qty_fuel, na.rm = TRUE))
IND1_oth <- selectDBdata(tables='IND1_OTHCON') %>% select(-survey) %>% rename(item_oth = item, val_oth = val_tot)%>%
  group_by(id) %>% summarise(val_oth=sum(val_oth, na.rm = TRUE))
IND1 <- IND1_hh %>% left_join(IND1_fd) %>% left_join(IND1_fuel) %>% left_join(IND1_oth) %>% mutate(valsum=val_oth+val_food+val_fuel) %>% 
  arrange(val_food) %>% filter(is.na(val_food))
