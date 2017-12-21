# Cooking fuel energy
pop.POF <- as.numeric(BRA0_hh %>% ungroup() %>% summarise(sum(weight*hh_size)))
# ef from https://energypedia.info/images/3/32/2014-03_Multiple_Household_Cooking_Fuels_GIZ_HERA_eng.pdf
# ec from https://en.wikipedia.org/wiki/Energy_content_of_biofuel, https://en.wikipedia.org/wiki/Energy_density (LPG)
e.content <- data.frame(item_fuel = c("Firewood", "LPG", "Pipeline natural gas", "Coal"), ec=c(13.0, 46.4, 1.88e-2, 27.5), ef=c(0.2, 0.65, 0.55, 0.12)) # MJ/kg, MJ/kg, MJ/kg, MJ/l

BRA0_fuel <- selectDBdata(tables='BRA0_FUEL') %>% select(-survey) %>% 
  rename(item_fuel = fuel, val_fuel = val_tot, qty_fuel = qty_tot, unit_fuel = unit) %>%
  left_join(BRA0_hh %>% select(id, hh_size, weight, group)) %>% left_join(BRA0_decile) %>%
  group_by(group, item_fuel, unit_fuel) %>% 
  summarise(val_tot=sum(val_fuel*weight, na.rm = TRUE), 
            qty_tot=sum(qty_fuel*weight, na.rm = TRUE), 
            # val_cap =weighted.mean(val_fuel/hh_size, weight, na.rm = TRUE),
            # qty_cap =weighted.mean(qty_fuel/hh_size, weight, na.rm = TRUE),
            val_cap =sum(val_fuel*weight, na.rm = TRUE)/first(pop),
            qty_cap =sum(qty_fuel*weight, na.rm = TRUE)/first(pop),
            pop = first(pop),
            cnt=n()) %>%
  # mutate(val_pcap=val_fuel/pop.POF, qty_pcap=qty_fuel/pop.POF) %>% 
  left_join(e.content) %>% mutate(usefulMJ_pcap=ec*qty_cap*ef, finalMJ_pcap=ec*qty_cap)
BRA0_cookfuel <- BRA0_fuel %>% filter(!is.na(ec)) %>%
  arrange(item_fuel, unit_fuel, group) 
ggplot(BRA0_cookfuel, aes(x=group, y=usefulMJ_pcap)) +
  geom_bar(aes(fill = item_fuel), stat = "identity")
# xlsx::write.xlsx(BRA0_cookfuel %>% ungroup(), "BRA0_cookfuel.xlsx")

# BRA0_fuel_test <- selectDBdata(tables='BRA0_FUEL') %>% select(-survey) %>% rename(item_fuel = fuel, val_fuel = val_tot, qty_fuel = qty_tot, unit_fuel = unit) %>%
  # left_join(BRA0_hh %>% select(id, hh_size, weight)) %>% filter(item_fuel %in% c("Firewood", "LPG", "Pipeline natural gas", "Coal"))

load("H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_intensities_val_BRA_rev.Rda")
BRA0_intensity <- selectDBdata(tables='PRI_E_INT') %>% filter(country=="BRA") %>% left_join(selectDBdata(tables='ICP_CAT')) %>% 
  filter(icp_code > 151) %>% #filter(grepl("Firewood|LPG|natural gas|Coal", icp_name, ignore.case = TRUE)) %>% 
  mutate(int_rev=BRA_intensity[1,152:164]) %>%
  mutate_cond(icp_name=="Firewood and other fuels", icp_name="Firewood") %>%
  mutate_cond(icp_name=="Natural gas", icp_name="Pipeline natural gas") %>%
  mutate_cond(icp_name=="Charcoal/coal/briquette/coke", icp_name="Coal") %>% rename(item_fuel=icp_name, int=mean)

BRA0_cookE <- BRA0_cookfuel %>% left_join(BRA0_intensity %>% select(item_fuel, int, int_rev)) %>%
  mutate(embod.E=val_tot*int_rev/1e3, embod.E.old=val_tot*int/1e3) # GJ

tot.embod.E <- BRA0_cookE %>% ungroup() %>% group_by(group) %>% 
  summarise(embod.E.pcap = sum(embod.E)/first(pop), final.E.pcap=sum(finalMJ_pcap/1e3), useful.E.pcap=sum(usefulMJ_pcap/1e3)) 
ggplot(tot.embod.E %>% gather(type, GJ, embod.E.pcap:useful.E.pcap), aes(x=group, y=GJ)) +
  geom_bar(aes(fill = type), position = "dodge", stat="identity")

# National avg
BRA0_cookE %>% ungroup() %>% 
  summarise(embod.E.pcap = sum(embod.E)/pop.POF, final.E.pcap=sum(finalMJ_pcap*pop/1e3)/pop.POF, useful.E.pcap=sum(usefulMJ_pcap*pop/1e3)/pop.POF) 
