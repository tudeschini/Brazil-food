library(readxl)
library(data.table)
library(stringr)
library(dplyrExtras)

setup.scripts = list.files("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/", pattern="*.R$", full.names=T, ignore.case=T)

survey.code = "BRA0"
survey.name = "BRA POF 2008-2009"

source(setup.scripts[[1]])
source(setup.scripts[[2]]) # load functions like genvar
source(setup.scripts[[3]]) 

# Function to convert character vectors in data frame to numeric (via type.convert), when possible
char2num = function(d) {
  d[] = lapply(d, function(x) if (class(x)=="character") type.convert(x, as.is=T) else x)  # Convert character to numeric, if possible
  return(d)
}

# Excel file linking survey's item codes to item names
ce_code = read_excel("Documentation/BRA POF 2008-2009 CE Codes.xlsx") %>% data.table(key="code") %>%
  mutate(code7=as.numeric(code)) %>% filter(main=="Food and beverage")
# Excel file linking state numeric codes to state name
geo_code = read_excel("Documentation/BRA POF 2008-2009 Geographic Codes.xlsx")

ppp_path = "P:/ene.general/DecentLivingEnergy/Surveys/Consumer Prices/Monthly PPP adjustment factors for base year 2010.RData"
ppp_fact = filter(readRDS(ppp_path), iso3=="BRA", year==2009, month==1)$ppp_fact

nutri <- read_xls(paste0(datadir, "Tables of Nutritional Composition of Food - IBGE liv50002.xls"), skip=3, n_max=1971) %>% 
  rename(code7=`CÓDIGO DO ALIMENTO`) %>% mutate(code5 = floor(code7/100))
IBGE_map <- read_xls(paste0(datadir, "POF_2008-2009_Codigos_de_alimentacao - POF.xls"), skip=2, n_max=2339) 
names(IBGE_map) <- c("code5", "level1", "item1", "level2", "item2", "level3", "item3")
IBGE_map <- IBGE_map %>% #rename(code5=Código)
  mutate_cond(is.na(item3), item3=item2, level3=level2)
  
# IBGE_map_EN <- read_xlsx("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/BRA POF 2008-2009 CE Codes.xlsx") %>% 
#   filter(main=="Food and beverage") %>% rename(code7=code) %>% mutate(code7=as.numeric(code7))

load(paste0(path_POF, "t_caderneta_despesa_s.rda"))
load(paste0(path_POF, "t_despesa_individual_s.rda"))
load(paste0(path_POF, "codigos de alimentacao.rda"))
load(paste0(path_POF, "t_domicilio_s.rda"))
load(paste0(path_POF, "poststr.rda"))
load(paste0(path_POF, "t_morador_s.rda"))


d = char2num(t_domicilio_s)
post = char2num(poststr)

# Information used to define 'estrato' variable values that identify rural housholds
estrato.cats <- c( 7 , 3 , 9 , 3 , 9 , 4 , 6 , 13 , 10 , 24 , 9 , 10 , 16 , 9 , 8 , 22 , 28 , 10 , 31 , 31 , 19 , 14 , 19 , 9 , 11 , 18 , 8 )
names(estrato.cats) <- unique(d$cod_uf)

d1 = d %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(control=as.integer(paste0(cod_uf, str_pad(num_seq,3,pad=0), num_dv))) %>%  # Variable used to merge with post-stratification object
  left_join(post) %>%  # Join information from the post-stratification table
  rename(weight=fator_expansao2, income=renda_total) %>%  # fator_expansao2 is identical to post-stratifying weights using Census 2010 data
  mutate(income=income*12) %>%  # Original income variable is monthly HH income; this converts to annual
  mutate(date_int=as.Date("2008-05-19")+(perd_cod_p_visit_realm_em-1)*7) %>% # Raw variable gives number of weeks beginning May 19 2008
  left_join(geo_code) %>%   # cod_uf is numeric link to state name
  rename(region=reg1) %>%
  mutate(urban=ifelse(estrato >= estrato.cats[match(cod_uf, names(estrato.cats))] , 0 , 1 ))  %>%
  select(id, weight, date_int, region, urban, income)

### Process person data

# load("Data/t_morador_s.rda")

d2 = char2num(t_morador_s) %>%
  # mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(cid=paste0(id,cod_unid_consumo)) %>%
  mutate(pid=paste0(id,cod_unid_consumo,num_informante)) %>%
  rename(weight=fator_expansao2, age=idade_anos, pheight=altura_imputado, pweight=peso_imputado) %>%
  genvar(male, cod_sexo, c(1,0), c(1,2)) %>%
  genvar(rel, cod_rel_pess_refe_uc, c('Head','Spouse','Child','Other relative','Other non-relative','Tenant','Domestic employee','Relative of domestic employee'), 1:8) %>%
  genvar(head, rel, T, "Head", other=F) %>%
  genvar(race, cod_cor_raca, c('White','Black','Yellow','Brown','Indigenous','Do not know'), c(1:5,9)) %>%
  genvar(educ_level, cod_nivel_instr, c('Nursery school','Preschool','Child literacy class',
                                        'Adult literacy class','Old primary','Old gymnasium',
                                        'Old classic, scientific','Regular basic education',
                                        'Young and adult education for elementary school',
                                        'Regular high school','Young and adult education for high school',
                                        'Technological college','Pre-college','Undergraduate',
                                        'Professional degree','Masters or doctorate'), 1:16) %>%
  left_join(educ_years) %>% # Add years of schooling completed
  mutate(student=as.integer(cod_curso_freq>0)) %>%
  mutate(earner=as.integer(cod_sit_receita==1)) %>%
  data.table(key = "id") %>%
  select(id, cid, pid, weight, head, rel, male, age, race, educ_level, educ_years, student, earner, pheight, pweight)

# Create household summary variables from person records
pp = d2[,list(
  hh_size = .N,
  male = male[head],
  age = age[head],
  race = race[head],
  educ_level = educ_level[head],
  # educ_years = educ_years[head],
  minor = sum(age<18), # Number of minors (less than 18 years old)
  # student = sum(student),
  # earner = sum(earner),
  male_adult = sum(age>=18 & male==1),
  male_minor = sum(age<18 & male==1),
  age_adult = mean(age[age>=18]),  # Mean age of people over 18 (can be NA in case of minor head of household)
  age_minor = mean(age[age<18])  # Mean age of people under 18 (will be NA if no minors in household)
), by=id]

# If multiple household heads specified, retain information for eldest head only (male if age is a tie)
#check = pp[,list(n = .N), by=id] %>% filter(n>1)
pp = pp %>%
  arrange(id, -age, -male) %>%
  unique(by="id")

#--------------------------
#--------------------------

# Combine household level variables
hh = Reduce(function(...) left_join(...), list(d1,pp))





sum_nutri <- nutri %>% 
  # left_join(IBGE_map) %>%
  left_join(ce_code)
names(sum_nutri) <- make.names(names(sum_nutri))

length(unique(sum_nutri$code7))
length(unique(sum_nutri$code5))
sum_nutri5 <- sum_nutri %>% group_by(code5) %>% arrange(CÓDIGO............DA.PREPARAÇÃO) %>% slice(1) %>% ungroup()
sum_nutri %>% group_by(code5) %>% summarise(count=n())
sum_nutri <- sum_nutri %>% group_by(code7) %>% arrange(CÓDIGO............DA.PREPARAÇÃO) %>% slice(1) %>% ungroup()
# sum_nutri <- sum_nutri %>% filter(CÓDIGO............DA.PREPARAÇÃO==99 | CÓDIGO............DA.PREPARAÇÃO==1)

# All POF Food consumption records
food = char2num(t_caderneta_despesa_s) %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(code7=as.numeric(paste0(prod_num_quadro_grupo_pro, str_pad(cod_item,width=5,pad=0)))) %>%
  mutate(code5=floor(code7/100)) %>%
  mutate(period=7, kg=quant_kg, unit="kg") %>%  # Food consumption reported in one-week diary; all quantities are kg
  mutate(value=val_despesa_corrigido*ppp_fact*365/period) %>%  # Inflation-adjusted value (Jan 2009) converted to $PPP in specified base year
  mutate(kg=kg*365/period) %>% 
  mutate_cond(kg==0, kg=NA) %>%  # This ensures that NA is returned for item where quantities are not possible
  # data.table(key=c("code", "code5")) %>%
  left_join(IBGE_map) %>% 
  left_join(ce_code %>% select(code7, product, main, item)) %>% 
  mutate(eatout=(code7 < 6000000), prepared=(code7>8500000)) %>% # nothing below 6000000
  filter(main=="Food and beverage") %>% # Will drop any codes in 'd' not found in 'ce_code'
  # left_join(sum_nutri) %>%
  # group_by(id, item3, unit) %>% 
  select(id, code5, code7, item3, item, product, unit, everything()) %>% arrange(code5, code7, id) %>%
  left_join(hh) %>% ungroup()
  
food.tbl <- data.table(food, key=c("code7", "code5", "level3"))
dim(food.tbl[level3=="1.1.4"])[1]

# food.tbl <- food.tbl %>% group_by(level3, item3, unit) %>% 
#   summarize(val_tot=sum(value), qty_tot=sum(kg), kcal=mean(as.numeric(ENERGIA..kcal.), na.rm=TRUE), infocnt=sum(!is.na(ENERGIA..kcal.)))

# filter(main=="Food and beverage") #%>%
  # summarize(val_tot=sum(value), qty_tot=sum(kg)) %>%  # Sum values by household and item
  # filter(val_tot>0 | is.na(val_tot))

# Link the nutri info based on code7 and see the coverage
a <- food %>% left_join(sum_nutri %>% select(code7, DESCRIÇÃO.DO.ALIMENTO, ENERGIA..kcal.)) %>% mutate(kcal=as.numeric(ENERGIA..kcal.)) %>%
  select(code5, code7, product, DESCRIÇÃO.DO.ALIMENTO, item, item3, value, kg, prepared, kcal, weight, hh_size) %>% 
  data.table(key="item") %>%
  mutate(no_nutri = is.na(DESCRIÇÃO.DO.ALIMENTO))
a <- a %>% filter(item=="Baked Products" | item=="Dairy" | item=="Cereals and legumes" | grepl("meat", item, ignore.case=TRUE) | 
                    grepl("vegetables", item, ignore.case=TRUE) | grepl("fish", item, ignore.case=TRUE) | item=="Poultry and eggs" | 
                    item=="Flour, starch and pasta" | grepl("nuts", item, ignore.case=TRUE) | grepl("fruit", item, ignore.case=TRUE) )
exp <- a %>% group_by(item) %>% summarise(val_tot=sum(value), qty_tot=sum(kg)) %>% 
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




### Other consumption
load("Data/t_despesa_90dias_s.rda")  # 3-month housing
load("Data/t_despesa_12meses_s.rda")  # 12-month housing
load("Data/t_despesa_individual_s.rda")  # Name???
load("Data/t_despesa_veiculo_s.rda")  # Vehicle expenditures
load("Data/t_outras_despesas_s.rda")  # Other expenditures
load("Data/t_aluguel_estimado_s.rda")  # Other expenditures

d = bind_rows(
  char2num(t_despesa_90dias_s),
  char2num(t_despesa_12meses_s),
  # char2num(t_despesa_individual_s),
  char2num(t_despesa_veiculo_s),
  char2num(t_outras_despesas_s),
  char2num(t_aluguel_estimado_s)
)

cons = d %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(code7=as.numeric(paste0(str_pad(num_quadro,width=2,pad=0), str_pad(cod_item,width=5,pad=0)))) %>%
  # mutate(code7=as.numeric(paste0(prod_num_quadro_grupo_pro, str_pad(cod_item,width=5,pad=0)))) %>%
  mutate(period=365/fator_anual) %>%
  mutate(value=val_despesa_corrigido*ppp_fact*365/period, qty=quantidade_final*365/period) %>%  # Inflation-adjusted value (Jan 2009) converted to $PPP in specified base year
  data.table(key="code7") %>%
  merge(ce_code) %>% 
  filter(main=="Food and beverage") %>%  # Will drop any codes in 'd' not found in 'ce_code'
  group_by(id,item) %>%
  summarise(eatout_tot=sum(value), qty_tot=sum(qty)) %>%  # Sum and annualize values
  filter(eatout_tot>0 | is.na(eatout_tot))




# Link the nutri info from Claudia and see the coverage
a <- food %>% mutate(covered = code7 %in% POF.covered) %>% left_join(POF.nutri.p %>% select(-code5)) 
a <- food %>% mutate(covered = code5 %in% POF.covered5) %>% left_join(POF.nutri.p %>% select(-code7)) 
a %>% group_by(covered) %>% summarise(val.tot=sum(value*weight), val.share=sum(value*weight)/sum(a$value*a$weight), kcal.tot=sum(kg*weight*kcal*10, na.rm=TRUE), 
                                      kcal.pcap.day=sum(kg*weight*kcal*10, na.rm=TRUE)/sum(hh$hh_size*hh$weight)/365)

