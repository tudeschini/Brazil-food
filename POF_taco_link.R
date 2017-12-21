### Function to find out where each POF code belongs to
get.idx <- function(y) {
  idx <- which(sapply(POF.mapping, function(x) {y %in% x}))
  return(idx)
}



### 1. Read in Claudia's mapping
# Nutrition info is based on "Taco_4a_edicao_2011 - TACO.xls" (2011)
POF.nutri <- read_xlsx("C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/Food/Brazil/Mapping collaboration/171023 Nutricional values food POF2008-9 - Claudia.xlsx", 
                       range = "A4:J109", 
                       col_types=c("text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) 
names(POF.nutri) <- c("item.prt", "item.eng", "POF.num", "cons.percap", "kcal", "protein", "iron", "zinc", "vita", "taco.num")
POF.nutri <- POF.nutri %>% mutate(item.num=row_number()) %>% select(item.num, everything()) 

# Mapped POF
POF.mapping <- strsplit(POF.nutri$POF.num, c("[,'´`]"))
POF.mapping <- lapply(POF.mapping, function(x) {as.numeric(x[-1])})

# List of POF item numbers (7 & 5 digits) covered by Claudia's mapping
POF.covered <- unlist(POF.mapping)
POF.covered5 <- floor(POF.covered/100)

# Rearrange in terms of POF item number
POF.nutri.p <- data.frame(code7 = POF.covered, code5 = POF.covered5, item.num=sapply(POF.covered, get.idx)) %>% 
  left_join(POF.nutri %>% select(item.num, item.eng, kcal, protein, iron, zinc, vita, taco.num)) %>% arrange(code7) %>%
  select(-item.num, -taco.num)
# POF.nutri.p <- unique(POF.nutri.p %>% select(-code7))




### 2. TACO (2008-2009) based on liv5002.pdf
taco.raw <- read_xls("H:/MyDocuments/Data/Food-BRA/Tables of Nutritional Composition of Food.xls", skip=3, n_max=1971) 
taco <- taco.raw %>% select(1:3, 7:10, 16, 21, 23:24) 
names(taco) <- c("code7", "item", "preparation", "kcal", "protein", "fat", "carb", "iron", "zinc", "retinol", "retinol.eq")
taco <- taco %>% mutate(code5 = floor(code7/100)) %>% 
  mutate_at(c("kcal", "protein", "fat", "carb", "iron", "zinc", "retinol", "retinol.eq"), as.numeric) %>%
  rowwise() %>% mutate(vita=sum(retinol, retinol.eq, na.rm = TRUE)) %>% select(-retinol, -retinol.eq) %>% ungroup() 

# Let's remove unnecessary preparation types

# Items with only one observation or multiple obs with CRU (preperation=1)
# - Choose CRU (1) if there are both 1 and 99
taco.CRU <- taco %>%
  group_by(code7) %>% mutate(n_preps = n()) %>% 
  filter(n_preps==1 | (n_preps>1 & preparation==1) | (n_preps>1 & preparation==99)) %>% 
  mutate(n_preps = n()) %>% slice(1) %>% select(-n_preps) 

# Item numbers having multiple obs but preperation!=1 or 99
no.CRU.items <- unique(taco$code7)[which((unique(taco$code7) %in% unique(taco.CRU$code7))==FALSE)]

taco <- taco.CRU %>% 
  # Then I select whatever first obs from the no.CRU group
  rbind(taco %>% filter(code7 %in% no.CRU.items) %>% group_by(code7) %>% slice(1)) %>% #
  arrange(code7) %>% select(code7, code5, everything()) %>% ungroup()

