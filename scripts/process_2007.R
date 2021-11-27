library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(janitor)

options("scipen" = 10)


# Processing --------------------------------------------------------------


## Load data ---------------------------------------------------------------



# ciselniky
cis7_progs <- read_excel("data-input/sestavy 2007-13 orig/ciselniky/ciselnik_programy2007.xlsx")
cis_nuts3 <- read_csv("data-input/sestavy 2007-13 orig/ciselniky/nuts3convert.csv")

# payments
plat7 <- read_csv("data-input/sestavy 2007-13 orig/Platby_Chl_Nov_bez filtru.csv")
# pl7o <- read_csv("N:/odd. 223_EVALUACE/05a_Evaluace NOK/2007–2013/Strategická hodnocení/EX-POST evaluace 2007–2013/Evaluace příspevku ESIF/výstupy DWH07-13/Platby_Chl_Nov_bez filtru.csv")
names(plat7) <- c("projnum","projname","platcislo","platbatyp","den","platbaid","skutid","castka","zdroj")

# project list with categories
prj7kat <- read_excel("data-input/sestavy 2007-13 orig/Prehled_projektu_prioritni_tema_2017.xlsx")
# prj7kat <- read_excel("N:/odd. 223_EVALUACE/05a_Evaluace NOK/2007–2013/Strategická hodnocení/EX-POST evaluace 2007–2013/Evaluace příspevku ESIF/požadavky na data od OSMS/Prehled_projektu_prioritni_tema_2017.xlsx")

# projstavy <- projkat %>%
#   select(`Název projektu`, `Číslo projektu`, `Stav projektu`) %>%
#   distinct(`Číslo projektu`)


# Economic categories -----------------------------------------------------

ec1 <- read_excel("data-input/sestavy 2007-13 20211126/211026_kategorizace7.xlsx")
ec2 <- read_excel("data-input/sestavy 2007-13 20211126/211026_kategorizace8.xlsx")

ec <- bind_rows(ec1, ec2) %>%
  clean_names() %>%
  select(projnum = cislo_projektu,
         stav_2021 = stav_projektu, hospodarska_cinnost) %>%
  separate(hospodarska_cinnost, c("econcat_id", "econcat_name"), sep = " - ")


# Transform, summarise and merge ------------

# recode, summarise payment data to allow checks

plat7sum <- plat7 %>%
  mutate(castka = str_replace(castka, ",","."),
         castka = as.numeric(castka),
         date = as.Date(den, format = "%Y/%m/%d"),
         year = year(date)) %>%
  # group_by(year) %>%
  summarise(castka = sum(castka, na.rm=T)/1e6) %>%
  mutate(castka = round(castka))

# recode, summarise and transform payment data for further use

prj7plat <- plat7 %>%
  mutate(op = str_sub(projnum, 1, 7)) %>%
  mutate(castka = str_replace(castka, ",","."),
         castka = as.numeric(castka),
         date = as.Date(den, format = "%Y/%m/%d"),
         year = year(date),
         eu_nar = ifelse(str_detect(zdroj,"^EU"),"eu","narodni"),
         idvar = paste(projnum, year, sep = "_")) %>%
  group_by(idvar, projnum, year, eu_nar) %>%
  summarise(castka = sum(castka, na.rm = T)) %>%
  spread(eu_nar, castka) %>%
  arrange(projnum)

# check the sums

sum_plat_orig <- prj7plat %>%
  ungroup() %>%
  summarise(castka = (sum(eu, na.rm = T) + sum(narodni, na.rm = T))/1e6)

sum_plat_sum <- plat7sum[[1]]
round(sum_plat_orig) == round(sum_plat_sum)

# Load data on share of NUTS per project

# prj7nuts <- read_excel("N:/odd. 223_EVALUACE/05a_Evaluace NOK/2007–2013/Strategická hodnocení/EX-POST evaluace 2007–2013/Evaluace příspevku ESIF/výstupy DWH07-13/07-13_realizace v NUTS3.xlsx")
prj7nuts <- read_excel("data-input/sestavy 2007-13 orig/07-13_realizace v NUTS3.xlsx")
prj7plc <- read_excel("data-input/sestavy 2007-13 orig/Prehled_projektu_20170405.xlsx")
# nuts3convert <- read.csv("~/Documents/Research/esifdata/ciselniky/nuts3convert.csv")

# Merge NUTS shares with payment data and cross-multiply to get per-NUTS values

prj7mrg_id <- prj7plat %>%
  left_join(prj7nuts %>% rename(projnum = Projekt) %>% arrange(projnum)) %>%
  gather(key = "NUTScode", value = "podil", starts_with("CZ"), starts_with("PL")) %>%
  mutate(fin_vNUTS_EU = eu * podil,
         fin_vNUTS_nar = narodni * podil) %>%
  gather("source","finance", fin_vNUTS_EU, fin_vNUTS_nar) %>%
  ungroup() %>%
  mutate(source = str_replace(source, "fin_vNUTS_",""),
         idvar = paste(NUTScode, source, sep = "_")) %>%
  select(-eu, -narodni, -NUTScode, -podil, -source)

prj7mrg <- prj7mrg_id %>%
  separate(idvar, into = c("NUTS3", "zdroj")) %>%
  spread(zdroj, finance)

# Note: this can be done in three other ways
# 1. Using places of implementation to define NUTS3 weights (tried below)
# 2. Using population in places to define NUTS3 weights (tenuous)
# 3. Using population in NUTS3 to define NUTS3 weights (tenuous)
# can check by correlating pairs of results by NUTS3 (unit) and OP (facet)
#                                          and by OP (unit) and NUTS3 (facet)

# check that the sum hasn't changed

sum(prj7mrg_id$finance, na.rm = T)/1e6
# it dropped a tiny bit - some projects cannot be found in NUTS table

# put EU x national into columns

prj7mrg_wide <- prj7mrg_id %>%
  spread(idvar, finance)

# summarise to give totals per programme, year and region

nuts7yr <- prj7mrg %>%
  select(-projnum) %>%
  group_by(year, NUTS3) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(celkem = EU + nar) %>%
  summarise_all(sum) %>%
  left_join(cis_nuts3)

sum(nuts7yr$celkem, na.rm = T)/1e6

# create sum by programme, year and region

prg7 <- prj7mrg_wide %>%
  mutate(prognum = str_sub(projnum, 1, 7)) %>%
  gather(key = "NUTScode", value = "podil", starts_with("CZ"), starts_with("PL")) %>%
  separate(NUTScode, c("NUTS","source")) %>%
  spread(source, podil) %>%
  replace_na(list(EU = 0, nar = 0)) %>%
  mutate(celkem = EU + nar) %>%
  group_by(NUTS, year, prognum) %>%
  rename(NUTS3 = NUTS, prog_code = prognum) %>%
  summarise(eu = sum(EU, na.rm = T),
            nar = sum(nar, na.rm = T),
            celkem = sum(celkem, na.rm = T)) %>%
  left_join(cis_nuts3) %>%
  left_join(cis7_progs)

# Total in transformed

sum(prg7$celkem, na.rm = T)/1e6


# project data in long form

prj7mrg <- prj7mrg_id %>%
  separate(idvar, c("NUTS3","zdroj"), sep = "_") %>%
  spread(zdroj, finance) %>%
  replace_na(list(EU = 0, nar = 0)) %>%
  mutate(celkem = EU + nar,
         prog_code = str_sub(projnum, 1, 7)) %>%
  left_join(cis7_progs) %>%
  left_join(cis_nuts3) %>%
  left_join(ec, by = "projnum")

sum(prj7mrg$celkem, na.rm = T)/1e6

# Total in rowwise
prj7mrg_wide %>%
  select(-projnum, -year) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  summarise_all(sum) %>%
  mutate(sum = rowSums(.[1:42])) %>% select(sum) %>% mutate(sum=sum/1e6)

# ggplot(prg7, aes(year, celkem, fill = prognum)) +
#   geom_col() +
#   facet_wrap( ~ NUTS3)


## Export regionalised data -------------------------------------------------


write.csv(prg7, file = "nutssum_prog.csv", row.names = F)
write.csv(nuts7yr, file = "nuts_wide.csv", row.names = F)
write.csv(nuts7yrsrc, file = "nuts_long.csv", row.names = F)

write_xlsx(prg7, "nutssum_prg.xlsx")
write_xlsx(nuts7yr, "nuts_wide.xlsx")
write_xlsx(nuts7yrsrc, "nuts_long.xlsx")

##---- Add categories ----

# check share of projects and money in projects with > 1 categories

names(prj7kat) <- c("prog","priax", "oblpodp", "projnum", "projname", "projstav",
                   "vfp_propl_vyuct", "eu_propl_vyuct", "nar_propl_vyuct",
                   "kat_kod","kat_nazev", "kat_poradi","kat_pocet")

prj7kat %>%
  filter(kat_poradi==1) %>%
  group_by(kat_pocet) %>%
  summarise(pocet = n(),
            penize = sum(vfp_propl_vyuct, na.rm=T)) %>%
  ungroup() %>%
  mutate(podil = pocet/sum(pocet), podilpenize = penize/sum(penize))

# check that reported numbers of categories correspond to actually listed cats
prj7kat %>%
  select(projnum, kat_poradi, kat_pocet) %>%
  group_by(projnum) %>%
  mutate(pocetkat = n()) %>%
  filter(kat_poradi == max(kat_poradi)) %>%
  mutate(sedi = pocetkat == kat_poradi) %>%
  ungroup() %>%
  group_by(sedi) %>%
  filter(!sedi)
  # View() # těch 16000 projektů, co nesedí, má kateg 0 a pocet kateg taky 0
  # summarise(pocet = n())

table(prj7kat$projstav, prj7kat$kat_pocet)
# ale ty s 0 kategoriemi jsou téměř všechny v negativních stavech

# check totals
sum(prj7kat$vfp_propl_vyuct, na.rm = T)/1e6
sum(prj7kat$vfp_propl_vyuct[prj7kat$kat_poradi==1], na.rm = T)/1e6

prj7katnuts <- prj7kat %>%
  right_join(prj7mrg) %>%
  mutate(kat_pocet = ifelse(kat_pocet == 0, 1, kat_pocet)) %>%
  mutate(EU = EU/kat_pocet, nar = nar/kat_pocet, celkem = celkem/kat_pocet)

# check sum
sum(prj7katnuts$celkem, na.rm = T)/1e6

# should also check per-project totals in payments with totals in NUTS dataset

# check that number of added rows is reasonable
(nrow(prj7katnuts)-nrow(prj7mrg))/2500/20

prg7katnuts <- prj7katnuts %>%
  group_by(program, econcat_id, econcat_name, NUTS3, year, kat_kod, kat_nazev) %>%
  summarise(EU = sum(EU, na.rm = T), nar = sum(nar, na.rm = T),
            celkem = sum(celkem, na.rm = T)) %>%
  left_join(cis7_progs, by = "program") %>%
  left_join(cis_nuts3) %>%
  ungroup() %>%
  rename(dt_year = rok, op_id = prog_code, region_id = NUTS3,
         op_nazev = prog_name, op_zkr = prog_abbrev,
         oblast_intervence_kod = econcat_id,
         oblast_intervence_nazev = econcat_name,
         fin_vyuct_narodni_verejne = nar,
         fin_vyuct_eu = EU,
         fin_vyuct_verejne = celkem)

sum(prg7katnuts$celkem)/1e6

## Export categorised & regionalised data ----

# write.csv(prj7katnuts, file = "nuts_proj_kat.csv", row.names = F)

write_parquet(prg7katnuts, "data-export/prg7katnuts.parquet")
write_xlsx(prg7katnuts, "data-export/prg7katnuts.xlsx")
write_excel_csv2(prg7katnuts, "data-export/prg7katnuts.csv")

write_parquet(prj7katnuts, "data-export/prj7katnuts.parquet")
write_xlsx(prj7katnuts, "data-export/prj7katnuts.xlsx")
write_excel_csv2(prj7katnuts, "data-export/prj7katnuts.csv")


# Checking ----------------------------------------------------------------

## Try using places of implementation to impute region weights ----

prj7plcNUTS <- prj7plc %>%
  select(projnum = `Číslo projektu`,
         misto_code = `Kód NUTS`,
         misto_nazev = `Název NUTS`,
         misto_pocet = `Počet míst realizace`,
         misto_obyv = `Počet obyvatel celkem`,
         misto_obyvMC = `Počet obyvatel MČ`,
         misto_poradi = `Pořadí v rámci v projektu (filtr)`,
         stav = `Stav projektu`) %>%
  filter(!str_detect(stav, "^N")) %>%
  mutate(NUTS3 = str_sub(misto_code, 1, 5)) %>%
  group_by(projnum, NUTS3) %>%
  mutate(pocetmistvnuts = n(), podilmistvnuts = pocetmistvnuts/misto_pocet) %>%
  select(-misto_code, -misto_nazev, -misto_poradi, -misto_obyv, -misto_obyvMC) %>%
  distinct() %>%
  left_join(prj7nuts %>%
              rename(projnum = Projekt) %>%
              gather("NUTS3","nuts_podil_NUTStable", -projnum) %>%
              filter(nuts_podil_NUTStable != 0)) %>%
  replace_na(list(podilmistvnuts = 0, nuts_podil_NUTStable = 0)) %>%
  mutate(podilmistvnuts = ifelse(is.infinite(podilmistvnuts), 1, podilmistvnuts),
         prog_code = str_sub(projnum, 1,7)) %>%
  left_join(read_csv("ciselniky/nuts3convert.csv")) %>%
  left_join(read_xlsx("ciselniky/ciselnik_programy2007.xlsx")) %>%
  mutate(NUTS3_abbrev2 = ifelse(str_detect(NUTS3, "^PL"), "PL", NUTS3_abbrev),
         prog_abbrev2 = ifelse(str_detect(prog_abbrev, "^ROP"), "ROP", prog_abbrev),
         prog_abbrev2 = ifelse(str_detect(prog_code, "PL"), "OP ČR-PL", prog_abbrev2))

NUTSyvSeznamuProjektu <- prj7plc %>% select(`Kód NUTS`) %>% mutate(kodnuts = str_sub(`Kód NUTS`, 1, 5)) %>%
  select(kodnuts) %>% distinct()

# are we dealing with the same lists of NUTS codes?
NUTSyvNUTStable <- names(prj7nuts)[2:22]
table(NUTSyvSeznamuProjektu$kodnuts, NUTSyvNUTStable)

all(NUTSyvNUTStable %in% NUTSyvSeznamuProjektu$kodnuts)
all(NUTSyvSeznamuProjektu$kodnuts %in% NUTSyvNUTStable)
# so some in project list were not in NUTS ratio table - which ones?
NUTSyvSeznamuProjektu$kodnuts[!(NUTSyvSeznamuProjektu$kodnuts %in% NUTSyvNUTStable)]
# just a Polish region and NA, so that's fine

# are we dealing with the same lists of projects?
PRJvNUTStable <- unique(prj7nuts$Projekt)
PRJvSeznamu <- unique(prj7plc$`Číslo projektu`)
all(PRJvNUTStable %in% PRJvSeznamu)
all(PRJvSeznamu %in% PRJvNUTStable)
# so some in project list were not in NUTS ratio table - which ones?
PRJvNUTStable[!(PRJvNUTStable %in% PRJvSeznamu)]
length(PRJvSeznamu[!(PRJvSeznamu %in% PRJvNUTStable)])
PRJvSeznamu[!(PRJvSeznamu %in% PRJvNUTStable)]
# just a Polish region and NA, so that's fine

# so what is the correlation?

cor(prj7plcNUTS$podilmistvnuts, prj7plcNUTS$nuts_podil_NUTStable)
plot(prj7plcNUTS$podilmistvnuts, prj7plcNUTS$nuts_podil_NUTStable)
table(is.na(prj7plcNUTS$nuts_podil_NUTStable))
table(is.na(prj7plcNUTS$podilmistvnuts))

ggplot(prj7plcNUTS) +
  geom_point(aes(podilmistvnuts, nuts_podil_NUTStable, colour = prog_abbrev2), pch = 19, alpha = 0.2) +
  facet_wrap(~ NUTS3_abbrev2)

ggplot(prj7plcNUTS) +
  geom_point(aes(podilmistvnuts, nuts_podil_NUTStable, colour = NUTS3_abbrev2), pch = 19, alpha = 0.2) +
  facet_wrap(~ prog_abbrev2)

ggplot(prj7plcNUTS) +
  geom_point(aes(podilmistvnuts, nuts_podil_NUTStable), pch = 19, alpha = 0.2) +
  facet_grid(NUTS3_abbrev2 ~ prog_abbrev2)

# It seems that in several programmes, estimating from number of places of
# implementation gives a 50:50 split between two regions, whereas the original
# figure in the data is different - that suggests that the original data
# reflects extra information beyond the split implied by # of places.

# It might even be sensible to use the NUTS shares to infer place shares.

#---- Split project money into places and categories by prg and yr ----

prj7plc2 <- prj7plc %>%
  select(kodmista = `Kód NUTS`, nazevmista = `Název NUTS`,
         projnum = `Číslo projektu`, pocetmist = `Počet míst realizace`) %>%
  mutate(kodkraje = str_sub(kodmista, 1, 5)) %>%
  group_by(projnum, kodkraje, pocetmist) %>%
  summarise(pocetmistvkraji = n())

prj7plc2plat <- prj7plat %>%
  ungroup() %>%
  select(-idvar) %>%
  left_join(prj7plc2) %>%
  mutate()

# summarise by programme and place, per category and year
