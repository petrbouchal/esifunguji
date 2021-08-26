library(arrow)
library(statnipokladna)
library(tidyverse)
options(scipen = 99)

cnf <- config::get()
ds <- open_dataset(cnf$sp_central_arrowdir_new)

targets::tar_load(sp_cl)
targets::tar_load(compiled_op_sum)

sp_cl$sp_cl_1f1e99ba_zdroj

zdroj <- sp_cl$sp_cl_1f1e99ba_zdroj %>%
  rename(zdroj = zdroj_id)
nastroje_zdroje <- zdroj %>%
  filter(nastroj_id %in% c(cnf$sp_nastroj_ids_1420_ops, cnf$sp_nastroj_ids_1420_prv))

ds2021 <- sp_get_table("budget-central", 2021, 5)

polozka <- sp_cl$sp_cl_52371964_polozka
paragraf <- sp_cl$sp_cl_17831b9f_long

ds %>%
  select(zdroj, budget_spending, period_vykaz, per_yr) %>%
  collect() %>%
  sp_add_codelist(zdroj) %>%
  filter(nastroj_id %in% "107")

ds_vydaje_cons_esifmark <- ds %>%
  select(per_yr, budget_spending, zdroj, polozka, period_vykaz) %>%
  collect() %>%
  bind_rows(ds2021 %>% mutate(per_yr = as.integer(per_yr),
                              period_vykaz = as.character(period_vykaz))) %>%
  sp_add_codelist(zdroj) %>%
  mutate(esif = nastroj_id %in% c(cnf$sp_nastroj_ids_1420_ops,
                                  cnf$sp_nastroj_ids_1420_prv)) %>%
  sp_add_codelist(polozka) %>%
  sp_add_codelist(paragraf, by = "paragraf") %>%
  filter(!kon_pol | kon_rep | kon_okr | kon_kraj)

ds_vydaje_cons_esifmark %>%
  filter(druh == "Výdaje") %>%
  filter(str_detect(zdroj_nazev, "^SR")) %>%
  filter(esif) %>%
  count(nastroj_id, zdroj_nazev, wt = budget_spending/1e9, sort = T) %>%
  # count(nastroj_id, wt = budget_spending/1e9) %>%
  left_join(nastroj_op) %>%
  add_op_labels() %>% View()

compiled_op_sum %>%
  count(wt = fin_vyuct_narodni_verejne/1e9)
