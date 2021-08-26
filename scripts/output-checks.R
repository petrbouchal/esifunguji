targets::tar_load(efs_zop)
targets::tar_load(efs_zop_quarterly)
targets::tar_load(efs_zop_annual)
targets::tar_load(efs_prv)
targets::tar_load(ef_pub)
targets::tar_load(compiled_macro_sum_quarterly)

library(tidyverse)
library(lubridate)

zz <- efs_zop %>%
  mutate(rk = str_extract(zop_cislo, "(?<=/)20[12][0-9](?=/)"),
         midyear = as.Date(make_date(rk, 6, 30)),
         diff = as.difftime(dt_zop_proplaceni - midyear))

mean(zz$diff, na.rm = T)

efs_zop %>%
  mutate(opid = str_sub(prj_id, 1, 5)) %>%
  group_by(missing_date = is.na(dt_zop_proplaceni), opid, ) %>%
  summarise(pocet = n(), czv = sum(fin_vyuct_czv))

efs_zop_quarterly %>%
  count(dt_zop_rok, dt_zop_kvartal, timing_inferred, wt = fin_vyuct_czv) %>%
  spread(timing_inferred, n) %>%
  arrange(dt_zop_rok, dt_zop_kvartal)


efs_zop_quarterly %>% count(wt = fin_vyuct_czv/1e6)
efs_zop_quarterly %>% count(wt = fin_vyuct_eu/1e6)
efs_zop %>% count(wt = fin_vyuct_czv/1e6)
efs_zop %>% count(wt = fin_vyuct_czv/1e9) %>% pull() + sum(efs_prv$fin_vyuct_czv)/1e9

compiled_macro_sum_quarterly %>% count(wt = fin_vyuct_czv_wt_pocetkraju/1e6)
compiled_macro_sum_quarterly %>% count(wt = fin_vyuct_czv_wt_pocetobyv/1e6)

compiled_macro_sum_quarterly %>% count(source, wt = fin_vyuct_czv_wt_pocetobyv/1e6)
compiled_macro_sum_quarterly %>% count(source, wt = fin_vyuct_czv_wt_pocetkraju/1e6)
compiled_macro_sum_quarterly %>% count(source, wt = fin_vyuct_eu_wt_pocetobyv/1e6)
compiled_macro_sum_quarterly %>% count(source, wt = fin_vyuct_eu_wt_pocetkraju/1e6)

efs_zop_quarterly %>%
  count(timing_inferred, wt = fin_vyuct_czv/1e6)

sum(ef_pub$fin_vyuct_v_zop_czv)/1e6

compiled_macro_sum_quarterly %>%
  mutate(date = make_date(zop_rok, 3*zop_kvartal-2)) %>%
  ggplot(aes(date, fin_vyuct_czv_wt_pocetkraju/1e9,fill = hermin_class)) +
  geom_col() +
  facet_wrap(~zop_kvartal)
