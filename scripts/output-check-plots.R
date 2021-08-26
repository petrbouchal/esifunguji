compiled_macro_prv_annual %>%
  count(prv_operace_kod, prv_operace_nazev, quest_class, wt = fin_vyuct_czv/1e6) %>% View()

library(ggplot2)
compiled_macro_sum_annual %>%
  ggplot(aes(zop_rok, fin_vyuct_czv/1e9, fill = kraj_id)) +
  geom_col() +
  facet_wrap(~quest_class)

compiled_macro_sum_annual %>%
  filter(is.na(quest_class)) %>%
  count(zop_rok, wt = fin_vyuct_czv)

targets::tar_load(efs_zop_quarterly)

efs_zop_quarterly %>%
  ungroup() %>%
  count(zop_rok, zop_kvartal)
