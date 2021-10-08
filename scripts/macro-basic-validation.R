library(tidyverse)

targets::tar_load(starts_with("macro_sum"))
targets::tar_load(efs_zop)
targets::tar_load(efs_prv)

macro_sum_reg_quarterly %>% count(source, wt = fin_vyuct_czv_wt_cond)
macro_sum_reg_quarterly %>% count(wt = fin_vyuct_czv_wt_cond)
macro_sum_reg_annual %>% count(source, wt = fin_vyuct_czv_wt_cond)
macro_sum_reg_annual %>% count(wt = fin_vyuct_czv_wt_cond)
macro_sum_annual %>% count(wt = fin_vyuct_czv)
macro_sum_annual %>% count(source, wt = fin_vyuct_czv)
macro_sum_quarterly %>% count(source, wt = fin_vyuct_czv)
macro_sum_quarterly %>% count(wt = fin_vyuct_czv)

efs_zop %>% count(wt = fin_vyuct_czv)
efs_prv %>% count(wt = fin_vyuct_czv)

compiled_macro_sum_reg_quarterly %>%
  count(eu20_id, wt = fin_vyuct_czv_wt_cond)
