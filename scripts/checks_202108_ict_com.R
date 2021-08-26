library(tidyverse)
library(targets)

targets::tar_load(macrocat_quest)
targets::tar_load(efs_prj)
targets::tar_load(efs_fin)
targets::tar_load(efs_obl)

source("R/functions.R")
source("R/utils.R")

nrow(efs_prj)
n_distinct(efs_prj$prj_id)

prj_kom <- efs_obl %>%
  filter(oblast_intervence_nazev == "Informace a komunikace") %>%
  distinct(prj_id) %>%
  pull(prj_id)

efs_prj %>%
  filter(prj_id %in% prj_kom) %>%
  distinct(prj_id, prj_nazev) %>%
  left_join(efs_fin %>% select(prj_id, fin_vyuct_czv, fin_pravniakt_czv)) %>%
  add_op_labels() %>%
  # filter(op_zkr %in% "IROP")
  # group_by(op_zkr) %>%
  summarise(pocet_projektu = n(),
            nasmlouvane_milczk = round(sum(fin_pravniakt_czv, na.rm = T)/1e6),
            proplacene_milczk = round(sum(fin_vyuct_czv, na.rm = T)/1e6)) %>%
  arrange(desc(nasmlouvane_milczk)) %>%
  gt::gt()

prj_ikt_jine <- efs_obl %>%
  filter(str_detect(oblast_intervence_nazev, "^IKT: jiné druhy")) %>%
  distinct(prj_id) %>%
  pull(prj_id)

efs_prj %>%
  filter(prj_id %in% prj_ikt_jine) %>%
  distinct(prj_id, vyzva_nazev) %>%
  add_op_labels() %>%
  left_join(efs_fin %>% select(prj_id, fin_vyuct_czv, fin_pravniakt_czv)) %>%
  group_by(vyzva_nazev) %>%
  summarise(pocet_projektu = n(),
            nasmlouvane_milczk = round(sum(fin_pravniakt_czv, na.rm = T)/1e6),
            proplacene_milczk = round(sum(fin_vyuct_czv, na.rm = T)/1e6)) %>%
  arrange(desc(nasmlouvane_milczk)) %>%
  gt::gt()

efs_prj %>%
  add_op_labels() %>%
  filter(prj_id %in% prj_ikt_jine) %>%
  distinct(prj_id, prj_nazev) %>%
  count(prj_nazev, sort = T)
