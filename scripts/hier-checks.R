targets::tar_load(ef_hier)
targets::tar_load(efs_prj_sc)

eu1_sc <- ef_hier %>%
  filter(eu20_id == "EU2020_1") %>%
  pull(sc_id)

efs_prj_sc %>%
  group_by(prj_id) %>%
  mutate(n = n()) %>%
  mutate(eu1 = sc_id %in% eu1_sc,
         any_eu1 = any(eu1)) %>%
  filter(any_eu1 & n > 1) %>%
  count(prj_id, wt = sc_podil) %>%
  arrange(n)

efs_prj_sc %>%
  left_join(ef_hier) %>%
  group_by(prj_id, eu20_id) %>%
  summarise(sum_wt = sum(sc_podil)) %>%
  group_by(eu20_id) %>%
  filter(sum_wt == min(sum_wt) & sum_wt < 1) %>%
  add_op_labels() %>%
  View()
