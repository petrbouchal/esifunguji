get_stats_pop_obce <- function(czso_pop_table_id) {
  pop_all <- czso::czso_get_table(czso_pop_table_id)

  pop_all %>%
    filter(is.na(pohlavi_kod), vuzemi_cis == "43", rok >= 2014) %>%
    # select(hodnota, rok, geo_id = vuzemi_kod)
    select(hodnota, rok, obec_id = vuzemi_kod) %>%
    group_by(obec_id) %>%
    summarise(pocob_stred201420 = mean(hodnota))
}

get_zuj_obec <- function() {
  czso::czso_get_codelist("cis43vaz51") %>%
    select(geo_id = CHODNOTA2, obec_id = CHODNOTA1)
}

summarise_geo_by_kraj <- function(path, pop_obce, zuj_obec) {
  ds <- open_dataset(path)

  dsk <- ds %>%
    select(prj_id, geo_id_long, geo_id) %>%
    collect() %>%
    distinct() %>%
    mutate(kraj_id = str_extract(geo_id_long, "CZ[0-9]{3}")) %>%
    # translate ZUJ to obce
    left_join(zuj_obec, by = "geo_id") %>%
    # where geo_id was a city ID (!= zuj ID), use this
    mutate(obec_id = if_else(is.na(obec_id), geo_id, obec_id)) %>%
    # drop multiple ZUJ per project, only use obec level
    distinct(prj_id, obec_id, kraj_id) %>%
    # add population information
    left_join(pop_obce, by = "obec_id") %>%
    # how many obce in total in each project?
    add_count(prj_id, name = "obyv_v_obcich", wt = pocob_stred201420) %>%
    group_by(prj_id, kraj_id, obyv_v_obcich) %>%
    # how many obce in each kraj in each project?
    summarise(obci_v_kraji = n(),
              # now weighted by population
              obyv_v_obcich_v_kraji = sum(pocob_stred201420),
              .groups = "drop") %>%
    # how many obce in each project in total?
    add_count(prj_id, name = "obci_celkem", wt = obci_v_kraji) %>%
    # calculate kraj shares in each project
    # one weighted by number of obce
    mutate(kraj_podil_wtpocetobci = obci_v_kraji/obci_celkem,
           # another weighted by population in those obce
           kraj_podil_wtpocetobyv = obyv_v_obcich_v_kraji/obyv_v_obcich) %>%
    group_by(prj_id) %>%
    mutate(kraj_podil_wtpocetkraju = 1/n())

  return(dsk)
}

# dsa <- ds %>% collect()
#
# dsa %>%
#   distinct(prj_id, geo_id_long) %>%
#   nrow()

#
# nogeo <- ds %>%
#   filter(is.na(geo_id)) %>%
#   select(prj_id) %>%
#   collect()
#
# dsng <- ds %>%
#   select(-starts_with("geo")) %>%
#   filter(radek == 1) %>%
#   collect()
#
# ds %>%
#   filter(is.na(geo_id_long) & !is.na(geo_id)) %>%
#   select(prj_id, starts_with("rozpad")) %>% collect()
#
# dsnokraj <- dsa %>%
#   mutate(kraj = str_extract(geo_id_long, "CZ[0-9]{3}")) %>%
#   filter(is.na(kraj))
#
# dsnokraj %>%
#   count(rozpad_typ, rozpad_duvod)
#
# dso <- ds %>%
#   select(prj_id, geo_id_long, geo_id, geo_id_orig) %>%
#   collect() %>%
#   mutate(kraj = str_extract(geo_id_long, "CZ[0-9]{3}")) %>%
#   filter(is.na(kraj)) %>%
#   mutate(op_id = str_sub(prj_id, 4, 5))
#
# table(is.na(dso$kraj))
#
# targets::tar_load(ts$efs_prj)
#
# table(unique(efs_prj$prj_id) %in% unique(dsa$prj_id))
#
# efs_prj %>%
#   filter(!prj_id %in% dso$prj_id) %>%
#   mutate(pneg = str_detect(real_stav_kod, "PN")) %>%
#   count(op_nazev, pneg) %>%
#   spread(pneg, n)
#
# efs_prj %>%
#   mutate(pneg = str_detect(real_stav_kod, "PN"),
#          vyzva_rok = str_extract(vyzva_id, "_[0-9]{1,2}_"),
#          zadost_rok = lubridate::year(dt_zadost)) %>%
#   filter(!prj_id %in% dso$prj_id,
#          !pneg,
#          op_id != "11",
#          zadost_rok < 2020,
#          # !is.na(dt_ukon_fyz),
#          ) %>%
#   filter(!prj_id %in% dsa$prj_id) %>%
#   count(op_id, zadost_rok, sort = T)
