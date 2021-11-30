load_7_platby <- function(dir, path) {

  plat7 <- read_csv(file.path(dir, path), locale = locale(decimal_mark = ","),
                    skip = 1,
                    col_names = c("projnum","projname","platcislo","platbatyp",
                                  "den","platbaid","skutid","castka","zdroj"),
                    col_types = "cccccccnc")

  prj7plat <- plat7 %>%
    mutate(op = str_sub(projnum, 1, 7)) %>%
    mutate(date = as.Date(den, format = "%Y/%m/%d"),
           year = year(date),
           eu_nar = ifelse(str_detect(zdroj,"^EU"),"eu","narodni"),
           idvar = paste(projnum, year, sep = "_")) %>%
    group_by(idvar, projnum, year, eu_nar) %>%
    summarise(castka = sum(castka, na.rm = T)) %>%
    spread(eu_nar, castka) %>%
    select(-idvar) %>%
    arrange(projnum)

  prj7plat
}

load_7_nuts3 <- function(dir, path) {
  read_excel(file.path(dir, path)) %>%
    rename(projnum = Projekt)
}

load_7_kat <- function(dir, path) {
  read_excel(file.path(dir, path),
             col_names = c("prog", "priax", "oblpodp", "projnum", "projname", "projstav",
                           "vfp_propl_vyuct", "eu_propl_vyuct", "nar_propl_vyuct",
                           "tema_kod","tema_nazev", "tema_poradi","tema_pocet"),
             skip = 1)
}

load_7_katekon <- function(dir, paths) {
  map_dfr(file.path(dir, paths), read_excel) %>%
    clean_names() %>%
    select(projnum = cislo_projektu,
           stav_2021 = stav_projektu, hospodarska_cinnost) %>%
    separate(hospodarska_cinnost, c("katekon_id", "katekon_name"), sep = " - ")
}

compile_7_prj <- function(s7_platby, s7_kat, s7_nuts3, s7_katekon,
                          cis7_op, cis7_nuts3) {
  prj7mrg_id <- s7_platby %>%
    left_join(s7_nuts3, by = "projnum") %>%
    gather(key = "NUTScode", value = "podil",
           starts_with("CZ"), starts_with("PL")) %>%
    mutate(fin_vNUTS_EU = eu * podil,
           fin_vNUTS_nar = narodni * podil) %>%
    gather("source", "finance", fin_vNUTS_EU, fin_vNUTS_nar) %>%
    ungroup() %>%
    mutate(source = str_replace(source, "fin_vNUTS_",""),
           idvar = paste(NUTScode, source, sep = "_")) %>%
    select(-eu, -narodni, -NUTScode, -podil, -source)

  prj7mrg <- prj7mrg_id %>%
    separate(idvar, c("NUTS3","zdroj"), sep = "_") %>%
    spread(zdroj, finance) %>%
    replace_na(list(EU = 0, nar = 0)) %>%
    mutate(celkem = EU + nar,
           prog_code = str_sub(projnum, 1, 7)) %>%
    left_join(cis7_op, by = "prog_code") %>%
    left_join(cis7_nuts3, by = "NUTS3") %>%
    left_join(s7_katekon, by = "projnum")

  prj7katnuts <- s7_kat %>%
    right_join(prj7mrg) %>%
    mutate(tema_pocet = ifelse(tema_pocet == 0, 1, tema_pocet)) %>%
    mutate(EU = EU/tema_pocet, nar = nar/tema_pocet, celkem = celkem/tema_pocet) %>%
    rename(fin_vyuct_verejne = celkem, fin_vyuct_eu = EU,
           fin_vyuct_narodni_verejne = nar,
           dt_zop_rok = year, tema_id = tema_kod, tema_name = tema_nazev,
           kraj_id = NUTS3,
           op_id = prog_code,
           op_zkr = prog_abbrev)

  return(prj7katnuts)
}

summarise_7_prg <- function(s7_compiled_prj) {
  s7_compiled_prj %>%
    group_by(dt_zop_rok, op_id, op_zkr,
             across(starts_with("katekon_")),
             tema_id, tema_name, kraj_id) %>%
    summarise(across(starts_with("fin_vyuct"), sum, na.rm = TRUE), .groups = "drop")
}

summarise_7 <- function(s7_sum_prg) {
  s7_sum_prg %>%
    group_by(dt_zop_rok, across(c(starts_with("katekon_"))),
             tema_id, tema_name, kraj_id) %>%
    summarise(across(starts_with("fin_vyuct"), sum, na.rm = TRUE), .groups = "drop")
}

summarise_7_macro <- function(s7_sum_prg, ...) {
  s7_sum_prg %>%
    group_by(dt_zop_rok, kraj_id, quest_class, hermin_class, ...) %>%
    summarise(across(starts_with("fin_vyuct"), sum, na.rm = TRUE), .groups = "drop")
}


# Categorisations ---------------------------------------------------------

categorise_7 <- function(s7_compiled_prj, mc_7_quest, mc_7_hermin) {
  s7_compiled_prj %>%
    left_join(mc_7_quest %>% select(tema_id, quest_class), by = "tema_id") %>%
    left_join(mc_7_hermin %>% select(-katekon_name), by = "katekon_id") %>%
    mutate(hermin_class = if_else(quest_class == "AIS",
                                  paste0(quest_class, AIS_hermin_class),
                                  quest_class)) %>%
    select(-AIS_hermin_class)
}

# Utils -------------------------------------------------------------------



export_0713_kategorie <- function(s7_sum, dir, path) {
  kat <- s7_sum %>%
    count(tema_id, tema_name, wt = fin_vyuct_verejne, name = "fin_vyuct_verejne")
  ekonkat <- s7_sum %>%
    count(katekon_id, katekon_name, wt = fin_vyuct_verejne, name = "fin_vyuct_verejne")
  kombinace <- s7_sum %>%
    count(tema_id, tema_name, katekon_id, katekon_name,
          wt = fin_vyuct_verejne, name = "fin_vyuct_verejne")

  pth <- file.path(dir, path)

  write_xlsx(list(ekonomicke_kategorie = ekonkat,
                  prioritni_temata = kat,
                  kombinace = kombinace), pth)
  return(pth)
}


