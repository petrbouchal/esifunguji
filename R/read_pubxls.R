##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param path
##' @return
##' @author Petr Bouchal
##' @export
read_pubxls <- function(path) {

  esif <- read_excel(path, skip = 2, col_types = "text") %>%
    slice(-1) %>%
    clean_names() %>%
    mutate(ico = str_pad(ic, width = 8, pad = "0"),
           across(starts_with("celkove"), as.numeric),
           across(contains("datum"), ~as.Date(.x, format = "%d.%m.%Y")),
           across(starts_with("financni"), as.numeric)) %>%
    rename(nazev_prijemce = nazev_zadatele_prijemce_neuvadet_jmena_fyzickych_osob_misto_jmen_budou_krizky_xxx) %>%
    rename_with(~str_replace(.x, "financni_prostredky_vyuctovane_v_zadostech_o_platbu_", "fin_vyuct_")) %>%
    rename_with(~str_replace(.x, "celkove_zpusobile_vydaje_pridelene_na_operaci_", "fin_pravniakt_czv_")) %>%
    rename_with(~str_replace(.x, "_ze_strany_unie", "_eu")) %>%
    rename_with(~str_replace(.x, "soukrome_zdroje_czk", "czk")) %>%
    rename_with(~str_replace(.x, "celkove_zpusobile_vydaje", "czv")) %>%
    rename_with(~str_replace(.x, "_verejne_cr", "_narodni_verejne")) %>%
    rename_with(~str_replace(.x, "_narodni_soukrome", "_soukr")) %>%
    rename_with(~str_replace(.x, "_prispevek_unie", "_eu")) %>%
    rename_with(~str_remove(.x, "_zdroje")) %>%
    rename_with(~str_remove(.x, "_czk$")) %>%
    rename(prj_id = registracni_cislo_projektu_operace,
           op_zkr = program,
           pomer_eu = mira_spolufinancovani_eu,
           prj_nazev = nazev_projektu_nazev_operace,
           prj_priorita_nazev = nazev_prioritni_osy_priority_unie,
           prj_nuts3_nazev = nazev_nuts_3,
           prj_nuts3_kod = kod_nuts_3,
           prj_shrnuti = shrnuti_operace,
           prj_fond = fond,
           p_ico = ico,
           p_psc = psc_prijemce,
           p_forma = pravni_forma_prijemce,
           p_ic = ic,
           p_nazev = nazev_prijemce,
           dt_real_zahaj_fyz = datum_zahajeni_fyzicke_operace,
           dt_real_ukon_fyz_skut = skutecne_datum_ukonceni_fyzicke_realizace_operace,
           dt_real_ukon_fyz_predp = predpokladane_datum_ukonceni_fyzicke_realizace_operace,
           real_stav = stav_realizace
           ) %>%
    mutate(op_zkr = recode(op_zkr, `OP ŽP` = "OP ZP"),
           ) %>%
    add_op_labels()
  return(esif)
}
