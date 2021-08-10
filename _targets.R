library(targets)
library(tarchetypes)
library(future)

# Config ------------------------------------------------------------------

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "statnipokladna", "here", "readxl",
                            "janitor", "curl", "httr", "stringr", "config",
                            "dplyr", "purrrow", "future", "arrow", "tidyr",
                            "ragg", "magrittr", "czso", "lubridate", "writexl",
                            "readr", "purrr", "pointblank"),
               # debug = "compiled_macro_quarterly",
               imports = c("purrrow", "statnipokladna"),
)

options(crayon.enabled = TRUE,
        scipen = 100)

future::plan(multicore)

source("R/utils.R")
source("R/functions.R")

cnf <- config::get(config = "default")
names(cnf) <- paste0("c_", names(cnf))
list2env(cnf, envir = .GlobalEnv)

# Public project data -----------------------------------------------------

t_public_list <- list(
  tar_download(ef_pubxls, c_ef_pubxls_url,
               here::here("data-input/ef_publish.xls")),
  tar_target(ef_pub, read_pubxls(ef_pubxls))
)


# Custom MS sestavy -------------------------------------------------------

t_sestavy <- list(
  # finanční pokrok
  tar_target(efs_fin, load_efs_fin(c_sest_dir, c_sest_xlsx_fin)),
  # seznam ŽOPek
  tar_target(efs_zop, load_efs_zop(c_sest_dir, c_sest_xlsx_zop)),
  # základní info o projektech
  # obsahuje ekonomické kategorie intervence, SC atd.
  tar_target(efs_prj, load_efs_prj(c_sest_dir, c_sest_xlsx_prj)),
  # oblasti intervence
  tar_target(efs_obl, load_efs_obl(c_sest_dir, c_sest_xlsx_obl)),
  # výřes základních informací o projektech
  tar_target(efs_prj_basic, efs_prj %>% select(-starts_with("katekon_"),
                                               -starts_with("sc_")) %>%
               distinct()),
  # specifické cíle
  # bez rozpadu na kategorie intervence
  # protože ten je v datech nepřiznaný
  tar_target(efs_prj_sc, efs_prj %>%
               select(prj_id, starts_with("sc_")) %>%
               distinct()),
  # kategorie intervence, bez rozpadu na SC
  tar_target(efs_prj_kat, efs_prj %>%
               select(prj_id, starts_with("katekon_")) %>%
               distinct() %>%
               group_by(prj_id) %>%
               mutate(katekon_podil = 1/n())),
  # sečíst ŽOP za každý projekt po letech
  tar_target(efs_zop_annual, summarise_zop(efs_zop, quarterly = FALSE)),
  # a po čtvrtletích
  tar_target(efs_zop_quarterly, summarise_zop(efs_zop, quarterly = TRUE)),
  # načíst PRV
  tar_target(efs_prv, load_prv(c_prv_data_path, cis_kraj)),
  # posčítat platby PRV za projekt po letech
  tar_target(efs_prv_annual, summarise_prv(efs_prv, quarterly = FALSE)),
  # a PRV po čtvrtletích
  tar_target(efs_prv_quarterly, summarise_prv(efs_prv, quarterly = TRUE))
)

# Geographically disaggregated (obce) ESIF data ---------------------------

t_esif_obce <- list(
  tar_file(esif_obce, c_ef_obce_arrowdir),
  # vazby ZÚJ a obcí, abychom mohli ZÚJ v datech
  # převést na obce
  tar_target(zuj_obec, get_zuj_obec()),
  # číselník krajů pro vložení kódu kraje v PRV
  tar_target(cis_kraj, czso::czso_get_codelist("cis100")),
  # populace obcí pro vážení projektů mezi kraji
  tar_target(pop_obce, get_stats_pop_obce(c_czso_pop_table_id)),
  # spočítat podíly krajů na každém projektu
  tar_target(eo_kraj, summarise_geo_by_kraj(esif_obce, pop_obce, zuj_obec))
)


# Modeling categorisations ------------------------------------------------

# dodané a ručně upravené vazby
t_cats <- list(
  # QUEST kategorie <=> oblast intervence
  tar_target(macrocat_quest, load_macrocat_quest(c_mc_xlsx_q, prv = FALSE)),
  # QUEST a HERMIN <=> typ operace v PRV (u PRV máme HERMIN a QUEST v jedné tabulce)
  tar_target(macrocat_prv, load_macrocat_quest(c_mc_xlsx_prv, prv = TRUE)),
  # HERMIN podkategorie QUEST AIS <=> kategorie intervence
  tar_target(macrocat_hermin, load_macrocat_hermin(c_mc_xlsx_h)),
  # rozkategorizovat všechna data
  tar_target(efs_macrocat, build_efs_macrocat(efs_prj_kat, efs_obl,
                                              macrocat_quest,
                                              macrocat_hermin))
)


# Compile for macro -------------------------------------------------------

t_macro_compile <- list(
  # rozpad na kraj x platby po letech x makro kategorie => sečíst
  tar_target(compiled_macro_quarterly,
             compile_ef(efs_macrocat, eo_kraj, efs_zop_quarterly)),
  # U PRV jen stačí navázat makro kategorizaci a sečíst po letech/kvartálech
  tar_target(compiled_macro_prv_quarterly,
             compile_ef_prv(efs_prv_quarterly, macrocat_prv)),
  # spojit PRV a ostatní po letech
  tar_target(compiled_macro_sum_annual,
             summarise_macro(compiled_macro_quarterly,
                             compiled_macro_prv_quarterly,
                             quarterly = FALSE)),
  # spojit PRV a ostatní po kvartálech
  tar_target(compiled_macro_sum_quarterly,
             summarise_macro(compiled_macro_quarterly,
                             compiled_macro_prv_quarterly,
                             quarterly = TRUE))
)


# Compile by OP -----------------------------------------------------------

t_op_compile <- list(
  tar_target(compiled_op_sum,
             summarise_by_op(efs_zop_quarterly, efs_prv_quarterly)))


# Export data for macro models --------------------------------------------

t_macro_export <- list(
  tar_file(macro_export_annual_csv,
           export_table(compiled_macro_sum_annual,
                        here::here(c_macro_export_dir, c_macro_export_csv_a),
                        write_excel_csv2)),
  tar_file(macro_export_quarterly_csv,
           export_table(compiled_macro_sum_quarterly,
                        here::here(c_macro_export_dir, c_macro_export_csv_q),
                        write_excel_csv2)),
  tar_file(macro_export_annual_excel,
           export_table(compiled_macro_sum_annual,
                        here::here(c_macro_export_dir, c_macro_export_xlsx_a),
                        write_xlsx)),
  tar_file(macro_export_quarterly_excel,
           export_table(compiled_macro_sum_quarterly,
                        here::here(c_macro_export_dir, c_macro_export_xlsx_q),
                        write_xlsx))
)


# Build and export codebook -----------------------------------------------

t_macro_codebook <- list(
  tar_target(macro_sum_codebook,
             make_macro_sum_codebook(compiled_macro_sum_quarterly)),
  tar_file(macro_sum_codebook_yaml,
           {pointblank::yaml_write(informant = macro_sum_codebook %>%
                                    pointblank::set_read_fn(read_fn = ~compiled_macro_sum_quarterly),
                                  path = c_macro_export_dir,
                                  filename = c_macro_export_cdbk)
              file.path(c_macro_export_dir, c_macro_export_cdbk)
             })
)

# Objective/category hierarchy --------------------------------------------

# vychází z tabulky vazeb cílů ESIF/OP/NPR/EU2020

t_hier <- list(
  tar_file(ef_hier_path, c_hier_excel_path),
  tar_target(ef_hier_raw, load_hierarchy(c_hier_excel_path, c_hier_excel_sheet)),
  tar_target(ef_hier, process_hierarchy(ef_hier_raw, efs_prj))
)

# Statnipokladna - číselníky ----------------------------------------------

# keep downloaded data in project directory
options(statnipokladna.dest_dir = "sp_data")
codelist_names <- c("druhuj", "poddruhuj", "nuts",
                    "paragraf", "paragraf_long",
                    "ucelznak", "nastroj", "nastrojanal",
                    "polozka", "polvyk", "zdroj", "ucjed")

t_sp_codelists <- list(
  tar_target(codelists, codelist_names),

  # track changes at URL via {targets}/{tarchetypes}

  tar_url(sp_cl_urls, sp_get_codelist_url(codelists),
          pattern = map(codelists)),

  # track changes to file: if deleted/changed, redownload it

  tar_file(sp_cl_paths,
           sp_get_codelist_file(url = sp_cl_urls),
           pattern = map(sp_cl_urls)),

  # keep all codelists in one list tracked by {targets}:

  tar_target(sp_cl, sp_load_codelist_named(sp_cl_paths),
             pattern = map(sp_cl_paths)),
  tar_file(sp_clp, codelist_to_parquet(sp_cl, codelists, "codelists"),
           pattern = map(sp_cl, codelists))

)

# Central state budget data -----------------------------------------------

# read_tarrow(target_name)

t_sp_data_central_new <- list(
  tar_target(d_years, c_sp_years_central_new),
  tar_target(d_years_ptrn, d_years, pattern = map(d_years)),
  tar_target(d_months, c_sp_months_central_new),
  tar_target(d_id, "misris"),
  tar_url(d_url, sp_get_dataset_url(d_id, d_years_ptrn, d_months),
          pattern = map(d_years_ptrn)),
  tar_file(d_file, {is.character(d_url)
    sp_get_dataset(d_id, d_years, d_months)},
    pattern = map(d_years)), # to make sure target runs when data at URL changes
  tar_target(table_file, sp_get_table_file("budget-central", d_file),
             format = "file", pattern = map(d_file)),
  tar_target(sp_central_new_arrdir,
             budget_arrow_months(table_file, c_sp_central_arrowdir_new,
                                 c_sp_months_central_new, load_budget_yearsum_central_new,
                                 codelists = sp_cl),
             format = "file"),
  tar_target(sp_central_new_ops, budget_new_ops(sp_central_new_arrdir, nastroj_op, sp_cl))
)

# Local budget data -----------------------------------------------

# read_tarrow(target_name)

t_sp_data_local <- list(
  tar_target(d_years_l, c_sp_years_local),
  tar_target(d_years_l_ptrn, d_years_l, pattern = map(d_years_l)),
  tar_target(d_months_l, c_sp_months_local),
  tar_target(d_id_l, "finm"),
  tar_url(d_url_l, sp_get_dataset_url(d_id_l, d_years_l_ptrn, d_months_l),
          pattern = map(d_years_l_ptrn)),
  tar_file(d_file_l, {is.character(d_url_l)
    sp_get_dataset(d_id_l, d_years_l, d_months_l)},
    pattern = map(d_years_l)), # to make sure target runs when data at URL changes
  tar_target(table_file_l, sp_get_table_file("budget-local", d_file_l),
             format = "file", pattern = map(d_file_l)),
  tar_target(sp_local_arrdir,
             budget_arrow_months(table_file_l, c_sp_local_arrowdir,
                                 c_sp_months_local, load_budget_yearsum_local,
                                 codelists = sp_cl),
             format = "file")
)

# Pre-2015 central budget data --------------------------------------------

t_sp_data_central_old <- list(
  tar_target(d_years_o, c_sp_years_central_old),
  tar_target(d_years_o_ptrn, d_years_o, pattern = map(d_years_o)),
  tar_target(d_months_o, c_sp_months_central_old),
  tar_target(d_id_o, "finu"),
  tar_url(d_url_o, sp_get_dataset_url(d_id_o, d_years_o_ptrn, d_months_o),
          pattern = map(d_years_o_ptrn)),
  tar_file(d_file_o, {is.character(d_url_o)
    sp_get_dataset(d_id_o, d_years_o, d_months_o)},
    pattern = map(d_years_o)), # to make sure target runs when data at URL changes
  tar_target(table_file_o, sp_get_table_file("budget-central-old", d_file_o),
             format = "file", pattern = map(d_file_o)),
  tar_target(sp_central_old_arrdir,
             budget_arrow_months(table_file_o, c_sp_central_arrowdir_old,
                                 c_sp_months_central_old, load_budget_yearsum_central_old,
                                 codelists = sp_cl),
             format = "file")
)

# Local budgets - grants --------------------------------------------------

t_sp_data_local_grants <- list(
  tar_target(table_file_lg, sp_get_table_file("budget-local-purpose-grants",
                                              d_file_l),
             format = "file", pattern = map(d_file_l)),
  tar_target(sp_local_grants_arrdir,
             budget_arrow_months(table_file_lg, c_sp_local_grants_arrowdir,
                                 c_sp_months_local, load_budget_local_grants,
                                 codelists = sp_cl),
             format = "file")
)


# HTML output -------------------------------------------------------------

source("R/html_output.R")


# Compile targets lists ---------------------------------------------------

list(t_public_list, t_sp_codelists, t_sp_data_central_new,
     t_cats,
     t_sp_data_central_old, t_html, t_sp_data_local, t_esif_obce,
     t_sp_data_local_grants, t_sestavy, t_hier,
     t_op_compile,
     t_macro_compile, t_macro_export, t_macro_codebook)
