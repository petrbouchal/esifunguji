library(targets)
library(tarchetypes)
library(future)


# Config ------------------------------------------------------------------

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "statnipokladna", "here", "readxl",
                            "janitor", "curl", "httr", "stringr", "config",
                            "dplyr", "purrrow", "future", "arrow", "tidyr",
                            "ragg"),
               # debug = "sp_local_arrdir",
               imports = c("purrrow", "statnipokladna"),
               )

options(crayon.enabled = TRUE,
        scipen = 100)

future::plan(multicore)

source("R/utils.R")
source("R/functions.R")

cnf <- config::get(config = "default")


# Public project data -----------------------------------------------------

c_ef_pubxls_url <- cnf$doteu_xls_url

t_public_list <- list(
  tar_url(ef_pubxls_tarurl, c_ef_pubxls_url),
  tar_file(ef_pubxls, curl::curl_download(ef_pubxls_tarurl,
                                          here::here("data-input/ef_publish.xls"))),
  tar_target(ef_pub, read_pubxls(ef_pubxls))
)


# Custom MS sestavy -------------------------------------------------------

sest_dir <- cnf$sest_dir
sest_xlsx_prj <- cnf$sest_xlsx_prj
sest_xlsx_fin <- cnf$sest_xlsx_fin
sest_xlsx_zop <- cnf$sest_xlsx_zop
sest_xlsx_obl <- cnf$sest_xlsx_obl

t_sestavy <- list(
  tar_target(efs_fin, load_efs_fin(sest_dir, sest_xlsx_fin)),
  tar_target(efs_zop, load_efs_zop(sest_dir, sest_xlsx_zop)),
  tar_target(efs_prj, load_efs_prj(sest_dir, sest_xlsx_prj)),
  tar_target(efs_obl, load_efs_obl(sest_dir, sest_xlsx_obl)),
  tar_target(efs_prj_basic, efs_prj %>% select(-starts_with("kat_"),
                                               -starts_with("sc_")) %>%
               distinct()),
  tar_target(efs_prj_sc, efs_prj %>%
               select(prj_id, starts_with("sc_")) %>%
               distinct()),
  tar_target(efs_prj_kat, efs_prj %>%
               select(prj_id, starts_with("kat_")) %>%
               distinct())
  )

# Geographically disaggregated (obce) ESIF data ---------------------------

c_ef_obce_arrowdir <- cnf$c_ef_obce_arrowdir

t_esif_obce <- list(
  tar_file(esif_obce, c_ef_obce_arrowdir)
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

c_sp_years_central_new <- cnf$sp_years_central_new
c_sp_months_central_new <- cnf$sp_months_central_new
c_sp_central_arrowdir_new <- cnf$sp_central_arrowdir_new

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

c_sp_years_local <- cnf$sp_years_local
c_sp_months_local <- cnf$sp_months_local
c_sp_local_arrowdir <- cnf$sp_local_arrowdir

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

c_sp_years_central_old <- cnf$sp_years_central_old
c_sp_months_central_old<- cnf$sp_months_central_old
c_sp_central_arrowdir_old <- cnf$sp_central_arrowdir_old

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

c_sp_local_grants_arrowdir <- cnf$sp_local_grants_arrowdir

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
     t_sp_data_central_old, t_html, t_sp_data_local, t_esif_obce,
     t_sp_data_local_grants, t_sestavy)
