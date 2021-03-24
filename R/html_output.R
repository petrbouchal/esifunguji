t_html <- list(
  tar_file(siteconf, "_site.yml"),
  tar_file(sitefiles, "site"),
  # https://github.com/jdblischak/workflowr/issues/238#issuecomment-782024069
  tar_file(s_index_rmd, "index.Rmd"),
  tar_file(s_index_html, command = {!! tar_knitr_deps_expr("index.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_index_rmd)
    "docs/index.html"}),
  tar_file(s_inputchecks_rmd, "s_inputchecks.Rmd"),
  tar_file(s_inputchecks_html, command = {!! tar_knitr_deps_expr("s_inputchecks.Rmd")
    siteconf
    sitefiles
    rmarkdown::render_site(s_inputchecks_rmd)
    "docs/s_inputchecks.html"})
)
