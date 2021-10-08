targets::tar_load(macro_sum_codebook)

clmns <- macro_sum_codebook$metadata$columns

df_with_name <- function(item, item_name) {
  x <- as_tibble(item)
  x$name <- item_name
  x
  relocate(x, name)
}

purrr::imap_dfr(clmns, df_with_name)

tbl <- macro_sum_codebook$metadata$table
tbl

print(tbl)

