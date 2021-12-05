compare_dt_sums <- function(datasets, var_pattern, names, group_pattern = NULL) {
  map2_dfr(datasets, names, ~ {
    nm <- ..2
    ..1 |>
      group_by(if(!is.null(group_pattern)) across(matches(group_pattern))) |>
      summarise(across(matches(var_pattern), ~sum(.x, na.rm = T)/1e6), .groups = "drop") |>
      mutate(dataset = nm) |>
      relocate(dataset)
  })
}

options(ggplot2.discrete.fill = palette("Okabe-Ito"))

plot_with_nplus3 <- function(macro_sum_quarterly, macro_sum_nplus3) {
  dt <- macro_sum_quarterly |>
    mutate(typ = "ŽoP") |>
    bind_rows(macro_sum_nplus3 |> rename(dt_zop_rok = dt_nplus3_rok,
                                         fin_vyuct_czv = fin_zbyva_czv) |>
                mutate(typ = "N+3"))

  ggplot(dt, aes(dt_zop_rok, fin_vyuct_czv/1e9, fill = typ)) +
    scale_fill_discrete(type = ggokabeito::palette_okabe_ito(5:9)) +
    geom_col() +
    scale_y_continuous(expand = ptrr::flush_axis) +
    scale_x_continuous(breaks = unique(dt$dt_zop_rok), expand = ptrr::flush_axis) +
    ptrr::theme_ptrr() +
    labs(title = "2014-20, platby a projekce po letech", subtitle = "mld. Kč, CZV")
}

plot_7_annual <- function(s7_sum) {
  s7_sum |>
    count(dt_zop_rok, wt = fin_vyuct_verejne/1e9) |>
    ggplot(aes(dt_zop_rok, n)) +
    geom_col() +
    scale_x_continuous(breaks = unique(s7_sum$dt_zop_rok), expand = ptrr::flush_axis) +
    scale_y_continuous(expand = ptrr::flush_axis) +
    ptrr::theme_ptrr() +
    labs(title = "2007-13, platby po letech", subtitle = "mld. Kč, CZV")
}

plot_nplus3_reg_nonreg <- function(macro_sum_nplus3_reg, macro_sum_nplus3) {
  bind_rows(macro_sum_nplus3_reg |> mutate(typ = "regionální", var = fin_zbyva_czv_wt_cond),
            macro_sum_nplus3 |> mutate(typ = "bez regionů", var = fin_zbyva_czv)) |>
    count(typ, dt_nplus3_rok, wt = var/1e9) |>
    ggplot(aes(dt_nplus3_rok, n, colour = typ)) +
    geom_line() +
    geom_point() +
    ptrr::theme_ptrr() +
    labs(title = "2014-20, srovnání projekcí N+3 v rozpadu na regiony a celkově",
         subtitle = "mld. Kč, CZV")

}
