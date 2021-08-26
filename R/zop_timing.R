build_efs_timing <- function(efs_prj, efs_zop, ef_pub) {
  prj_timing <- efs_prj %>%
    select(prj_id, starts_with("dt_")) %>%
    distinct() %>%
    left_join(ef_pub %>% select(prj_id, starts_with("dt_real"))) %>%
    mutate(dt_ukon = if_else(is.na(dt_real_ukon_fyz_skut), dt_real_ukon_fyz_predp, dt_ukon_fyz),
           prj_duration = as.duration(dt_ukon - dt_real_zahaj_fyz))

  prj_with_zop <- efs_zop %>%
    add_op_labels() %>%
    mutate(type_zop = str_extract(zop_cislo, "ANTE|POST") %>%
             fct_relevel("POST")) %>%
    left_join(prj_timing) %>%
    mutate(dt_zop_rel = as.duration(dt_zop_proplaceni - dt_real_zahaj_fyz)/prj_duration)

  return(prj_with_zop)
}



make_zop_timing_plot <- function(prj_with_zop) {
  ggplot(prj_with_zop %>%
           # filter(dt_zop_rel < 6) %>%
           # filter(str_detect(real_stav_kod, "PP"))
           filter(TRUE)
           ) +
    geom_jitter(aes(y = type_zop, x = dt_zop_rel, colour = type_zop), alpha = .05) +
    geom_violin(aes(y = type_zop, x = dt_zop_rel), colour = "white", fill = NA) +
    # geom_rug(aes(y = type_zop, x = dt_zop_rel, colour = type_zop), sides = "lb", position = "jitter", alpha = .1) +
    geom_vline(xintercept = 0, colour = "blue") +
    geom_vline(xintercept = 1, colour = "red") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_discrete(type = palette("Okabe-Ito")) +
    scale_colour_discrete(type = palette("Okabe-Ito")) +
    # geom_ysidebar(aes(y = type_zop, colour = type_zop)) +
    facet_wrap(~op_zkr, scales = "free_x") +
    ptrr::theme_ptrr(multiplot = T, gridlines = "x", legend.position = "none", axis_titles = TRUE) +
    labs(title = "Časování plateb ve vztahu k době fyzické realizace projektu",
         subtitle = "Modrá = začátek projektu\nČervená = konec projektu (fyzická realizace)\nCo bod, to platba",
         x = "Relativní doba trvání projektu", y = NULL)
}

