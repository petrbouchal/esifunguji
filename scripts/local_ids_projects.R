ucjed <- sp_cl$ucjed %>%
  left_join(sp_cl$druhuj) %>%
  left_join(sp_cl$poddruhuj) %>%
  distinct(ico, druhuj_id, druhuj_nazev, poddruhuj_id, poddruhuj_nazev)

ico_mro <- ucjed %>%
  filter(druhuj_nazev %in% c("Obce", "Dobrovolné svazky obcí",
                             "Krajské úřady") |
           poddruhuj_nazev %in% c("Příspěvkové organizace zřízené obcí",
                                  "Příspěvkové organizace zřízené MČ",
                                  "Příspěvkové organizace zřízené krajem",
                                  "Školská právnická osoba",
                                  "Městská část")) %>%
  distinct(ico) %>%
  pull(ico)

targets::tar_load(ef_pub)

ucjed %>% distinct(druhuj_nazev, poddruhuj_nazev) %>% View()

ef_mro <- ef_pub %>%
  filter(p_ico %in% ico_mro) %>%
  summarise(across(c(starts_with("czv"), starts_with("vyuct")), ~sum(.x, na.rm = T)/1e9))

