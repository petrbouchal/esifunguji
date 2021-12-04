make_macro_sum_codebook <- function(compiled_macro_sum_quarterly) {
  create_informant(tbl = compiled_macro_sum_quarterly,
                   label = "Codebook hlavního výstupu") %>%
    info_tabular(Info = "Tabulka se součty výdajů podle času, kraje, kategorie QUEST/HERMIN, TC DoP a cíle EU2020",
                 Note = "Platí pro data po kvartálech a analogicky i pro roční data\nPro data z ŽOP i pro projekci N+3",
                 `Celková struktura` = "dlouhý formát: čas a kraj jsou v řádcích, metadata a jednotlivé zdroje financí jsou ve sloupcích",
                 `Názvy proměnných` = "platí i pro ostatní datové sady v pipeline:\n- `dt_`: proměnné časového určení\n- `fin_`: finanční údaje") %>%
    info_columns("eu20_id",
                 Popis = "Cíl EU 2020",
                 Zdroj = "Matice cílů od NOK") %>%
    info_columns("quest_class",
                 Popis = "Kategorie pro QUEST") %>%
    info_columns("hermin_class",
                 Popis = "Kategorie pro HERMIN") %>%
    info_columns(matches("(czv|eu|sr|sf|obec|kraj|soukr|jine_nar_ver|narodni|narodni_verejne)$"),
                 Pozor = "Neváženo podle území - v exportech s rozpady na kraje nesčítat!") %>%
    info_columns("source",
                 Popis = "Zdroj dat (MSSF/MSEU nebo SZIF (PRV))") %>%
    info_columns("kraj_id", Popis = "Kód NUTS3 (kraj)") %>%
    info_columns(contains("dt_zop_rok"),
                 Popis = "Rok proplacení ŽOP",
                 `Zdroj dat` = "Datum proplacení ŽOP") %>%
    info_columns(contains("dt_nplus3_rok"),
                 Popis = "Rok N+3 - projekce",
                 `Zdroj dat` = "Finace do let rozloženy lineárně") %>%
    info_columns(contains("dt_zop_kvartal"),
                 Popis = "Kvartál proplacení ŽOP",
                 `Zdroj dat` = "Datum proplacení ŽOP") %>%
    info_columns(contains("dt_zop_kvartal_datum"),
                 Popis = "První den kvartálu",
                 `Zdroj dat` = "Datum proplacení ŽOP") %>%
    info_columns(starts_with("dt_"), Typ = "Časový údaj (datum)") %>%
    info_columns(starts_with("fin_"), Typ = "Finance",
                 Jednotka = "CZK",
                 `Zdroj dat` = "ŽOP sečtené podle data proplacení") %>%
    info_columns(contains("_czv"), `Zdroj financí` = "Celkové způsobilé výdaje") %>%
    info_columns(contains("_eu"), `Zdroj financí` = "Příspěvek Unie") %>%
    info_columns(contains("_sr"), `Zdroj financí` = "Státní rozpočet") %>%
    info_columns(contains("_sf"), `Zdroj financí` = "Státní fondy") %>%
    info_columns(contains("_obec"), `Zdroj financí` = "Obec") %>%
    info_columns(contains("_kraj"), `Zdroj financí` = "Kraj") %>%
    info_columns(contains("_soukr"), `Zdroj financí` = "Soukromý") %>%
    info_columns(contains("_jine_nar"), `Zdroj financí` = "Jiné národní") %>%
    info_columns(contains("_narodni"), `Zdroj financí` = "Národní") %>%
    info_columns(contains("_verejne"), `Zdroj financí` = "Veřejné (EU + ČR)") %>%
    info_columns(contains("fin_zbyva_"), `Typ informace` = "Projekce N+3") %>%
    info_columns(ends_with("_wt_pocetkraju"),
                 Váha = "1/počet krajů, kde se projekt realizuje") %>%
    info_columns(ends_with("_wt_cond"),
                 Váha = "Váha zvolena podle proměnné `class_quest`",
                 Detail = "podle krajů pro RD, AIS, TA; podle počtu obyvatel obcí pro INFR a HC") %>%
    info_columns(ends_with("_wt_pocetobyv"),
                 Váha = "Počet obyvatel obcí, kde se projekt realizuje")
}

make_0713_codebook <- function(s7_sum_macro_detail) {
  create_informant(tbl = s7_sum_macro_detail,
                   label = "Codebook výstupu pro období 2007-13") %>%
    info_tabular(Info = "Tabulka se součty výdajů podle let, kraje, prioritního tématu a ekonomické kategorie",
                 `Celková struktura` = "dlouhý formát: čas a kraj jsou v řádcích, metadata a jednotlivé zdroje financí jsou ve sloupcích",
                 `Názvy proměnných` = "platí i pro ostatní datové sady v pipeline:\n- `dt_`: proměnné časového určení\n- `fin_`: finanční údaje") %>%
    info_columns("kraj_id", Popis = "Kód NUTS3 (kraj)",
                 Zdroj = "sestava podílů jednotlivých NUTS3 na realizaci projektů") %>%
    info_columns("dt_zop_rok",
                 Popis = "Rok proplacení ŽOP",
                 `Zdroj` = "Datum proplacení ŽOP ze sestavy plateb") %>%
    info_columns("tema_id", Info = "Kód prioritního tématu",
                 Poznámka = "Prioritní téma je obdoba oblasti intervence v 2014-20") %>%
    info_columns("tema_name", Info = "Název prioritního tématu",
                 Poznámka = "Prioritní téma je obdoba oblasti intervence v 2014-20") %>%
    info_columns("katekon_id", Info = "Kód ekonomické oblasti",
                 Zdroj = "Sestava vygenerovaná nověv v roce 2021") %>%
    info_columns("katekon_name", Info = "Název ekonomické oblasti",
                 Zdroj = "Sestava vygenerovaná nověv v roce 2021") %>%
    info_columns("fin_vyuct_eu", Info = "EU příspěvek") %>%
    info_columns("fin_vyuct_narodni_verejne", Info = "Národní veřejné výdaje (kofinancování)",
                 Zdroj = "Sestava plateb") %>%
    info_columns("fin_vyuct_verejne", Info = "Veřejné výdaje (EU + národní veřejné) celkem",
                 Pozor = "Narozdíl od období 2014-20 nemáme údaje o soukromém kofinancování",
                 Zdroj = "Sestava plateb") %>%
    info_columns("quest_class", Info = "Kategorie pro model QUEST",
                 Zdroj = "Napojeno z ručně vyrobené kategorizace v excelu ve vstupních datech") %>%
    info_columns("hermin_class", Info = "Kategorie pro model HERMIN",
                 Note = "Seskupuje prioritní témata do kategorií",
                 Zdroj = "Napojeno z ručně vyrobené kategorizace v excelu ve vstupních datech",
                 Note = "Vychází z QUEST kategorií, ale rozpadá kategorii AIS na víc podle ekonomické činnosti") %>%
    info_columns("op_id", Info = "Kód programu") %>%
    info_columns("op_zkr", Info = "Zkratka názvu programu")
}
