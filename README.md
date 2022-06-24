esifunguji
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Tento repozitář obsahuje kód na zpracování dat o výdajích financovaných
z ESI fondů v ČR primárně v letech 2014-2020. Cílem je:

-   sestavit dataset použitelný pro modelování dopadů ESI fondů na HDP
    (QUEST, HERMIN - tj. republiková a krajská úroveň, kategorizace
    výdajů podle specifikace modelů)
-   ověřit spolehlivost dat státní pokladny o zdrojích veřejných výdajů
    (u výdajů ústředních orgánů informace o zdroji, u místně řízených
    organizací informace o přijatých transferech)
-   připravit dataset pro vyhodnocení role ESI fondů ve veřejných
    výdajích
-   \[buď zde nebo jinde\] analýza role ESI fondů ve veřejných výdajích

**Vytvořeno pro Úřad vlády ČR jako součást projektu *Systémová podpora
společné evropské politiky podpory a pomoci, ESI a obdobných fondů na
Úřadu vlády ČR* (CZ.08.1.125/0.0/0.0/15_001/0000176)**

**Konečná verze studie vycházející z těcho dat je zveřejněná na webu
[Úřadu
vlády](https://www.vlada.cz/cz/evropske-zalezitosti/analyzy-eu/analyzy-uvod-125732/)**:

Detailnější dokumentace k pipelinu na tvorbu dat pro makro modely:

-   [obsahová dokumentace](s_doc.html)
-   [vstupní validace dat](s_inputchecks.html)
-   [dokumantace a validace výstupu](s_output.html)
-   [technická dokumentace](dev.html)

Relevantní předchozí publikace v této oblasti:

-   [MMR (2017) o roli ESI fondů ve veřejných
    výdajích](https://dotaceeu.cz/cs/evropske-fondy-v-cr/narodni-organ-pro-koordinaci/evaluace/knihovna-evaluaci/verejne-vydaje-a-fondy-eu-2007%e2%80%932015)
    (
    [PDF](https://dotaceeu.cz/Dotace/media/SF/NOK/Evaluace/Evalua%c4%8dn%c3%ad%20knihovna/2017/Adicionalita/Verejne-vydaje-a-fondy-EU-final-public_1.pdf))
-   [Předchozí studie Úřadu vlády (2017) - makroekonomické odhady dopadu
    ESIF na
    ekonomiku](https://www.vlada.cz/assets/evropske-zalezitosti/analyzy-EU/171204_Dopad_ESI_fondu_na_hospodarsky_rust_CR_final.pdf)
-   [Navazující studie Úřadu vlády
    (2018)](https://www.vlada.cz/assets/evropske-zalezitosti/analyzy-EU/Dopad-ESI-fondu-na-hospodarstvi-CR.pdf)
-   [Úřad vlády (2019) přehled využívání makro modelů v zemích
    EU](https://www.vlada.cz/assets/evropske-zalezitosti/aktualne/Vyuzivani-makroevaluaci-a-makroekonomickych-modelu-clenskymi-staty-EU.pdf)

*Pojmenováno s citem k regionu narození jednoho nejmenovaného šéfa
projektového týmu.*

## Dokumentace souborů

-   `esifunguji.Rproj`: konfigurace RStudio projektu
-   `_targets.R`: hlavní soubor definující datový pipeline
-   `_site.yml`: konfigurace webu generovaného uvnitř pipeline do složky
    `docs`
-   `_interactive.R`: utilita pro rychlé načtení objektů pro
    interaktivní práci
-   `build.R`: utilita - spouští pipeline, v RStudio projectu navázáno
    na Build command
-   `*.Rmd`: zdroje webové dokumentace
-   `docs`: vygenerovaná webová dokumentace
-   `data-export`: exporty výstupních dat
-   `data-input`: vstupní data
-   `data-output`: výstupní data ve formátu pro R
-   `data-processed`: mezidata
-   `renv`: skladiště systému renv pro reprodukovatelnost prostředí
    (needitovat ručně)
-   `R`: kód funkcí, které dohromady vytváří pipeline
-   `scripts`: jiný kód mimo pipeline - odkladiště
-   `sp_data`: cache dat Státní pokladny

Detaily v [technické dokumentaci](dev.html).
