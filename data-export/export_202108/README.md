# data exports

Data exportována v rámci pipeline definované v `./_targets.R`.

Dokumentace automaticky generována a zobrazena v `docs/s_output.html` a exportována do `macro-sum_codebook.yml`.

## Files

CSV a XLSX soubory mají stejný obsah. 

- `macro-sum_annual.csv`: roční součty
- `macro-sum_annual.xlsx`
- `macro-sum_quarterly.csv`: čtvrletní součty
- `macro-sum_quarterly.xlsx`

- `macro-sum_codebook.yml`: odraz dokumentace (codebooku) výstupních souborů, generováno balíkem `{pointblank}` uvnitř `{targets}` pipeline; zobrazeno v `docs/s_output.html`
