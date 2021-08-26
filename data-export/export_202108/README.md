# data exports

Data exportována v rámci pipeline definované v `./_targets.R`.

Dokumentace automaticky generována a zobrazena v `docs/s_output.html` a exportována do `macro-sum_codebook.yml`.

## Files

- `macro-sum_codebook.yml`: odraz dokumentace (codebooku) výstupních souborů, generováno balíkem `{pointblank}` uvnitř `{targets}` pipeline; zobrazeno v `docs/s_output.html`

### Součty podle krajů

CSV a XLSX soubory mají stejný obsah. 

- `macro-sum_reg_annual.csv`: roční součty
- `macro-sum_reg_annual.xlsx`
- `macro-sum_reg_quarterly.csv`: čtvrletní součty
- `macro-sum_reg_quarterly.xlsx`

