# BD_ANC_Reproduce

Code for "Changes in Antenatal Care Utilisation (Four or More Visits) in Bangladesh from 2014 to 2022: A Decomposition Analysis" (PLOS ONE, PONE-D-25-63955).

One script, `ANC4_decomposition_R6.R`, reproduces every number, table and figure in the paper from the original DHS files. This replaces the earlier `Rcode for oaxaca decomposition.R` and the three Stata do files.

## Data
Download the complete Individual (women's) Recode Stata files from https://dhsprogram.com (registration required):

| Survey | File |
|---|---|
| BDHS 2014 | BDIR72FL.DTA |
| BDHS 2017-18 | BDIR7RFL.DTA |
| BDHS 2022 | BDIR81FL.DTA |

Use the files exactly as downloaded. Copies saved through software with a variable limit (e.g. older Stata IC, 2,048 variables) are truncated and lack the V5xx-V7xx variables; the script stops with an error if a required variable is missing.

## Run
```
install.packages(c("haven", "survey", "ggplot2"))
Rscript ANC4_decomposition_R6.R BDIR72FL.DTA BDIR7RFL.DTA BDIR81FL.DTA outputs 1000
```
It runs in about two minutes. The seed is fixed, so the bootstrap intervals reproduce exactly.

## Analytic sample
Currently married women aged 15-49 who completed the full Woman's Questionnaire and whose most recent live birth occurred in the 36 months before interview (the reference period of the ANC questions in all three surveys). In 2022 the full questionnaire was administered in a two-thirds subsample of households (19,987 of 30,078 women).

## Definitions
All variable definitions are in the function `build()`. The outcome is four or more ANC visits (`m14_1 >= 4`; 98 = don't know excluded). Higher decision-making autonomy means a favourable response on all three items: participates in own health-care decisions (`v743a` 1-2), rejects wife-beating for refusing sex (`v744d` = 0), and participates in the contraceptive decision (`v632`/`v632a` 1 or 3).

## Outputs (`outputs/`)
| File | Content |
|---|---|
| `sample_flow.csv`, `missing_by_variable.csv` | Sample construction and exclusions |
| `table1_body.tex`, `table1_estimates.csv` | Table 1 |
| `table2_body.tex` | Table 2 (aggregate decomposition) |
| `macros.tex` | Every number quoted in the text |
| `Fig1.tif`, `Fig2.tif` | Figures (400 dpi, LZW) |
| `S1_Table_composition.csv` ... `S4_Table_sensitivity.csv` | Supporting tables |
| `results_summary.txt` | Plain-text summary |

`fill_manuscript.R` inserts `macros.tex`, `table1_body.tex` and `table2_body.tex` into the LaTeX manuscript template.
