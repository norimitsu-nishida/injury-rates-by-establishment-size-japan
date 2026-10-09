# Fatal and reported occupational injury rates by establishment size in Japan, 2009-2025

Code and data that reproduce Tables 1 to 4 of the paper from public statistics.

The numerator is the finalized industrial accident statistics of the Ministry of Health, Labour and Welfare, with fatal injuries and reported injuries involving four or more days of absence from work.
The denominator is regular employees from the Economic Census of the Ministry of Internal Affairs and Communications and the Ministry of Economy, Trade and Industry.
Both are tabulated in six establishment size classes (1-9, 10-29, 30-49, 50-99, 100-299 and 300 or more workers).
Between the six benchmark years of the Census (2009, 2012, 2014, 2016, 2021 and 2024) the denominator is interpolated log-linearly, and 2025 carries the 2024 value forward.

## Running

Run the two scripts in order from this folder with R (tidyverse, readxl and jsonlite).

```
Rscript 01_fetch.R
Rscript 02_analysis.R
```

| File | Contents |
|---|---|
| `01_fetch.R` | Downloads the raw data into `data/` and writes the inventory `data/manifest.csv`. Files already present are skipped |
| `02_analysis.R` | Builds the year by size panel `output/panel_all.csv` from `data/` and prints Tables 1 to 4 |

The downloaded data are included in `data/`, so `02_analysis.R` runs on its own.
An e-Stat appId is needed only to download the Census tables again. Put it in `~/.estat_appid` or in the environment variable `ESTAT_APPID`.

## Data

| File | Contents |
|---|---|
| `data/death_YYYY.xls(x)`, `data/injury_YYYY.xls(x)` | Finalized statistics by industry and establishment size (業種・規模), deaths and reported injuries, 2009-2025 |
| `data/*_2020_excovid.xlsx`, `data/*_2021_excovid.xlsx` | The same tables with occupational COVID-19 removed, published for 2020 and 2021 only |
| `data/estat_reg_YYYY.rds` and others | Economic Census responses of the e-Stat API. reg is regular employees, eng persons engaged and est establishments |
| `data/meta_*.json` | e-Stat class dictionaries (code to name) |
| `data/estat_tables.csv` | Census table ids and the classification codes fixed in each query (such as employment status) |
| `data/manifest.csv` | Source address, retrieval date and MD5 checksum of every file |
| `output/panel_all.csv` | 102 rows of year by size class, with counts and employee-years |

Public administration (官公署) is subtracted from the numerator, because the Economic Census covers privately owned establishments only.
The Census tables are those classified by regular-employee size, and regular employees are selected on the employment status axis.
For 2024 the final results (e-Stat 0004040083) are used.
In 2020 and 2021, `deaths` and `reported_injuries` exclude occupational COVID-19.

## Sensitivity analyses in Table 4

- (a) The published counts for 2020 and 2021, which include occupational COVID-19. `deaths_published` and `reported_injuries_published`
- (b) The six benchmark years only, pooled without interpolation
- (c) Without construction. The numerator subtracts `deaths_construction` and `reported_injuries_construction`, the construction subtotal (建設業小計) of the accident statistics' own industry classification. The denominator, `regular_employee_years_excl_construction`, subtracts division D construction of the Japan Standard Industrial Classification from all industries before interpolation. Subtracting the whole division keeps the establishments whose industry is undetermined below the division (格付不能), which are numerous in 2012, and removes only those in construction
- (d) Persons engaged as the denominator. `persons_engaged_years`

Construction in the accident statistics corresponds to JSIC division D (major groups 06 to 08) under the correspondence table between the industry classification of occupational safety and health statistics and the Japan Standard Industrial Classification (業種区分一覧表, for the 13th revision of JSIC).
The table is not among the public documents of the Ministry of Health, Labour and Welfare, and we used the version published as supplementary material to the Monthly Business Guide, September 2015, by Nihon Horei (https://www.horei.co.jp/linkbg/2015_9bg.pdf).

## License

The code is under the MIT license. The statistical tables included follow the terms of use of each ministry.
