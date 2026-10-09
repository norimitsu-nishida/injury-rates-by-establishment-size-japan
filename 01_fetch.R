library(tidyverse)
library(jsonlite)

# Fatal and reported occupational injury rates by establishment size in Japan, 2009-2025
# Downloads the raw data into data/ and writes the inventory data/manifest.csv. Files already there are skipped.
# An e-Stat appId is needed only to download the census tables (~/.estat_appid or ESTAT_APPID).

DATA <- "data"
dir.create(DATA, showWarnings = FALSE)

YEARS     <- 2009:2025
BENCHMARK <- c(2009, 2012, 2014, 2016, 2021, 2024)

## Finalized accident statistics (MHLW)-----
# NOTE: one workbook per year for deaths (sibou) and reported injuries (sisyou)
BASE <- "https://anzeninfo.mhlw.go.jp/user/anzen/tok/"

kakutei <- tibble(kind = rep(c("death", "injury"), each = length(YEARS)), year = rep(YEARS, 2)) |>
  mutate(
    stem = if_else(kind == "death", "sibou", "sisyou"),
    era  = case_when(year <= 2018 ~ sprintf("h%02d", year - 1988),
                     year == 2019 ~ "h31",
                     TRUE         ~ sprintf("r%d", year - 2018)),
    name = case_when(year <= 2020 ~ sprintf("%s_%s(kakutei).xls", era, stem),
                     year == 2021 ~ sprintf("R3_16_%s.xls", stem),
                     TRUE         ~ sprintf("%s_16_%s.xlsx", era, stem)),
    file = sprintf("%s_%d.%s", kind, year, if_else(year >= 2022, "xlsx", "xls")),
    url  = paste0(BASE, name),
    holds = if_else(kind == "death", "Fatal injuries by industry and establishment size, worksheet 5",
                    "Reported injuries by industry and establishment size, worksheet 5"))

# NOTE: COVID-19 cases removed, published for 2020 and 2021 only
kakuteiex <- tribble(
  ~kind,    ~year, ~name,
  "death",   2020, "r2_16_sibou(exceptCORONA).xlsx",
  "injury",  2020, "r2_16_sisyou(exceptCORONA).xlsx",
  "death",   2021, "R3_16_sibou(exceptCORONA).xlsx",
  "injury",  2021, "R3_16_sisyou(exceptCORONA).xlsx"
) |>
  mutate(file  = sprintf("%s_%d_excovid.xlsx", kind, year),
         url   = paste0(BASE, name),
         holds = "The same worksheet with occupational COVID-19 removed")

workbooks <- bind_rows(kakutei, kakuteiex)

workbooks |> pwalk(function(file, url, ...){
  dest <- file.path(DATA, file)
  if (file.exists(dest)) return(NULL)
  download.file(url, dest, quiet = TRUE, mode = "wb")
})

## Economic Census (e-Stat API)-----
# NOTE: reg = regular employees, eng = persons engaged, est = establishments, by establishment size.
# The tables are classified by regular-employee size. pin fixes axes to one code (employment status, or in 2024
# the type of organisation); sizecol is the size axis.
estat <- tribble(
  ~tag,  ~year, ~id,          ~tab,       ~sizecol, ~pin,
  "reg",  2009, "0003032572", "004",      "cat03",  "cdCat02=004",
  "reg",  2012, "0003090096", "005",      "cat03",  "cdCat01=004",
  "reg",  2014, "0003111125", "004",      "cat02",  "cdCat01=004",
  "reg",  2016, "0003218663", "005",      "cat01",  "cdCat03=0060",
  "reg",  2021, "0004005662", "113-2021", "cat02",  "cdCat03=4",
  "reg",  2024, "0004040083", "105-2024", "cat02",  "cdCat03=0",

  "eng",  2009, "0003032572", "004",      "cat03",  "cdCat02=000",
  "eng",  2012, "0003090096", "005",      "cat03",  "cdCat01=000",
  "eng",  2014, "0003111125", "004",      "cat02",  "cdCat01=000",
  "eng",  2016, "0003218663", "005",      "cat01",  "cdCat03=0010",
  "eng",  2021, "0004005662", "113-2021", "cat02",  "cdCat03=0",
  "eng",  2024, "0004040083", "103-2024", "cat02",  "cdCat03=0",

  "est",  2009, "0003032571", "003",      "cat02",  "",
  "est",  2012, "0003090074", "004",      "cat02",  "",
  "est",  2014, "0003111122", "003",      "cat01",  "",
  "est",  2016, "0003218662", "004",      "cat01",  "",
  "est",  2021, "0004005645", "102-2021", "cat02",  "",
  "est",  2024, "0004040083", "102-2024", "cat02",  "cdCat03=0"
) |>
  mutate(file = sprintf("estat_%s_%d.rds", tag, year),
         url  = paste0("https://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=YOUR_APPID",
                       "&statsDataId=", id, "&cdTab=", tab, "&cdArea=00000&metaGetFlg=N&limit=100000",
                       if_else(pin == "", "", paste0("&", pin))),
         holds = recode(tag, reg = "Regular employees by establishment size",
                        eng = "Persons engaged by establishment size",
                        est = "Establishments by establishment size"))

# NOTE: 02_analysis.R reads the table ids and size axes from here
write_csv(estat |> select(tag, year, id, tab, sizecol, pin), file.path(DATA, "estat_tables.csv"))

appid <- function(){
  key <- Sys.getenv("ESTAT_APPID")
  if (!nzchar(key)) key <- trimws(readLines(path.expand("~/.estat_appid"), warn = FALSE)[1])
  key
}

estat |> pwalk(function(file, url, ...){
  dest <- file.path(DATA, file)
  if (file.exists(dest)) return(NULL)
  j <- fromJSON(str_replace(url, "YOUR_APPID", appid()))
  stopifnot(j$GET_STATS_DATA$RESULT$STATUS == 0)
  write_rds(as_tibble(j$GET_STATS_DATA$STATISTICAL_DATA$DATA_INF$VALUE), dest)
  Sys.sleep(1)
})

# NOTE: the code-to-name dictionary of every table, kept as the JSON the API returned
meta <- estat |>
  distinct(id) |>
  mutate(file  = sprintf("meta_%s.json", id),
         url   = paste0("https://api.e-stat.go.jp/rest/3.0/app/json/getMetaInfo?appId=YOUR_APPID&statsDataId=", id),
         holds = "Class dictionary: the code to name mapping for every axis of the table")

meta |> pwalk(function(file, url, ...){
  dest <- file.path(DATA, file)
  if (file.exists(dest)) return(NULL)
  download.file(str_replace(url, "YOUR_APPID", appid()), dest, quiet = TRUE)
  Sys.sleep(1)
})

## Inventory-----
# NOTE: retrieved is the date the file was written
manifest <- bind_rows(workbooks, estat, meta) |>
  mutate(path      = file.path(DATA, file),
         bytes     = file.size(path),
         md5       = unname(tools::md5sum(path)),
         retrieved = as.Date(file.mtime(path))) |>
  select(file, bytes, md5, retrieved, source = url, holds)

stopifnot(!anyNA(manifest$bytes))
nrow(manifest)
# FOOTNOTE: 67 files: 34 workbooks, 4 without COVID-19, 18 census tables, 11 dictionaries

write_excel_csv(manifest, file.path(DATA, "manifest.csv"))
