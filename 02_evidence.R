library(tidyverse)
library(jsonlite)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# NOTE: checks that cache/ holds the published statistics and writes evidence/

EVID <- file.path(ROOT, "evidence")
dir.create(EVID, showWarnings = FALSE, recursive = TRUE)

## The accident workbooks against the published national totals-----
# NOTE: the 合計 row of ⑤業種・規模 must reproduce the published national totals.
# Injuries of 2009-2010 were published on the claims basis, so only deaths are compared.
publishedtotals <- tribble(
  ~year, ~deaths, ~injuries, ~injury_basis, ~source,
  2009, 1075, 105718, "claims",  "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/xls/09-kakutei.xls",
  2010, 1195, 107759, "claims",  "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/xls/10-kakutei.xls",
  2011, 1024, 117958, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/xls/12-kakutei.xls (2011 restated)",
  2012, 1093, 119576, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/xls/12-kakutei.xls",
  2013, 1030, 118157, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/xls/13-kakutei.xls",
  2014, 1057, 119535, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/dl/14_kakutei.pdf",
  2015,  972, 116311, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/dl/15_kakutei.pdf",
  2016,  928, 117910, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/dl/16_kakutei.pdf",
  2017,  978, 120460, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/dl/17-kakutei.pdf",
  2018,  909, 127329, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/dl/18-kakutei.pdf",
  2019,  845, 125611, "reports", "https://www.mhlw.go.jp/content/11302000/000633584.pdf",
  2020,  802, 131156, "reports", "https://www.mhlw.go.jp/bunya/roudoukijun/anzeneisei11/rousai-hassei/dl/20-kakutei.pdf",
  2021,  867, 149918, "reports", "https://www.mhlw.go.jp/content/11302000/000943971.pdf",
  2022,  774, 132355, "reports", "https://www.mhlw.go.jp/stf/newpage_33256.html",
  2023,  755, 135371, "reports", "https://www.mhlw.go.jp/stf/newpage_40395.html",
  2024,  746, 135718, "reports", "https://www.mhlw.go.jp/stf/newpage_58198.html",
  2025,  700, 135333, "reports", "https://www.mhlw.go.jp/stf/newpage_73382.html"
)
# FOOTNOTE: 2020-21 include occupational COVID-19; read from the documents on 2026-10-07

worksheettotals <- map_dfr(YEARS, function(year){
  dth <- read_kakutei("death",  year)
  inj <- read_kakutei("injury", year)
  tibble(year = year,
         deaths_worksheet   = sum(pickrow(dth, "合計")),
         injuries_worksheet = sum(pickrow(inj, "合計")))
})

totalcheck <- publishedtotals |>
  left_join(worksheettotals, by = "year") |>
  mutate(deaths_agree   = deaths == deaths_worksheet,
         injuries_agree = if_else(injury_basis == "reports", injuries == injuries_worksheet, NA))

totalcheck |> select(year, deaths, deaths_worksheet, deaths_agree, injuries, injuries_worksheet, injuries_agree) |> print(n = 17)
stopifnot(all(totalcheck$deaths_agree), all(totalcheck$injuries_agree, na.rm = TRUE))
# FOOTNOTE: deaths agree in all 17 years; injuries agree in the 15 years on the injury-report basis

## The API tables against the published census-----
# NOTE: 総数 of the all-industry establishment count against the figure printed in each census summary
totalrow <- function(tag, year){

  values  <- read_rds(file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year)))
  spec    <- estattable |> filter(tag == !!tag, year == !!year)
  sizecol <- paste0("@", spec$sizecol)

  sizes     <- estat_meta(spec$id, spec$sizecol)
  totalcode <- sizes$code[sizes$name == "総数"]

  v <- values |>
    mutate(val = as.numeric(`$`)) |>
    filter(!is.na(val), .data[[sizecol]] == totalcode)
  v <- allindustry(v, sizecol)

  stopifnot(nrow(v) == 1)
  v$val
}

estcheck <- censuspdf |>
  select(year, published = establishments) |>
  mutate(thiscode = map_dbl(year, function(y) totalrow("est", y)))

estcheck
stopifnot(estcheck$thiscode == estcheck$published)
# FOOTNOTE: 5,886,193 / 5,453,635 / 5,541,634 / 5,340,783 / 5,156,063 / 4,023,941, all equal to the summaries

# NOTE: regular employees 総数 of the industry table must equal that of the all-industry table
regcheck <- tibble(year = BENCHMARK) |>
  mutate(from_reg = map_dbl(year, function(y) totalrow("reg", y)),
         from_ind = map_dbl(year, function(y) totalrow("ind", y)))

regcheck
stopifnot(regcheck$from_reg == regcheck$from_ind)
# FOOTNOTE: 47,843,039 / 46,102,066 / 48,684,580 / 49,144,392 / 50,725,472 / 51,018,265 in both

## The published census summaries-----
# NOTE: the documents the census figures in 00_setup.R were read from
download_censuspdf <- function(year, url){

  dest <- file.path(EVID, sprintf("census_%d_summary.pdf", year))
  if (file.exists(dest)) return(NULL)

  download.file(url, dest, quiet = TRUE, mode = "wb")
  Sys.sleep(1)
}

walk2(censuspdf$year, censuspdf$url, download_censuspdf)

list.files(EVID, pattern = "[.]pdf$")
# FOOTNOTE: six files, one per benchmark year

## The API responses as csv-----
# NOTE: the cached response holds codes only; class names joined in
namecols <- function(values, id){

  axes <- grep("^@(tab|cat[0-9]+)$", names(values), value = TRUE)

  for (ax in axes) {
    dict <- estat_meta(id, str_remove(ax, "^@")) |>
      select(code, name) |>
      set_names(c(ax, paste0(str_remove(ax, "^@"), "name")))

    values <- values |> left_join(dict, by = ax)
  }

  values |> relocate(ends_with("name"), .after = last_col())
}

export_estat <- function(tag, year){

  spec   <- estattable |> filter(tag == !!tag, year == !!year)
  values <- read_rds(file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year)))

  dest <- file.path(EVID, sprintf("estat_%s_%d.csv", tag, year))
  namecols(values, spec$id) |> write_excel_csv(dest)
}

walk(BENCHMARK, ~ export_estat("reg", .x))
walk(BENCHMARK, ~ export_estat("est", .x))
walk(BENCHMARK, ~ export_estat("ind", .x))

list.files(EVID, pattern = "^estat_.*[.]csv$") |> length()
# FOOTNOTE: 18 files, matching the 18 cached responses

## What each file is and where it came from-----
# NOTE: addresses rebuilt from the same definitions the download used
kakuteirows <- expand_grid(kind = c("death", "injury"), year = YEARS) |>
  transmute(
    path   = map2_chr(kind, year, kakutei_file),
    source = map2_chr(kind, year, kakutei_url),
    holds  = if_else(kind == "death",
                     "Fatal injuries by industry and establishment size, worksheet 5",
                     "Reported injuries by industry and establishment size, worksheet 5")
  )

excvrows <- kakuteiex |>
  transmute(
    path   = map2_chr(kind, year, kakuteiex_file),
    source = map2_chr(kind, year, kakuteiex_url),
    holds  = "The same worksheet with the occupational COVID-19 cases removed"
  )

estatrows <- estattable |>
  transmute(
    path   = file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year)),
    source = map2_chr(tag, year, estat_url),
    holds  = case_when(
      tag == "reg" ~ "Regular employees by establishment size, all industries",
      tag == "est" ~ "Establishments by establishment size, all industries",
      tag == "ind" ~ "Regular employees by establishment size and JSIC major group"
    )
  )

metarows <- tibble(id = unique(estattable$id)) |>
  transmute(
    path   = map_chr(id, meta_file),
    source = map_chr(id, meta_url),
    holds  = "Class dictionary: the code to name mapping for every axis of the table"
  )

pdfrows <- censuspdf |>
  transmute(
    path   = file.path(EVID, sprintf("census_%d_summary.pdf", year)),
    source = url,
    holds  = "Published summary of the census, carrying the establishment count checked in 00_setup.R"
  )

csvrows <- estattable |>
  transmute(
    path   = file.path(EVID, sprintf("estat_%s_%d.csv", tag, year)),
    source = map2_chr(tag, year, estat_url),
    holds  = "The response above, written out with the class names joined in"
  )

manifest <- bind_rows(kakuteirows, excvrows, estatrows, metarows, pdfrows, csvrows) |>
  mutate(
    # NOTE: path relative to this directory
    file      = str_remove(path, fixed(paste0(ROOT, "/"))),
    bytes     = file.size(path),
    md5       = unname(tools::md5sum(path)),
    retrieved = as.Date(file.mtime(path))
  ) |>
  select(file, bytes, md5, retrieved, source, holds) |>
  arrange(file)

manifest |> filter(is.na(bytes))
# FOOTNOTE: no rows, so every file the manifest names is actually present

write_excel_csv(manifest, file.path(EVID, "manifest.csv"))

manifest |> count(kind = str_extract(file, "^[^/]+/[a-z]+"))
nrow(manifest)
