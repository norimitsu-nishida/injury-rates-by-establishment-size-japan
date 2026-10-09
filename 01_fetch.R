library(tidyverse)
library(jsonlite)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# NOTE: download the raw data into cache/ (MHLW workbooks and e-Stat API); existing files are skipped

## Finalized accident statistics, by industry and establishment size-----
download_kakutei <- function(kind, year){

  dest <- kakutei_file(kind, year)
  if (file.exists(dest) && file.size(dest) > 20000) return(NULL)

  url <- kakutei_url(kind, year)
  ok  <- tryCatch({
    download.file(url, dest, quiet = TRUE, mode = "wb")
    file.size(dest) > 20000
  }, error = function(e) FALSE)

  if (!ok) {
    unlink(dest)
    message("could not download: ", url)
  }
}

walk(YEARS, ~ download_kakutei("death",  .x))
walk(YEARS, ~ download_kakutei("injury", .x))

list.files(CACHE, pattern = "^(death|injury)_[0-9]{4}[.]xlsx?$")
# FOOTNOTE: 34 files, two for each of the 17 years

## The same statistics with the COVID-19 cases removed-----
# NOTE: published for 2020 and 2021 only
download_kakuteiex <- function(kind, year){

  dest <- kakuteiex_file(kind, year)
  if (file.exists(dest) && file.size(dest) > 20000) return(NULL)

  download.file(kakuteiex_url(kind, year), dest, quiet = TRUE, mode = "wb")
}

walk(EXYEARS, ~ download_kakuteiex("death",  .x))
walk(EXYEARS, ~ download_kakuteiex("injury", .x))

list.files(CACHE, pattern = "_excovid[.]xlsx$")
# FOOTNOTE: 4 files, death and reported injuries for 2020 and 2021

## Economic Census (e-Stat)-----
# NOTE: table ids and pinned axes are in 00_setup.R
download_estat <- function(tag, year){

  dest <- file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year))
  if (file.exists(dest)) return(NULL)

  spec <- estattable |> filter(tag == !!tag, year == !!year)
  stopifnot(nrow(spec) == 1)

  pinned <- estatfix |> filter(tag == !!tag, year == !!year)
  fix    <- set_names(pinned$code, pinned$param)

  write_rds(estat(spec$id, spec$tab, fix), dest)
  Sys.sleep(1)
}

walk(BENCHMARK, ~ download_estat("reg", .x))
walk(BENCHMARK, ~ download_estat("est", .x))
walk(BENCHMARK, ~ download_estat("ind", .x))

list.files(CACHE, pattern = "^estat_")
# FOOTNOTE: 18 tables, three for each of the six benchmark years
