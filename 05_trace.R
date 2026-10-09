library(tidyverse)
library(readxl)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# NOTE: follow nine panel cells back to the raw files, printing file, sheet, row, column and value

dat <- read_rds(file.path(OUT, "panel_all.rds"))      |> mutate(bin = as.character(bin))
ind <- read_rds(file.path(OUT, "panel_industry.rds")) |> mutate(bin = as.character(bin))

panelcell <- function(year, bin, col){
  d <- dat |> filter(year == !!year, bin == !!bin)
  d[[col]]
}

industrycell <- function(year, grp, bin, col){
  d <- ind |> filter(year == !!year, grp == !!grp, bin == !!bin)
  stopifnot(nrow(d) == 1)
  d[[col]]
}

say <- function(...) cat(sprintf(...), "\n")

# NOTE: one worksheet cell: the row with this label, the column of this size class
worksheetcell <- function(kind, year, label, bin, suffix = ""){
  file  <- if (suffix == "") kakutei_file(kind, year) else kakuteiex_file(kind, year)
  sheet <- grep("規模", excel_sheets(file), value = TRUE)[1]
  d     <- read_kakutei(kind, year, suffix)
  v     <- pickrow(d, label)[match(bin, BINS)]
  say("  %-28s sheet %-14s row %-14s column %-8s -> %s", basename(file), sheet, label, bin, format(v, big.mark = ","))
  v
}

# NOTE: one e-Stat value: rows of the named size classes, all-industry rows only
estatcells <- function(tag, year, classnames, indcode = NULL){
  spec    <- estattable |> filter(tag == !!tag, year == !!year)
  file    <- file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year))
  values  <- read_rds(file)
  sizecol <- paste0("@", spec$sizecol)
  sizes   <- estat_meta(spec$id, spec$sizecol)

  v <- values |>
    mutate(val = suppressWarnings(as.numeric(`$`))) |>
    filter(!is.na(val)) |>
    left_join(sizes |> select(code, classname = name), by = setNames("code", sizecol)) |>
    filter(classname %in% classnames)

  if (is.null(indcode)) {
    v <- allindustry(v, sizecol)
  } else {
    indcol <- paste0("@", spec$indcol)
    v <- v |> filter(.data[[indcol]] %in% indcode)
  }

  # NOTE: the industry code is shown when the rows were picked by industry
  industry <- if (is.null(indcode)) rep("", nrow(v)) else paste0(spec$indcol, "=", v[[paste0("@", spec$indcol)]], " ")
  for (i in seq_len(nrow(v))) {
    say("  %-28s table %-11s %s%s=%-4s %-8s -> %s", basename(file), spec$id, industry[i],
        spec$sizecol, v[[sizecol]][i], v$classname[i], format(v$val[i], big.mark = ","))
  }
  v$val
}

## A  death count, 2021, 1-9, published workbook-----
cat("\nA  deaths, 2021, class 1-9, as published\n")
total  <- worksheetcell("death", 2021, "合計",   "1-9")
public <- worksheetcell("death", 2021, "官公署", "1-9")
say("  %s - %s = %s;  panel death_pub(2021, 1-9) = %s", total, public, total - public, panelcell(2021, "1-9", "death_pub"))
stopifnot(total - public == panelcell(2021, "1-9", "death_pub"))

## B  the same cell from the COVID-19-removed workbook, which the main series uses-----
cat("\nB  deaths, 2021, class 1-9, COVID-19 removed (the main series)\n")
total  <- worksheetcell("death", 2021, "合計",   "1-9", "_excovid")
public <- worksheetcell("death", 2021, "官公署", "1-9", "_excovid")
say("  %s - %s = %s;  panel death(2021, 1-9) = %s", total, public, total - public, panelcell(2021, "1-9", "death"))
stopifnot(total - public == panelcell(2021, "1-9", "death"))

## C  reported injuries, 2025, 300 or more-----
cat("\nC  reported injuries, 2025, class 300 or more\n")
total  <- worksheetcell("injury", 2025, "合計",   "300+")
public <- worksheetcell("injury", 2025, "官公署", "300+")
say("  %s - %s = %s;  panel injury(2025, 300+) = %s", format(total, big.mark = ","), public, format(total - public, big.mark = ","),
    format(panelcell(2025, "300+", "injury"), big.mark = ","))
stopifnot(total - public == panelcell(2025, "300+", "injury"))

## D  an industry cell that is the sum of six printed rows-----
cat("\nD  deaths, 2019, other services, class 50-99 (holds the 2019 incident)\n")
labels <- c("金融広告業", "映画・演劇業", "通信業", "教育研究", "清掃・と畜", "その他の事業")
parts  <- map_dbl(labels, function(l) worksheetcell("death", 2019, l, "50-99"))
say("  %s = %s;  panel_industry death(2019, その他, 50-99) = %s", paste(parts, collapse = " + "), sum(parts),
    industrycell(2019, "その他", "50-99", "death"))
stopifnot(sum(parts) == industrycell(2019, "その他", "50-99", "death"))

## E  benchmark-year denominator, 2021, 1-9-----
cat("\nE  regular employees, 2021, class 1-9 = 1～4人 + 5～9人, all industries (API table 0004005662)\n")
v <- estatcells("reg", 2021, c("1～4人", "5～9人"))
say("  sum = %s;  panel py(2021, 1-9) = %s", format(sum(v), big.mark = ","), format(panelcell(2021, "1-9", "py"), big.mark = ","))
stopifnot(abs(sum(v) - panelcell(2021, "1-9", "py")) < 0.5)   # py is stored as a double

## F  the same for 2024, a table with a different layout-----
cat("\nF  regular employees, 2024, class 1-9 (API table 0004040083)\n")
v <- estatcells("reg", 2024, c("1～4人", "5～9人"))
say("  sum = %s;  panel py(2024, 1-9) = %s", format(sum(v), big.mark = ","), format(panelcell(2024, "1-9", "py"), big.mark = ","))
stopifnot(abs(sum(v) - panelcell(2024, "1-9", "py")) < 0.5)

## G  an interpolated year: 2017 from 2016 and 2021-----
cat("\nG  regular employees, 2017, class 1-9, interpolated log-linearly between 2016 and 2021\n")
v2016 <- panelcell(2016, "1-9", "py")
v2021 <- panelcell(2021, "1-9", "py")
v2017 <- exp(log(v2016) + (2017 - 2016) / (2021 - 2016) * (log(v2021) - log(v2016)))
say("  exp( log(%s) + (2017-2016)/(2021-2016) * (log(%s) - log(%s)) ) = %s;  panel py(2017, 1-9) = %s",
    format(v2016, big.mark = ","), format(v2021, big.mark = ","), format(v2016, big.mark = ","),
    format(round(v2017), big.mark = ","), format(round(panelcell(2017, "1-9", "py")), big.mark = ","))
stopifnot(abs(v2017 - panelcell(2017, "1-9", "py")) < 1)

## H  the carried-forward year: 2025 takes the 2024 value-----
cat("\nH  regular employees, 2025, class 1-9, carried forward from 2024\n")
say("  panel py(2024, 1-9) = %s;  panel py(2025, 1-9) = %s",
    format(panelcell(2024, "1-9", "py"), big.mark = ","), format(panelcell(2025, "1-9", "py"), big.mark = ","))
stopifnot(panelcell(2025, "1-9", "py") == panelcell(2024, "1-9", "py"))

## I  an industry denominator: construction, 2021, 1-9 = JSIC 06 + 07 + 08-----
cat("\nI  regular employees, 2021, construction (JSIC 06, 07, 08), class 1-9 (API table 0004005646)\n")
v <- estatcells("ind", 2021, c("1～4人", "5～9人"), indcode = c("06", "07", "08"))
say("  sum = %s;  panel_industry py(2021, 建設業, 1-9) = %s", format(sum(v), big.mark = ","),
    format(industrycell(2021, "建設業", "1-9", "py"), big.mark = ","))
stopifnot(abs(sum(v) - industrycell(2021, "建設業", "1-9", "py")) < 0.5)

cat("\n9 cells traced from the raw files to the panels; all agree\n")
