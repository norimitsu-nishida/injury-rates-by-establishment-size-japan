library(tidyverse)
library(jsonlite)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# NOTE: print the census figures used, and where e-Stat shows them (dbview?sid=<statsDataId>).
# The summary PDFs tabulate by persons-engaged size, so only their national totals are comparable.

ind <- read_rds(file.path(OUT, "panel_industry.rds")) |> mutate(bin = as.character(bin))

# NOTE: title and axis names of a table, read from its cached metadata
tableinfo <- function(id){
  m    <- fromJSON(meta_file(id), simplifyVector = FALSE)
  t    <- m$GET_META_INFO$METADATA_INF$TABLE_INF
  axes <- m$GET_META_INFO$METADATA_INF$CLASS_INF$CLASS_OBJ
  axisnames <- character()
  for (a in axes) axisnames[a[["@id"]]] <- a[["@name"]]
  list(title = paste(t$STATISTICS_NAME, "|", t$TITLE$`$`), axisnames = axisnames)
}

# NOTE: the all-industry rows of one table, one row per size class in the table's own classes
byownclass <- function(tag, year){
  spec    <- estattable |> filter(tag == !!tag, year == !!year)
  values  <- read_rds(file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year)))
  sizecol <- paste0("@", spec$sizecol)
  sizes   <- estat_meta(spec$id, spec$sizecol)

  v <- values |>
    mutate(val = suppressWarnings(as.numeric(`$`))) |>
    filter(!is.na(val)) |>
    left_join(sizes |> select(code, class = name), by = setNames("code", sizecol))
  v <- allindustry(v, sizecol)

  v |>
    transmute(code = .data[[sizecol]], class, value = val) |>
    arrange(code)
}

# NOTE: the axis settings on the e-Stat page that show the rows printed
howtoset <- function(tag, year){
  spec <- estattable |> filter(tag == !!tag, year == !!year)
  info <- tableinfo(spec$id)
  pin  <- estatfix |> filter(tag == !!tag, year == !!year)
  settings <- character()
  for (j in seq_len(nrow(pin))) {
    axis <- str_to_lower(str_remove(pin$param[j], "^cd"))
    meta <- estat_meta(spec$id, axis)
    settings <- c(settings, sprintf("%s = %s", info$axisnames[axis], meta$name[meta$code == pin$code[j]]))
  }
  tabs <- estat_meta(spec$id, "tab")
  settings <- c(sprintf("表章項目 = %s", tabs$name[tabs$code == spec$tab]), settings)
  list(title = info$title, settings = settings)
}

## The figures used, table by table-----
for (y in BENCHMARK) {
  for (tg in c("reg", "est")) {

    spec <- estattable |> filter(tag == tg, year == y)
    how  <- howtoset(tg, y)
    what <- if (tg == "reg") "regular employees (the denominator)" else "establishments (used only for the mean size of the 300 or more class)"

    cat(sprintf("\n===== %d  %s\n", y, what))
    cat(sprintf("      %s\n", how$title))
    cat(sprintf("      open https://www.e-stat.go.jp/dbview?sid=%s and set: %s; 産業 = 全産業 (the largest code), 地域 = 全国\n",
                spec$id, paste(how$settings, collapse = "; ")))
    cat("      the rows below are the table's own classes; 0人, 総数 and 1人 to 4人 are not used by the paper\n")

    shown <- byownclass(tg, y) |> mutate(value = format(value, big.mark = ","))
    print(as.data.frame(shown), row.names = FALSE)
  }
}

## The regular-employee totals that the printed summaries carry-----
# NOTE: 2016 and 2021 print none; the 2009 figure excludes agriculture, forestry and fisheries
regulartotals <- tribble(
  ~year, ~printed,  ~file,                     ~page, ~where,
  2009,  47601397,  "census_2009_summary.pdf", 33,    "表Ⅰ-22 従業上の地位、男女別従業者数（民営、非農林漁業）、常用雇用者の行",
  2012,  46102066,  "census_2014_summary.pdf", 15,    "従業上の地位別従業者数の表、2012年の列、常用雇用者の行",
  2014,  48684580,  "census_2014_summary.pdf", 15,    "同じ表、2014年の列",
  2024,  51018265,  "census_2024_summary.pdf", 13,    "表Ⅲ-1 産業大分類別従業者数及び常用雇用者数、合計の行"
)

cat("\n===== regular employees, national total, against the printed summaries\n")
for (i in seq_len(nrow(regulartotals))) {
  r   <- regulartotals[i, ]
  own <- byownclass("reg", r$year)
  api <- own$value[own$class == "総数"]
  if (r$year == 2009) {
    agri <- ind |> filter(year == 2009, grp == "農林水産") |> summarise(py = sum(py)) |> pull(py)
    cat(sprintf("  %d  API 総数 %s minus agriculture, forestry and fisheries %s = %s;  printed %s\n        evidence/%s p.%d, %s\n",
                r$year, format(api, big.mark = ","), format(round(agri), big.mark = ","), format(round(api - agri), big.mark = ","),
                format(r$printed, big.mark = ","), r$file, r$page, r$where))
    api <- api - agri
  } else {
    cat(sprintf("  %d  API 総数 %s;  printed %s\n        evidence/%s p.%d, %s\n",
                r$year, format(api, big.mark = ","), format(r$printed, big.mark = ","), r$file, r$page, r$where))
  }
  stopifnot(abs(api - r$printed) < 1)
}
cat("  2016 and 2021: no summary prints the regular-employee total; the table page above is the published form.\n")
cat("  All four printed totals agree with the API.\n")
