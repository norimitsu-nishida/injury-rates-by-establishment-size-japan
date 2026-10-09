library(tidyverse)
library(jsonlite)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# NOTE: write manual_estat.md (how to take the 18 tables by hand) and compare any downloads in manual/ with the panels

MANUAL <- file.path(ROOT, "manual")
dir.create(MANUAL, showWarnings = FALSE)

## Part 1: write the manual-----
tableinfo <- function(id){
  m    <- fromJSON(meta_file(id), simplifyVector = FALSE)
  t    <- m$GET_META_INFO$METADATA_INF$TABLE_INF
  axes <- m$GET_META_INFO$METADATA_INF$CLASS_INF$CLASS_OBJ
  axisnames <- character()
  for (a in axes) axisnames[a[["@id"]]] <- a[["@name"]]
  list(stat = t$STATISTICS_NAME, title = t$TITLE$`$`, axes = axisnames)
}

# NOTE: the axes to set on the page: pinned axes, the industry axis (all-industry code; open for ind) and 全国
settingsfor <- function(spec){
  info <- tableinfo(spec$id)
  tabs <- estat_meta(spec$id, "tab")
  pin  <- estatfix |> filter(tag == spec$tag, year == spec$year)

  lines <- sprintf("表章項目 = **%s**", tabs$name[tabs$code == spec$tab])
  for (j in seq_len(nrow(pin))) {
    axis <- str_to_lower(str_remove(pin$param[j], "^cd"))
    meta <- estat_meta(spec$id, axis)
    lines <- c(lines, sprintf("%s = **%s**", info$axes[axis], meta$name[meta$code == pin$code[j]]))
  }

  # NOTE: the industry axis: the column that is neither the size axis nor pinned
  values  <- read_rds(file.path(CACHE, sprintf("estat_%s_%d.rds", spec$tag, spec$year)))
  sizecol <- paste0("@", spec$sizecol)
  catcols <- setdiff(grep("^@cat", names(values), value = TRUE), sizecol)
  for (o in catcols) {
    axis <- sub("@", "", o)
    if (n_distinct(values[[o]]) > 1) {
      meta <- estat_meta(spec$id, axis)
      if (spec$tag == "ind") {
        lines <- c(lines, sprintf("%s = **すべて選択**（中分類をすべて。%d 項目）", info$axes[axis], nrow(meta)))
      } else {
        lines <- c(lines, sprintf("%s = **%s** だけ", info$axes[axis], meta$name[1]))
      }
    }
  }
  lines <- c(lines, sprintf("%s = **%s** だけ", info$axes["area"], "全国"),
             sprintf("%s = **すべて選択**（%s）", info$axes[spec$sizecol], "規模の区分をすべて。総数と0人も含めてよい"))
  list(info = info, lines = lines)
}

md <- c(
  "# e-Stat から手で分母を取るための手順",
  "",
  "`01_fetch.R` が API で取った18の表を、ブラウザで同じ表を開いて同じ軸の設定で取り直し、`08_manual_estat.R` で突き合わせるための手順。",
  "表の ID・軸の名前・選ぶ値はすべて表のメタデータ（`cache/meta_*.json`）から書き出しているので、API の設定と食い違わない。",
  "",
  "## 共通の操作",
  "",
  "1. 下の URL をブラウザで開く（e-Stat のデータベース表示。ログイン不要）。",
  "2. 画面上部の **「表示項目選択」** を押し、各軸（表章項目、産業、規模、従業上の地位、経営組織、地域 など）について、下に書いた値だけにチェックを残す。「すべて選択」と書いた軸は全項目を残す。",
  "3. **「確定」** で表に戻り、**「ダウンロード」** を押す。ファイル形式 **CSV**、文字コード **UTF-8**、「表章項目や分類の名称を出力」をオンにして保存。xlsx でもよい。",
  "4. 保存したファイルを `public2/manual/` に、下に書いたファイル名で置く。",
  "5. `public2/` で `V2_HOME=. Rscript --vanilla 08_manual_estat.R` を回す。ファイルがあるものだけ突き合わせ、合わなければ止まる。",
  "",
  "優先順位は **reg（分母そのもの）6表 → ind（業種別の分母）6表**。est（事業所数。Table 2 注 e の平均規模にだけ使う）6表は任意。",
  ""
)

sections <- list(reg = "常用雇用者数（全産業・規模別）= Table 2・5・6 の分母",
                 ind = "常用雇用者数（産業中分類 × 規模別）= Table 3・4 の分母（12群に畳む前）",
                 est = "事業所数（全産業・規模別）= Table 2 注 e の平均規模にだけ使う（任意）")

for (tg in names(sections)) {
  md <- c(md, sprintf("## %s", sections[[tg]]), "")
  for (y in BENCHMARK) {
    spec <- estattable |> filter(tag == tg, year == y)
    s    <- settingsfor(spec)
    md <- c(md,
            sprintf("### %s %d", tg, y),
            "",
            sprintf("- 表: %s — %s", s$info$stat, s$info$title),
            sprintf("- URL: https://www.e-stat.go.jp/dbview?sid=%s", spec$id),
            "- 表示項目選択:",
            paste0("  - ", s$lines),
            sprintf("- 保存名: `manual/%s_%d.csv`（xlsx なら `manual/%s_%d.xlsx`）", tg, y, tg, y),
            "")
  }
}

writeLines(md, file.path(ROOT, "manual_estat.md"))
cat("manual_estat.md written:", length(md), "lines\n")

## Part 2: compare what was downloaded by hand-----
# NOTE: a download has metadata rows (pinned axes), a header row holding "/表章項目", then one row per class
readcells <- function(path){
  if (str_detect(path, "[.]xlsx?$")) {
    d <- readxl::read_excel(path, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  } else {
    d <- read_csv(path, col_names = FALSE, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"))
  }
  d <- as.data.frame(d)
  d[] <- lapply(d, function(x) replace_na(x, ""))
  d
}

# NOTE: the value column is the one whose 表章項目 code equals the tab code in estattable
parsedownload <- function(path, tab){
  d <- readcells(path)

  # NOTE: 表章項目 code on the row "/表章項目 コード", its name on "/表章項目", same column
  coderow    <- which(apply(d, 1, function(r) any(r == "/表章項目 コード")))[1]
  measurerow <- which(apply(d, 1, function(r) any(r == "/表章項目")))[1]
  stopifnot(!is.na(coderow), !is.na(measurerow))
  codes    <- unlist(d[coderow, ])
  valuecol <- which(codes == tab)[1]
  stopifnot(!is.na(valuecol))
  measure  <- unlist(d[measurerow, ])[valuecol]

  # NOTE: long layout (reg, est): classes down the rows. Wide layout (ind): classes across, header has 5+ class names
  classnames <- c("総数", "0人", "1人", "2人", "3人", "4人", classtobin$class)
  nclass <- apply(d, 1, function(r) sum(r %in% classnames))
  wide   <- which(nclass >= 5)[1]

  if (!is.na(wide)) {
    hdr     <- unlist(d[wide, ])
    classco <- which(hdr %in% classnames)
    indcol  <- which(str_detect(hdr, "産業") & !str_detect(hdr, "コード"))[1]
    indcode <- which(str_detect(hdr, "産業") &  str_detect(hdr, "コード$"))[1]
    rows    <- seq(wide + 1, nrow(d))
    rows    <- rows[d[rows, indcol] != ""]
    out <- list()
    for (j in classco) {
      out[[length(out) + 1]] <- tibble(
        industry      = d[rows, indcol],
        industry_code = if (is.na(indcode)) "" else d[rows, indcode],
        class         = hdr[j],
        value         = suppressWarnings(as.numeric(str_replace_all(d[rows, j], ",", "")))
      )
    }
    parsed <- bind_rows(out) |> filter(!is.na(value)) |> mutate(measure = measure)
    return(parsed)
  }

  header   <- measurerow
  hdr      <- unlist(d[header, ])
  sizecol  <- which(str_detect(hdr, "常用雇用者規模") & !str_detect(hdr, "コード"))[1]
  indcol   <- which(str_detect(hdr, "産業") & !str_detect(hdr, "コード"))[1]
  indcode  <- which(str_detect(hdr, "産業") &  str_detect(hdr, "コード$"))[1]

  rows <- seq(header + 1, nrow(d))
  rows <- rows[d[rows, sizecol] != ""]

  parsed <- tibble(
    industry      = if (is.na(indcol))  "" else d[rows, indcol],
    industry_code = if (is.na(indcode)) "" else d[rows, indcode],
    class         = d[rows, sizecol],
    value         = suppressWarnings(as.numeric(str_replace_all(d[rows, valuecol], ",", "")))
  )
  parsed <- parsed |> filter(!is.na(value)) |> mutate(measure = measure)
  parsed
}

# NOTE: print the metadata rows (the settings chosen on the page)
settingsinfile <- function(path){
  d <- readcells(path)
  header <- which(apply(d, 1, function(r) any(r == "/表章項目")))[1]
  for (i in seq_len(header - 1)) {
    r <- unlist(d[i, ]); r <- r[r != ""]
    if (length(r) >= 2) cat("   ", paste(r, collapse = " "), "\n")
  }
}

dat <- read_rds(file.path(OUT, "panel_all.rds"))      |> mutate(bin = as.character(bin))
ind <- read_rds(file.path(OUT, "panel_industry.rds")) |> mutate(bin = as.character(bin))

# NOTE: JSIC major-group number to the nine groups, from GRPMAP in 00_setup.R
jsicgroup <- tibble(num = 1:96, grp = NA_character_)
for (i in seq_len(nrow(GRPMAP))) {
  jsicgroup$grp[GRPMAP$from[i]:GRPMAP$to[i]] <- GRPMAP$grp[i]
}

# NOTE: the API response of one table folded to the six classes, all industries, for est
apibybin <- function(tag, year){
  spec    <- estattable |> filter(tag == !!tag, year == !!year)
  values  <- read_rds(file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year)))
  sizecol <- paste0("@", spec$sizecol)
  sizes   <- estat_meta(spec$id, spec$sizecol)
  v <- values |>
    mutate(val = suppressWarnings(as.numeric(`$`))) |>
    filter(!is.na(val)) |>
    inner_join(sizes |> select(code, class = name), by = setNames("code", sizecol)) |>
    inner_join(classtobin, by = "class")
  v <- allindustry(v, sizecol)
  v |> group_by(bin) |> summarise(value = sum(val), .groups = "drop")
}

foldclasses <- function(d){
  d |>
    inner_join(classtobin, by = "class") |>
    group_by(industry, bin) |>
    summarise(value = sum(value), .groups = "drop")
}

found <- list.files(MANUAL, pattern = "^(reg|ind|est)_[0-9]{4}[.](csv|xlsx?)$", full.names = TRUE)
if (length(found) == 0) {
  cat("manual/ holds no downloads yet. Follow manual_estat.md, then run this file again.\n")
} else {
  for (path in found) {
    tg <- str_extract(basename(path), "^(reg|ind|est)")
    y  <- as.integer(str_extract(basename(path), "[0-9]{4}"))
    cat(sprintf("\n===== %s\n", basename(path)))

    settingsinfile(path)
    spec <- estattable |> filter(tag == tg, year == y)
    got  <- parsedownload(path, spec$tab)
    cat(sprintf("    value column used: %s (表章項目 code %s)\n", got$measure[1], spec$tab))

    if (tg %in% c("reg", "est")) {
      # NOTE: all industries only (the largest industry row)
      bybin <- foldclasses(got)
      top   <- bybin |> group_by(industry) |> summarise(total = sum(value), .groups = "drop") |> slice_max(total, n = 1)
      bybin <- bybin |> filter(industry == top$industry) |> arrange(match(bin, BINS))
      panel <- if (tg == "reg") dat |> filter(year == y) |> transmute(bin, panel = round(py)) else NULL
      if (tg == "est") {
        panel <- apibybin("est", y) |> transmute(bin, panel = value)
      }
      cmp <- bybin |> left_join(panel, by = "bin") |> mutate(agree = value == panel)
      print(as.data.frame(cmp), row.names = FALSE)
      stopifnot(all(cmp$agree))
    } else {
      # NOTE: by industry: the two-digit JSIC number at the head of the industry text
      got <- got |> mutate(industry = if_else(industry_code != "", paste(industry_code, industry), industry))
      bygrp <- foldclasses(got) |>
        mutate(num = as.integer(str_match(industry, "(^|[^0-9])([0-9]{2})([^0-9]|$)")[, 3])) |>
        filter(!is.na(num)) |>
        left_join(jsicgroup, by = "num") |>
        filter(!is.na(grp)) |>
        group_by(grp, bin) |>
        summarise(value = sum(value), .groups = "drop") |>
        mutate(year = y) |>
        applymoves(submoves_for(y)) |>
        select(-year)
      # NOTE: the minor-group moves (laundry, baths, cinemas) come from the API all-industry table
      panel <- ind |> filter(year == y) |> transmute(grp, bin, panel = round(py))
      cmp <- bygrp |> left_join(panel, by = c("grp", "bin")) |> mutate(agree = value == panel) |>
        arrange(grp, match(bin, BINS))
      print(as.data.frame(cmp), row.names = FALSE)
      stopifnot(all(cmp$agree))
    }
    cat("  agrees with the panel\n")
  }
}
