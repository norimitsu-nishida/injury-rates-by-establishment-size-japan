# Fatal and reported occupational injury rates by establishment size in Japan, 2009-2025
# See README.md for the run order.
# An e-Stat appId is needed only for 01_fetch.R (~/.estat_appid or ESTAT_APPID).
library(tidyverse)
library(readxl)
library(jsonlite)

ROOT  <- Sys.getenv("V2_HOME", unset = ".")
CACHE <- file.path(ROOT, "cache")   # downloaded raw data
OUT   <- file.path(ROOT, "output")  # analysis panels
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT,   showWarnings = FALSE, recursive = TRUE)

BINS      <- c("1-9","10-29","30-49","50-99","100-299","300+")
YEARS     <- 2009:2025
BENCHMARK <- c(2009, 2012, 2014, 2016, 2021, 2024)

appid <- function() {
  k <- Sys.getenv("ESTAT_APPID")
  if (nzchar(k)) return(k)
  f <- path.expand("~/.estat_appid")
  if (file.exists(f)) {
    key <- trimws(readLines(f, warn = FALSE)[1])
    return(key)
  }
  stop("No e-Stat appId found. Put it in ESTAT_APPID or in ~/.estat_appid")
}

# NOTE: one getStatsData call. `fix` pins axes to one code each; area 00000 is the whole country.
estat <- function(id, tab, fix = character(), area = "cdArea") {

  fixpart <- if (length(fix)) paste0("&", names(fix), "=", unname(fix), collapse = "") else ""

  u <- paste0("https://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=", appid(),
              "&statsDataId=", id, "&cdTab=", tab, "&", area, "=00000",
              "&metaGetFlg=N&limit=100000",
              fixpart)

  j <- fromJSON(u, simplifyVector = TRUE)
  stopifnot(j$GET_STATS_DATA$RESULT$STATUS == 0)

  as_tibble(j$GET_STATS_DATA$STATISTICAL_DATA$DATA_INF$VALUE)
}

# NOTE: code-to-name dictionary, cached in cache/ as the JSON the API returned
meta_url <- function(id, hide_key = TRUE){
  paste0("https://api.e-stat.go.jp/rest/3.0/app/json/getMetaInfo?appId=",
         if (hide_key) "YOUR_APPID" else appid(), "&statsDataId=", id)
}

meta_file <- function(id) file.path(CACHE, sprintf("meta_%s.json", id))

estat_meta <- function(id, cls_id) {

  if (!file.exists(meta_file(id))) {
    download.file(meta_url(id, hide_key = FALSE), meta_file(id), quiet = TRUE)
    Sys.sleep(1)
  }

  m <- fromJSON(meta_file(id), simplifyVector = FALSE)

  # NOTE: one CLASS_OBJ per axis; take the axis asked for
  axes    <- m$GET_META_INFO$METADATA_INF$CLASS_INF$CLASS_OBJ
  axis    <- keep(axes, ~ .x[["@id"]] == cls_id)[[1]]
  classes <- axis$CLASS

  # NOTE: an axis with a single class comes back as one record instead of a list of records
  if (!is.null(classes[["@code"]])) {
    classes <- list(classes)
  }

  tibble(code = map_chr(classes, "@code"), name = map_chr(classes, "@name"))
}

# NOTE: log-linear interpolation between benchmark years; 2025 holds the 2024 value (rule = 2)
interpolate <- function(d, value_col = "value", by = NULL) {

  one_class <- function(x){

    x        <- x |> arrange(year)
    logvalue <- log(pmax(x[[value_col]], 1))                      # pmax guards against log(0)
    fitted   <- approx(x$year, logvalue, xout = YEARS, rule = 2)   # straight lines in log scale

    tibble(
      bin  = x$bin[1],
      year = YEARS,
      py   = exp(fitted$y)
    )
  }

  # NOTE: no grouping variable: one call per size class
  if (is.null(by)) {
    out <- map_dfr(BINS, function(b){
      x <- d |> filter(bin == b)
      one_class(x)
    })
    return(out)
  }

  # NOTE: with by = "grp" the same thing is done once per industry group and size class
  out <- map_dfr(unique(d[[by]]), function(g){
    rows <- map_dfr(BINS, function(b){
      x <- d |> filter(.data[[by]] == g, bin == b)
      one_class(x)
    })
    rows |> mutate(!!by := g)
  })
  out
}

# NOTE: keep the all-industry rows. The industry axis holds all industries, divisions and major
# groups side by side; summing them all would count every worker three times.
allindustry <- function(v, sizecol){

  # NOTE: the industry axis is the unpinned @cat column with several codes
  catcols <- grep("^@cat", names(v), value = TRUE)
  catcols <- setdiff(catcols, sizecol)
  nlevels <- map_int(catcols, function(ax) n_distinct(v[[ax]]))
  indcol  <- catcols[nlevels > 1][1]

  if (is.na(indcol)) return(v)

  # NOTE: all industries is the code with the largest total
  totals <- v |>
    group_by(code = .data[[indcol]]) |>
    summarise(total = sum(val), .groups = "drop")
  allind <- totals$code[which.max(totals$total)]

  v |> filter(.data[[indcol]] == allind)
}

## Read the finalized accident statistics-----
# NOTE: the worksheet is taken by name
read_kakutei <- function(kind, year, suffix = ""){

  files <- list.files(CACHE, pattern = sprintf("^%s_%d%s[.]xlsx?$", kind, year, suffix), full.names = TRUE)
  stopifnot(length(files) == 1)

  sheet <- grep("規模", excel_sheets(files), value = TRUE)[1]
  raw   <- read_excel(files, sheet = sheet, col_names = FALSE, col_types = "text")

  # NOTE: column 1 is the label (running number stripped), columns 2-7 the six size classes
  labels <- raw[[1]] |>
    replace_na("") |>
    str_replace("^[0-9]+[[:space:]]+", "") |>
    str_squish()

  counts <- raw[, 2:7] |>
    set_names(BINS) |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

  bind_cols(tibble(label = labels), counts)
}

# NOTE: a label can appear at three levels; take the last row only
pickrow <- function(d, label){

  hit <- which(d$label == label)
  if (length(hit) == 0) {
    zeros <- rep(0, length(BINS))
    return(zeros)
  }

  as.numeric(d[max(hit), BINS]) |> replace_na(0)
}

# NOTE: nine groups of the industrial accident classification. Finance and advertising, film and theatre,
# communications, education and research, cleaning and slaughtering and other are merged into one group,
# because JSIC major groups 39, 41, 72 and 74 straddle them.
groupcounts <- function(d){
  tibble(
    bin            = BINS,
    `農林水産`     = pickrow(d, "農林業小計")     + pickrow(d, "畜産･水産業小計"),
    `鉱業`         = pickrow(d, "鉱業小計"),
    `建設業`       = pickrow(d, "建設業小計"),
    `製造業`       = pickrow(d, "製造業小計"),
    `運輸・貨物`   = pickrow(d, "運輸交通業小計") + pickrow(d, "貨物取扱小計"),
    `商業`         = pickrow(d, "商業"),
    `保健衛生`     = pickrow(d, "保健衛生業"),
    `接客娯楽`     = pickrow(d, "接客娯楽"),
    `その他`       = pickrow(d, "金融広告業")     + pickrow(d, "映画・演劇業") +
                     pickrow(d, "通信業")         + pickrow(d, "教育研究") +
                     pickrow(d, "清掃・と畜")     + pickrow(d, "その他の事業")
  )
}

## e-Stat table specifications-----
# NOTE: reg = regular employees, est = establishments, ind = regular employees by JSIC major group
estattable <- tribble(
  ~tag,  ~year, ~id,          ~tab,       ~sizecol, ~indcol,
  "reg",  2009, "0003032572", "004",      "cat03",  NA,
  "reg",  2012, "0003090096", "005",      "cat03",  NA,
  "reg",  2014, "0003111125", "004",      "cat02",  NA,
  "reg",  2016, "0003218663", "005",      "cat01",  NA,
  "reg",  2021, "0004005662", "113-2021", "cat02",  NA,
  "reg",  2024, "0004040083", "105-2024", "cat02",  NA,

  "est",  2009, "0003032571", "003",      "cat02",  NA,
  "est",  2012, "0003090074", "004",      "cat02",  NA,
  "est",  2014, "0003111122", "003",      "cat01",  NA,
  "est",  2016, "0003218662", "004",      "cat01",  NA,
  "est",  2021, "0004005645", "102-2021", "cat02",  NA,
  "est",  2024, "0004040083", "102-2024", "cat02",  NA,

  "ind",  2009, "0003032575", "004",      "cat01",  "cat05",
  "ind",  2012, "0003090097", "893",      "cat04",  "cat03",
  "ind",  2014, "0003111123", "004",      "cat04",  "cat05",
  "ind",  2016, "0003218664", "893",      "cat03",  "cat04",
  "ind",  2021, "0004005646", "121-2021", "cat03",  "cat01",
  "ind",  2024, "0004040084", "105-2024", "cat02",  "cat01"
)

# NOTE: axes pinned to one code. In 2009 cat04 is employment status; without 001 the query
# returns persons engaged (15.9% larger).
estatfix <- tribble(
  ~tag,  ~year, ~param,    ~code,
  "reg",  2009, "cdCat02", "004",
  "reg",  2012, "cdCat01", "004",
  "reg",  2014, "cdCat01", "004",
  "reg",  2016, "cdCat03", "0060",
  "reg",  2021, "cdCat03", "4",
  "reg",  2024, "cdCat03", "0",

  "est",  2024, "cdCat03", "0",

  "ind",  2009, "cdCat04", "001",
  "ind",  2009, "cdCat02", "000",
  "ind",  2009, "cdCat03", "000",
  "ind",  2012, "cdCat01", "000",
  "ind",  2012, "cdCat02", "000",
  "ind",  2014, "cdCat01", "012",
  "ind",  2014, "cdCat02", "000",
  "ind",  2014, "cdCat03", "001",
  "ind",  2016, "cdCat01", "000",
  "ind",  2016, "cdCat02", "0000",
  "ind",  2021, "cdCat02", "0",
  "ind",  2021, "cdCat04", "0",
  "ind",  2024, "cdCat03", "0"
)

# NOTE: published class name -> six classes. 0人, 総数 and 1人-4人 (inside 1～4人) are not listed,
# so they are dropped rather than double counted.
classtobin <- tribble(
  ~class,        ~bin,
  "1～4人",      "1-9",
  "5～9人",      "1-9",
  "10～19人",    "10-29",
  "20～29人",    "10-29",
  "30～49人",    "30-49",
  "50～99人",    "50-99",
  "100～199人",  "100-299",
  "200～299人",  "100-299",
  "300～499人",  "300+",
  "500～999人",  "300+",
  "1000人以上",  "300+",
  "1,000人以上", "300+",
  "300人以上",   "300+"
)
# FOOTNOTE: 2016 writes 1,000人以上 with a comma

# NOTE: the nine groups as ranges of JSIC major groups 01-96 (97-98 public administration excluded),
# following the official correspondence between the accident classification and JSIC (業種区分一覧表, JSIC 2013).
# Electricity, gas and water (33-36) and vehicle and machine repair (89-90) are manufacturing; warehousing (47),
# real estate rental (69), goods rental (70) and take-out and delivery meals (77) are commerce; postal services
# (49, 86) are communications.
# NOTE: minor groups placed elsewhere than their major group are moved in 03_build.R (SUBMOVE below)
GRPMAP <- tribble(
  ~grp,           ~from, ~to,
  "農林水産",         1,   4,
  "鉱業",             5,   5,
  "建設業",           6,   8,
  "製造業",           9,  36,
  "その他",          37,  41,
  "運輸・貨物",      42,  46,
  "商業",            47,  47,
  "運輸・貨物",      48,  48,
  "その他",          49,  49,
  "商業",            50,  61,
  "その他",          62,  68,
  "商業",            69,  70,
  "その他",          71,  74,
  "接客娯楽",        75,  76,
  "商業",            77,  77,
  "商業",            78,  78,
  "その他",          79,  79,
  "接客娯楽",        80,  80,
  "その他",          81,  82,
  "保健衛生",        83,  85,
  "その他",          86,  88,
  "製造業",          89,  90,
  "その他",          91,  96
)

# NOTE: JSIC minor groups the correspondence places in another group than their major group.
# A minor group split across groups by its detailed (four-digit) groups goes where most of its 2016 regular
# employees are; the rest is a known misfit, noted in GRPNOTE.
SUBMOVE <- tribble(
  ~minor, ~from,        ~to,
  "413",  "その他",     "商業",
  "414",  "その他",     "商業",
  "484",  "運輸・貨物", "製造業",
  "489",  "運輸・貨物", "その他",
  "681",  "その他",     "商業",
  "781",  "商業",       "製造業",
  "784",  "商業",       "保健衛生",
  "785",  "商業",       "保健衛生",
  "793",  "その他",     "製造業",
  "794",  "その他",     "商業",
  "795",  "その他",     "商業",
  "796",  "その他",     "商業",
  "801",  "接客娯楽",   "その他",
  "802",  "接客娯楽",   "その他",
  "851",  "保健衛生",   "その他",
  "921",  "その他",     "製造業"
)

# NOTE: the minor-group number is at the head of the name (2009-2016) or is the code (2021, 2024).
# Minor groups in SUBMOVE, read from the all-industry table (minor group x size, every benchmark year)
submoves_for <- function(year){

  values <- read_rds(file.path(CACHE, sprintf("estat_reg_%d.rds", year)))
  spec   <- estattable |> filter(tag == "reg", year == !!year)
  szcls  <- estat_meta(spec$id, spec$sizecol) |> inner_join(classtobin, by = c("name" = "class"))

  catcols <- setdiff(str_remove(grep("^@cat", names(values), value = TRUE), "^@"), spec$sizecol)
  indcol  <- catcols[map_int(catcols, function(ax) n_distinct(values[[paste0("@", ax)]])) > 1]
  stopifnot(length(indcol) == 1)

  minors <- estat_meta(spec$id, indcol) |>
    mutate(minor = coalesce(str_match(name, "^[[:space:]]*([0-9]{3})(?![0-9A-Z])")[, 2],
                            if_else(str_detect(code, "^[0-9]{3}$") & !str_detect(name, "^[[:space:]]*[0-9]"), code, NA_character_))) |>
    filter(minor %in% SUBMOVE$minor)
  stopifnot(setequal(minors$minor, SUBMOVE$minor))

  values |>
    transmute(indcode = .data[[paste0("@", indcol)]],
              szcode  = .data[[paste0("@", spec$sizecol)]],
              val     = suppressWarnings(as.numeric(`$`))) |>
    inner_join(minors |> select(indcode = code, minor), by = "indcode") |>
    inner_join(szcls  |> select(szcode = code, bin),     by = "szcode") |>
    filter(!is.na(val)) |>
    summarise(value = sum(val), .by = c(minor, bin)) |>
    mutate(year = year)
}

# NOTE: subtract the moved minor groups from their group and add them to the target group
applymoves <- function(den, moves){
  moved <- moves |>
    left_join(SUBMOVE, by = "minor") |>
    summarise(value = sum(value), .by = c(year, bin, from, to))
  den |>
    left_join(moved |> summarise(out = sum(value), .by = c(year, bin, from)), by = c("year", "bin", "grp" = "from")) |>
    left_join(moved |> summarise(inn = sum(value), .by = c(year, bin, to)),   by = c("year", "bin", "grp" = "to")) |>
    mutate(value = value - coalesce(out, 0) + coalesce(inn, 0)) |>
    select(-out, -inn)
}

# NOTE: why a JSIC group sits where it does, for the rows that are not obvious from the group name.
# Source: 業種区分一覧表 (安全衛生統計の業種分類と日本標準産業分類との対比表, JSIC 2013),
# published as https://www.horei.co.jp/linkbg/2015_9bg.pdf;
# employee shares of detailed groups from the 2016 Economic Census (e-Stat 0003218580)
GRPNOTE <- tribble(
  ~jsic, ~note,
  "33",  "労災統計では製造業（電気・ガス・水道業）",
  "34",  "労災統計では製造業（電気・ガス・水道業）",
  "35",  "労災統計では製造業（電気・ガス・水道業）",
  "36",  "労災統計では製造業（電気・ガス・水道業）",
  "39",  "ソフトウェア業は教育・研究業、情報処理はその他の事業、情報提供の一部は商業。商業の部分は分けられない",
  "41",  "411 映画製作は映画・演劇業。413・414 は小分類で商業へ移す（印刷部門は製造業だが分けられない）",
  "47",  "労災統計では商業（その他の商業 > 倉庫業）",
  "48",  "484 こん包業は製造業、489 は表に個別の記載がなくその他の事業。小分類で移す",
  "49",  "労災統計では通信業",
  "55",  "5598 仲立業は金融・広告業だが分けられない",
  "68",  "681 建物売買業は商業、682 不動産仲介は金融・広告業（代理商は商業だが分けられない）",
  "69",  "労災統計では商業（その他の商業）",
  "70",  "労災統計では商業（その他の商業）",
  "72",  "表に個別の記載がなく、その他の事業",
  "73",  "労災統計では金融・広告業（広告・あっせん業）",
  "77",  "労災統計では商業（小売業）",
  "78",  "理容・美容は商業。781 洗濯業、784・785 浴場は小分類で移す。789 は人数の多いエステ・ネイル（商業）に合わせる",
  "79",  "旅行業は金融・広告業。793・794・795・796 は小分類で移す",
  "80",  "801 映画館・802 興行場は小分類で移す。8048 フィットネスクラブ（2016年で 804 の29%）は教育・研究業だが分けられない",
  "83",  "8361 歯科技工所（2016年で 836 の27%）は製造業だが分けられない",
  "85",  "851 社会保険事業団体は保健衛生業に含まれない。小分類で移す",
  "86",  "労災統計では通信業（郵便局）",
  "87",  "表に個別の記載がなく、その他の事業",
  "89",  "労災統計では製造業（自動車整備業）",
  "90",  "労災統計では製造業（機械修理業）",
  "91",  "派遣労働者の災害は派遣先の業種で計上されるため、この群の分母は過大",
  "92",  "921 は製造業（印刷）なので小分類で移す。922 清掃・9292 産業設備洗浄は清掃・と畜業",
  "413", "労災統計では商業（その他の商業）。印刷部門は製造業",
  "414", "労災統計では商業（その他の商業）。印刷部門は製造業",
  "484", "労災統計では製造業（その他の製造業）",
  "489", "表に運輸・貨物の記載がなく、その他の事業。表の注で個別判断が必要とされる",
  "681", "労災統計では商業（その他の商業）",
  "781", "労災統計では製造業（クリーニング業）。7812 洗濯物取次業（2016年で16%）は金融・広告業",
  "784", "労災統計では保健衛生業（浴場業）",
  "785", "労災統計では保健衛生業（浴場業）。個室付浴場業は接客娯楽業",
  "793", "労災統計では製造業（その他の製造業）",
  "794", "労災統計では商業（その他の商業）",
  "795", "7952 墓地管理業（2016年で54%）は商業、7951 火葬業は清掃・と畜業",
  "796", "7961 葬儀業（2016年で55%）は商業、7962 結婚式場業（35%）は接客娯楽業、互助会はその他の事業",
  "801", "労災統計では映画・演劇業（映画館）",
  "802", "労災統計では映画・演劇業",
  "851", "社会保険事務所は官公署、それ以外は表に記載がなくその他の事業",
  "921", "労災統計では製造業（印刷・製本業）"
)

GRPMAP |> mutate(n = to - from + 1) |> summarise(covered = sum(n))
# FOOTNOTE: 96, so every JSIC major group from 01 to 96 is assigned exactly once

# NOTE: English display names
GRP_EN <- c(
  "建設業"     = "Construction",
  "製造業"     = "Manufacturing",
  "運輸・貨物" = "Transport and cargo handling",
  "商業"       = "Commerce",
  "農林水産"   = "Agriculture, forestry and fisheries",
  "接客娯楽"   = "Hospitality and amusement",
  "保健衛生"   = "Health care and hygiene",
  "鉱業"       = "Mining",
  "その他"     = "Other services")

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

## Where every file comes from-----
KAKUTEI_BASE <- "https://anzeninfo.mhlw.go.jp/user/anzen/tok/"

# NOTE: era prefix of the workbook name
erastem <- function(year){

  if (year <= 2018) {
    era <- sprintf("h%02d", year - 1988)
  } else if (year == 2019) {
    era <- "h31"
  } else {
    era <- sprintf("r%d", year - 2018)
  }

  era
}

# NOTE: (kakutei) up to 2020, R3 in 2021, lower case .xlsx from 2022
kakutei_url <- function(kind, year){

  stem <- if (kind == "death") "sibou" else "sisyou"

  if (year <= 2020) {
    name <- sprintf("%s_%s(kakutei).xls", erastem(year), stem)
  } else if (year == 2021) {
    name <- sprintf("R3_16_%s.xls", stem)
  } else {
    name <- sprintf("%s_16_%s.xlsx", erastem(year), stem)
  }

  paste0(KAKUTEI_BASE, name)
}

# NOTE: COVID-19-excluded versions, published for 2020 and 2021 only
kakuteiex <- tribble(
  ~kind,    ~year, ~name,
  "death",   2020, "r2_16_sibou(exceptCORONA).xlsx",
  "injury",  2020, "r2_16_sisyou(exceptCORONA).xlsx",
  "death",   2021, "R3_16_sibou(exceptCORONA).xlsx",
  "injury",  2021, "R3_16_sisyou(exceptCORONA).xlsx"
)
EXYEARS <- c(2020, 2021)

kakuteiex_url <- function(kind, year){
  name <- kakuteiex$name[kakuteiex$kind == kind & kakuteiex$year == year]
  paste0(KAKUTEI_BASE, name)
}

kakuteiex_file <- function(kind, year){
  file.path(CACHE, sprintf("%s_%d_excovid.xlsx", kind, year))
}

kakutei_file <- function(kind, year){
  file.path(CACHE, sprintf("%s_%d.%s", kind, year, if (year >= 2022) "xlsx" else "xls"))
}

# NOTE: the recorded address carries YOUR_APPID, not the key
estat_url <- function(tag, year, hide_key = TRUE){

  spec   <- estattable |> filter(tag == !!tag, year == !!year)
  pinned <- estatfix   |> filter(tag == !!tag, year == !!year)
  key    <- if (hide_key) "YOUR_APPID" else appid()

  paste0("https://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=", key,
         "&statsDataId=", spec$id, "&cdTab=", spec$tab, "&cdArea=00000",
         "&metaGetFlg=N&limit=100000",
         if (nrow(pinned)) paste0("&", pinned$param, "=", pinned$code, collapse = "") else "")
}

# NOTE: 事業内容等不詳を除いた民営事業所数 from each census summary PDF (read 2026-10-03)
censuspdf <- tribble(
  ~year, ~establishments, ~url,
  2009,  5886193, "https://www.stat.go.jp/data/e-census/2009/kakuho/gaiyou/pdf/gaiyou.pdf",
  2012,  5453635, "https://www.stat.go.jp/data/e-census/2012/kakuho/pdf/gaiyo.pdf",
  2014,  5541634, "https://www.stat.go.jp/data/e-census/2014/pdf/kaku_gaiyo.pdf",
  2016,  5340783, "https://www.stat.go.jp/data/e-census/2016/kekka/pdf/k_gaiyo.pdf",
  2021,  5156063, "https://www.stat.go.jp/data/e-census/2021/kekka/pdf/k_outline.pdf",
  2024,  4023941, "https://www.stat.go.jp/data/e-census/2024/pdf/kekka_k.pdf"
)
# NOTE: 2024 excludes 雇用者のいない個人経営の事業所 (https://www.stat.go.jp/data/e-census/2024/kekka.html)

## Check that the specifications point at the intended data-----
# NOTE: read every pinned code and size class back as its name
binsfor <- function(sizemeta){
  sizemeta |>
    inner_join(classtobin, by = c("name" = "class")) |>
    group_by(bin) |>
    summarise(classes = paste(name, collapse = " + "), .groups = "drop")
}

readspec <- function(i){

  spec <- estattable[i, ]
  tabs <- estat_meta(spec$id, "tab")
  size <- estat_meta(spec$id, spec$sizecol)
  pin  <- estatfix |> filter(tag == spec$tag, year == spec$year)

  # NOTE: e.g. "cdCat03=常用雇用者"
  pinned <- map_chr(seq_len(nrow(pin)), function(j){
    axis <- str_to_lower(str_remove(pin$param[j], "^cd"))   # "cdCat03" -> "cat03"
    meta <- estat_meta(spec$id, axis)
    paste0(pin$param[j], "=", meta$name[meta$code == pin$code[j]])
  })

  bins <- binsfor(size)

  tibble(
    tag      = spec$tag,
    year     = spec$year,
    measure  = tabs$name[tabs$code == spec$tab],
    pinned   = if (length(pinned)) paste(pinned, collapse = ", ") else "",
    binsok   = all(BINS %in% bins$bin),
    industry = if (is.na(spec$indcol)) "" else sprintf("%s (%d codes)", spec$indcol,
                                                       nrow(estat_meta(spec$id, spec$indcol)))
  )
}

checkspec <- function() map_dfr(seq_len(nrow(estattable)), readspec)

check <- checkspec() |> print(n = 18, width = 160)
check
# FOOTNOTE: 18 rows, binsok TRUE in all; 常用雇用者 for reg/ind and 事業所数 for est
