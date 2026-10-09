library(tidyverse)
library(readxl)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# Builds output/panel_all and output/panel_industry from cache/

## Read the finalized accident statistics-----

## Numerator for all industries-----
# NOTE: public administration subtracted (not in the denominator)
# NOTE: published series (incl. COVID-19 in 2020-21); the paper's series is built below
numall <- map_dfr(YEARS, function(year){

  dth <- read_kakutei("death",  year)
  inj <- read_kakutei("injury", year)

  tibble(
    year   = year,
    bin    = BINS,
    death  = pickrow(dth, "合計") - pickrow(dth, "官公署"),
    injury = pickrow(inj, "合計") - pickrow(inj, "官公署")
  )
})

numall |> filter(death <= 0 | injury <= 0)
# FOOTNOTE: zero rows

numall |> summarise(death = sum(death), injury = sum(injury))
# FOOTNOTE: 15,742 deaths and 2,131,627 reported injuries (published totals)

## Numerator by industry group-----
numind <- map_dfr(YEARS, function(year){

  dth <- groupcounts(read_kakutei("death",  year)) |>
    pivot_longer(!bin, names_to = "grp", values_to = "death")

  inj <- groupcounts(read_kakutei("injury", year)) |>
    pivot_longer(!bin, names_to = "grp", values_to = "injury")

  left_join(dth, inj, by = c("bin", "grp")) |> mutate(year = year)
})

# NOTE: the nine groups sum to total minus public administration
groupcheck <- numind |>
  group_by(year, bin) |>
  summarise(gdeath = sum(death), ginjury = sum(injury), .groups = "drop") |>
  left_join(numall, by = c("year", "bin")) |>
  mutate(diffdeath = gdeath - death, diffinjury = ginjury - injury)

mismatch <- groupcheck |> filter(diffdeath != 0 | diffinjury != 0)
mismatch
stopifnot(nrow(mismatch) == 0)
# FOOTNOTE: zero rows in all 102 cells

## Denominator for all industries-----
# NOTE: allindustry() keeps the all-industry rows
collapse_estat <- function(tag, year){

  values  <- read_rds(file.path(CACHE, sprintf("estat_%s_%d.rds", tag, year)))
  spec    <- estattable |> filter(tag == !!tag, year == !!year)
  sizecol <- paste0("@", spec$sizecol)

  # NOTE: codes not in classtobin (0人, 総数, single-worker codes) drop out
  sizemap <- estat_meta(spec$id, spec$sizecol) |>
    inner_join(classtobin, by = c("name" = "class")) |>
    select(code, bin)

  # NOTE: `$` is the value column; the join keeps the six classes
  v <- values |>
    mutate(val = as.numeric(`$`)) |>
    filter(!is.na(val)) |>
    inner_join(sizemap, by = setNames("code", sizecol))

  v <- allindustry(v, sizecol)

  bysize <- v |>
    group_by(bin) |>
    summarise(value = sum(val), .groups = "drop")

  tibble(bin = BINS) |>
    left_join(bysize, by = "bin") |>
    mutate(value = replace_na(value, 0), year = year)
}

denreg <- map_dfr(BENCHMARK, function(year) collapse_estat("reg", year))
estreg <- map_dfr(BENCHMARK, function(year) collapse_estat("est", year))

## Mean size of each class-----
# NOTE: regular employees per establishment, pooled over the benchmark years (Table 2 note: 687)
msize <- denreg |>
  left_join(estreg, by = c("bin", "year"), suffix = c("", "est")) |>
  group_by(bin) |>
  summarise(m = sum(value) / sum(valueest), .groups = "drop")

msize |> arrange(match(bin, BINS))
msize |> arrange(match(bin, BINS)) |> pull(m)


## Denominator by industry group-----
# NOTE: the two-digit JSIC number is in the code or at the head of the name
jsicnum <- function(code, name){

  fromcode <- if_else(str_detect(code, "^[0-9]{2}$"), code, NA_character_)
  fromname <- str_match(name, "^[[:space:]]*([0-9]{2})")[, 2]

  as.integer(coalesce(fromcode, fromname))
}

tojsicgroup <- function(num){

  hit <- GRPMAP |> filter(from <= num, num <= to) |> pull(grp)
  if (length(hit) == 1) hit else NA_character_
}

# NOTE: JSIC was revised in 2007, 2013 and 2023; the number must mean the same major group in all six years
jsicnames <- map_dfr(BENCHMARK, function(year){

  spec <- estattable |> filter(tag == "ind", year == !!year)

  estat_meta(spec$id, spec$indcol) |>
    mutate(num = jsicnum(code, name)) |>
    filter(!is.na(num)) |>
    transmute(year = year, num = num,
              name = name |>
                str_remove("^[[:space:]]*[0-9]{2}[[:space:]]*") |>
                str_replace_all("，", "、") |>
                str_remove_all("[（(][^）)]*[）)]") |>
                str_squish())
})

jsicnames |> count(year)
# FOOTNOTE: 95 in every year (01-95; 96 外国公務 is not in the tables)

renamed <- jsicnames |>
  distinct(num, name) |>
  count(num) |>
  filter(n > 1)

jsicnames |>
  filter(num %in% renamed$num) |>
  summarise(years = paste(year, collapse = ","), .by = c(num, name))
# FOOTNOTE: only 64: クレジットカード業等非預金信用機関 (2009) / 貸金業、クレジットカード業等非預金信用機関 (2012-2024), both 金融・広告
stopifnot(n_distinct(jsicnames$num[jsicnames$year == 2009]) == 95,
          all(table(jsicnames$num) == length(BENCHMARK)))

denind <- map_dfr(BENCHMARK, function(year){

  values <- read_rds(file.path(CACHE, sprintf("estat_ind_%d.rds", year)))
  spec   <- estattable |> filter(tag == "ind", year == !!year)

  indcls <- estat_meta(spec$id, spec$indcol)  |> mutate(num = jsicnum(code, name))
  szcls  <- estat_meta(spec$id, spec$sizecol) |>
    inner_join(classtobin, by = c("name" = "class"))

  # NOTE: industry code -> JSIC number -> group, size code -> class; summed per group and class
  rows <- values |>
    transmute(
      indcode = .data[[paste0("@", spec$indcol)]],
      szcode  = .data[[paste0("@", spec$sizecol)]],
      val     = as.numeric(`$`)
    )

  indlookup <- indcls |> select(indcode = code, num)
  szlookup  <- szcls  |> select(szcode  = code, bin)

  rows |>
    left_join(indlookup, by = "indcode") |>
    left_join(szlookup,  by = "szcode") |>
    filter(!is.na(bin), !is.na(num), !is.na(val)) |>
    mutate(grp = map_chr(num, tojsicgroup)) |>
    filter(!is.na(grp)) |>
    group_by(grp, bin) |>
    summarise(value = sum(val), .groups = "drop") |>
    mutate(year = year)
})

submoves <- map_dfr(BENCHMARK, submoves_for)

submoves |> summarise(value = sum(value), .by = c(year, minor)) |> pivot_wider(names_from = minor, values_from = value)
# FOOTNOTE: 2021: 16 minor groups, from 6,208 (795) to 217,756 (781); 923,437 in all

denind <- applymoves(denind, submoves)
stopifnot(all(denind$value > 0))

# NOTE: the nine groups cover JSIC 01-96, slightly short of the all-industry total
groupsum <- denind |> group_by(year) |> summarise(groups = sum(value), .groups = "drop")
allsum   <- denreg |> group_by(year) |> summarise(allind = sum(value), .groups = "drop")

indshare <- groupsum |>
  left_join(allsum, by = "year") |>
  mutate(pct = 100 * groups / allind)

indshare
stopifnot(indshare$pct > 97, indshare$pct < 101)
# FOOTNOTE: 100.0 / 98.8 / 100.0 / 99.9 / 100.0 / 100.0

## The same counts with the COVID-19 cases removed-----
# NOTE: only 2020 and 2021 have this version; from 2022 the published counts exclude COVID-19
excvall <- map_dfr(EXYEARS, function(year){

  dth <- read_kakutei("death",  year, "_excovid")
  inj <- read_kakutei("injury", year, "_excovid")

  tibble(
    year   = year,
    bin    = BINS,
    death  = pickrow(dth, "合計") - pickrow(dth, "官公署"),
    injury = pickrow(inj, "合計") - pickrow(inj, "官公署")
  )
})

excvind <- map_dfr(EXYEARS, function(year){

  dth <- groupcounts(read_kakutei("death",  year, "_excovid")) |>
    pivot_longer(!bin, names_to = "grp", values_to = "death")

  inj <- groupcounts(read_kakutei("injury", year, "_excovid")) |>
    pivot_longer(!bin, names_to = "grp", values_to = "injury")

  left_join(dth, inj, by = c("bin", "grp")) |> mutate(year = year)
})

# NOTE: removing cases can only lower a count
excvcheck <- excvall |>
  left_join(numall, by = c("year", "bin"), suffix = c("ex", "")) |>
  mutate(cvdeath = death - deathex, cvinjury = injury - injuryex)

excvcheck |> select(year, bin, cvdeath, cvinjury)
stopifnot(excvcheck$cvdeath >= 0, excvcheck$cvinjury >= 0)
# FOOTNOTE: the COVID-19 cases removed are 18 and 89 deaths, 6,041 and 19,330 reported injuries

## Main numerator with COVID-19 excluded in every year-----
# NOTE: 2020-21 replaced by the COVID-19-removed counts; published counts kept in *_pub
numall <- numall |>
  rename(death_pub = death, injury_pub = injury) |>
  left_join(excvall, by = c("year", "bin")) |>
  mutate(death = coalesce(death, death_pub), injury = coalesce(injury, injury_pub))

numind <- numind |>
  rename(death_pub = death, injury_pub = injury) |>
  left_join(excvind, by = c("year", "grp", "bin")) |>
  mutate(death = coalesce(death, death_pub), injury = coalesce(injury, injury_pub))

numall |> summarise(death = sum(death), injury = sum(injury), death_pub = sum(death_pub), injury_pub = sum(injury_pub))
# FOOTNOTE: 15,635 deaths and 2,106,256 injuries (published 15,742 and 2,131,627; COVID-19 107 and 25,371)
stopifnot(sum(numall$death_pub - numall$death) == 107, sum(numall$injury_pub - numall$injury) == 25371)

## Build the panels-----
dat <- interpolate(denreg) |>
  left_join(numall, by = c("bin", "year")) |>
  left_join(msize,  by = "bin") |>
  mutate(bin = factor(bin, BINS)) |>
  arrange(bin, year)

ind <- interpolate(denind, by = "grp") |>
  left_join(numind, by = c("grp", "bin", "year")) |>
  left_join(msize,  by = "bin") |>
  mutate(bin = factor(bin, BINS)) |>
  arrange(grp, bin, year)

stopifnot(nrow(dat) == 102, nrow(ind) == 918)
stopifnot(!anyNA(dat), !anyNA(ind))

write_rds(dat, file.path(OUT, "panel_all.rds"))
write_rds(ind, file.path(OUT, "panel_industry.rds"))

# NOTE: the industry correspondence for checking: every JSIC major group and every moved minor group,
# with its 2021 name and 2021 regular employees
ind2021  <- estattable |> filter(tag == "ind", year == 2021)
major2021 <- read_rds(file.path(CACHE, "estat_ind_2021.rds")) |>
  transmute(code = .data[[paste0("@", ind2021$indcol)]], szcode = .data[[paste0("@", ind2021$sizecol)]],
            val = suppressWarnings(as.numeric(`$`))) |>
  inner_join(estat_meta(ind2021$id, ind2021$sizecol) |> inner_join(classtobin, by = c("name" = "class")) |>
               select(szcode = code), by = "szcode") |>
  summarise(employees_2021 = sum(val, na.rm = TRUE), .by = code) |>
  inner_join(estat_meta(ind2021$id, ind2021$indcol), by = "code") |>
  mutate(num = jsicnum(code, name)) |>
  filter(!is.na(num)) |>
  transmute(level = "major", jsic = sprintf("%02d", num), name,
            group = map_chr(num, tojsicgroup), employees_2021)

minor2021 <- submoves |>
  filter(year == 2021) |>
  summarise(employees_2021 = sum(value), .by = minor) |>
  left_join(SUBMOVE, by = "minor") |>
  left_join(estat_meta(estattable$id[estattable$tag == "reg" & estattable$year == 2021], "cat01"),
            by = c("minor" = "code")) |>
  transmute(level = "minor", jsic = minor, name, group = to, employees_2021)

jsicgroups <- bind_rows(major2021, minor2021) |>
  mutate(group_en = GRP_EN[group]) |>
  left_join(GRPNOTE, by = "jsic") |>
  mutate(note = replace_na(note, "")) |>
  select(level, jsic, name, group, group_en, employees_2021, note) |>
  arrange(jsic)

jsicgroups |> count(level)
stopifnot(sum(jsicgroups$level == "major") == 95, sum(jsicgroups$level == "minor") == nrow(SUBMOVE),
          !anyNA(jsicgroups$group), all(GRPNOTE$jsic %in% jsicgroups$jsic))
# FOOTNOTE: 95 major groups (01-95) and 16 minor groups
write_excel_csv(jsicgroups, file.path(OUT, "jsic_groups.csv"))

# NOTE: panels as csv; *_published columns include COVID-19 in 2020-21
dat |>
  transmute(year, size = bin, deaths = death, reported_injuries = injury,
            deaths_published = death_pub, reported_injuries_published = injury_pub,
            regular_employee_years = round(py)) |>
  write_excel_csv(file.path(OUT, "panel_all.csv"))

ind |>
  transmute(year, industry = grp, size = bin, deaths = death, reported_injuries = injury,
            deaths_published = death_pub, reported_injuries_published = injury_pub,
            regular_employee_years = round(py)) |>
  write_excel_csv(file.path(OUT, "panel_industry.csv"))
