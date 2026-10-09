library(tidyverse)
library(readxl)
library(jsonlite)

# Fatal and reported occupational injury rates by establishment size in Japan, 2009-2025
# Builds the panel output/panel_all.csv from data/ and prints Tables 1 to 4. Run from this folder.

DATA <- "data"
dir.create("output", showWarnings = FALSE)

BINS      <- c("1-9", "10-29", "30-49", "50-99", "100-299", "300+")
YEARS     <- 2009:2025
BENCHMARK <- c(2009, 2012, 2014, 2016, 2021, 2024)

## Numerator-----
# NOTE: worksheet 5 (業種・規模). Column 1 is the label, columns 2-7 the six size classes.
read_kakutei <- function(file){
  path  <- file.path(DATA, file)
  sheet <- grep("規模", excel_sheets(path), value = TRUE)[1]
  raw   <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  tibble(label = raw[[1]] |> replace_na("") |> str_remove("^[0-9]+[[:space:]]+") |> str_squish()) |>
    bind_cols(raw[, 2:7] |> set_names(BINS) |> mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))))
}

# NOTE: a label can appear at several levels; the last row is the one wanted
pickrow <- function(d, label){
  hit <- which(d$label == label)
  if (length(hit) == 0) return(rep(0, length(BINS)))
  as.numeric(d[max(hit), BINS]) |> replace_na(0)
}

# NOTE: public administration is subtracted, because the Economic Census covers private establishments only.
# 建設業小計 is construction in the accident statistics' own industry classification.
counts <- function(kind, year, suffix = ""){
  d <- read_kakutei(sprintf("%s_%d%s.%s", kind, year, suffix,
                            if (suffix != "" || year >= 2022) "xlsx" else "xls"))
  tibble(year = year, bin = BINS, kind = kind,
         n   = pickrow(d, "合計") - pickrow(d, "官公署"),
         con = pickrow(d, "建設業小計"))
}

num <- map_dfr(YEARS, function(y) bind_rows(counts("death", y), counts("injury", y)))
numex <- map_dfr(c(2020, 2021), function(y) bind_rows(counts("death", y, "_excovid"), counts("injury", y, "_excovid")))

# NOTE: the published counts include occupational COVID-19 in 2020 and 2021; the main series removes it
num <- num |>
  left_join(numex, by = c("year", "bin", "kind"), suffix = c("_pub", "")) |>
  mutate(n = coalesce(n, n_pub), con = coalesce(con, con_pub)) |>
  select(-con_pub) |>
  pivot_wider(names_from = kind, values_from = c(n, n_pub, con)) |>
  rename(death = n_death, injury = n_injury, death_pub = n_pub_death, injury_pub = n_pub_injury,
         death_con = con_death, injury_con = con_injury)

num |> summarise(across(death:injury_con, sum))

## Denominator-----
tables <- read_csv(file.path(DATA, "estat_tables.csv"), col_types = cols(.default = "c")) |>
  mutate(year = as.integer(year))

# NOTE: census size class -> the six classes. 総数, 0人 and the single-worker classes drop out.
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

classes <- function(id, axis){
  m  <- fromJSON(file.path(DATA, sprintf("meta_%s.json", id)), simplifyVector = FALSE)
  ax <- keep(m$GET_META_INFO$METADATA_INF$CLASS_INF$CLASS_OBJ, ~ .x[["@id"]] == axis)[[1]]$CLASS
  if (!is.null(ax[["@code"]])) ax <- list(ax)
  tibble(code = map_chr(ax, "@code"), name = map_chr(ax, "@name"))
}

# NOTE: one census table by size class. The industry axis holds all industries, divisions and finer groups
# side by side, so one code is kept: all industries (the largest total) or JSIC division D construction.
census <- function(tag, year, industry = "all"){
  spec <- tables |> filter(tag == !!tag, year == !!year)
  size <- paste0("@", spec$sizecol)

  v <- read_rds(file.path(DATA, sprintf("estat_%s_%d.rds", tag, year))) |>
    mutate(val = suppressWarnings(as.numeric(`$`))) |>
    filter(!is.na(val)) |>
    inner_join(classes(spec$id, spec$sizecol) |> inner_join(classtobin, by = c("name" = "class")) |> select(code, bin),
               by = setNames("code", size))

  catcols <- setdiff(grep("^@cat", names(v), value = TRUE), size)
  indcol  <- catcols[which.max(map_int(catcols, ~ n_distinct(v[[.x]])))]
  keep    <- if (industry == "all") {
    v |> summarise(total = sum(val), .by = all_of(indcol)) |> slice_max(total) |> pull(1)
  } else {
    classes(spec$id, str_remove(indcol, "^@")) |> filter(str_detect(name, "^(D[[:space:]]*)?建設業$")) |> pull(code)
  }
  stopifnot(length(keep) == 1)

  v |>
    filter(.data[[indcol]] == keep) |>
    summarise(value = sum(val), .by = bin) |>
    mutate(year = year)
}

reg <- map_dfr(BENCHMARK, ~ census("reg", .x))
eng <- map_dfr(BENCHMARK, ~ census("eng", .x))
est <- map_dfr(BENCHMARK, ~ census("est", .x))
con <- map_dfr(BENCHMARK, ~ census("reg", .x, "construction"))

# NOTE: without construction, JSIC division D is subtracted before interpolation. Subtracting the division keeps the
# establishments whose industry is undetermined below it (格付不能).
nocon <- reg |> left_join(con, by = c("bin", "year"), suffix = c("", "_con")) |> mutate(value = value - value_con)

# NOTE: log-linear interpolation between benchmark years; 2025 holds the 2024 value (rule = 2)
interpolate <- function(d){
  map_dfr(BINS, function(b){
    x <- d |> filter(bin == b) |> arrange(year)
    tibble(bin = b, year = YEARS, py = exp(approx(x$year, log(x$value), xout = YEARS, rule = 2)$y))
  })
}

## Panel-----
dat <- interpolate(reg) |>
  left_join(interpolate(eng)   |> rename(eng = py),      by = c("bin", "year")) |>
  left_join(interpolate(nocon) |> rename(py_nocon = py), by = c("bin", "year")) |>
  left_join(num, by = c("bin", "year")) |>
  mutate(bin = factor(bin, BINS)) |>
  arrange(year, bin)

stopifnot(nrow(dat) == 102, !anyNA(dat))

# NOTE: *_published include occupational COVID-19 in 2020 and 2021
dat |>
  transmute(year, size = bin, deaths = death, reported_injuries = injury,
            deaths_published = death_pub, reported_injuries_published = injury_pub,
            regular_employee_years = round(py), persons_engaged_years = round(eng),
            deaths_construction = death_con, reported_injuries_construction = injury_con,
            regular_employee_years_excl_construction = round(py_nocon)) |>
  write_excel_csv("output/panel_all.csv")

## Tables-----
# NOTE: Spearman's rho of the values against their order (smallest class first, or first year first)
rho <- function(x) cor(seq_along(x), x, method = "spearman")

# NOTE: rates per 100,000 employee-years, pooled as sum of counts over sum of employee-years
rates <- function(d, by) d |>
  summarise(death = sum(death), injury = sum(injury), py = sum(py), .by = all_of(by)) |>
  mutate(fatal = death / py * 1e5, reported = injury / py * 1e5)

### Table 1-----
t1 <- rates(dat, "bin")
t1 |>
  mutate(death_pct = death / sum(death) * 100, injury_pct = injury / sum(injury) * 100,
         py_000 = py / 1e3, py_pct = py / sum(py) * 100) |>
  select(bin, death, death_pct, injury, injury_pct, py_000, py_pct, fatal, reported) |>
  print(width = Inf)
rates(dat, NULL)
t1 |> summarise(across(c(death, injury, py, fatal, reported), rho))

# NOTE: Table 1 note c, the share of employee-years in the benchmark years; note e, the mean size of the top class
dat |> summarise(benchmark_pct = sum(py[year %in% BENCHMARK]) / sum(py) * 100)
reg |> left_join(est, by = c("bin", "year"), suffix = c("", "_est")) |>
  filter(bin == "300+") |> summarise(mean_size = sum(value) / sum(value_est))

### Tables 2 and 3-----
byyear <- rates(dat, c("year", "bin"))

# NOTE: Table 2 shows each fatal rate with the number of deaths in parentheses
byyear |>
  mutate(cell = sprintf("%.2f (%s)", fatal, formatC(death, big.mark = ",", format = "d"))) |>
  select(year, bin, cell) |>
  pivot_wider(names_from = bin, values_from = cell) |>
  left_join(rates(dat, "year") |> transmute(year, overall = sprintf("%.2f (%s)", fatal, formatC(death, big.mark = ",", format = "d"))), by = "year") |>
  left_join(byyear |> summarise(rho = rho(fatal), .by = year), by = "year") |>
  print(n = 17, width = Inf)

byyear |>
  select(year, bin, reported) |>
  mutate(reported = round(reported)) |>
  pivot_wider(names_from = bin, values_from = reported) |>
  left_join(rates(dat, "year") |> transmute(year, overall = round(reported)), by = "year") |>
  left_join(byyear |> summarise(rho = rho(reported), .by = year), by = "year") |>
  print(n = 17, width = Inf)

# NOTE: bottom rows. rho over the 17 years for each class and overall
bind_rows(byyear, rates(dat, "year") |> mutate(bin = "Overall")) |>
  arrange(year) |>
  summarise(fatal = rho(fatal), reported = rho(reported), .by = bin)

# NOTE: Table 2, the pooled rates of the first and the last five years
dat |>
  mutate(period = case_when(year <= 2013 ~ "2009-2013", year >= 2021 ~ "2021-2025")) |>
  filter(!is.na(period)) |>
  (\(d) bind_rows(rates(d, c("period", "bin")), rates(d, "period") |> mutate(bin = "Overall")))() |>
  select(period, bin, fatal, death) |>
  arrange(period)

### Table 4-----
# NOTE: (a) published counts with COVID-19; (b) benchmark years only, no interpolation; (c) construction removed from
# the numerator and the denominator; (d) persons engaged as the denominator
sens <- list(
  main           = dat,
  covid          = dat |> mutate(death = death_pub, injury = injury_pub),
  benchmark      = dat |> filter(year %in% BENCHMARK),
  noconstruction = dat |> mutate(death = death - death_con, injury = injury - injury_con, py = py_nocon),
  engaged        = dat |> mutate(py = eng)
)

t4 <- imap_dfr(sens, function(d, nm){
  r <- rates(d, "bin") |> arrange(bin)
  tot <- rates(d, NULL)
  tibble(analysis = nm, outcome = c("fatal", "reported"), total = c(tot$fatal, tot$reported)) |>
    bind_cols(bind_rows(set_names(r$fatal, BINS), set_names(r$reported, BINS))) |>
    mutate(rho = c(rho(r$fatal), rho(r$reported)))
})

t4 |>
  arrange(outcome) |>
  mutate(across(total:`300+`, ~ if_else(outcome == "fatal", round(.x, 2), round(.x))), rho = round(rho, 2)) |>
  print(width = Inf)
