library(tidyverse)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# Checks that every number in the paper's text and tables comes out of the two panels.
# Each block: the sentence quoted, the quantity printed, then check(). Stops at the end if any check failed.

dat <- read_rds(file.path(OUT, "panel_all.rds"))      |> mutate(bin = factor(as.character(bin), BINS))
ind <- read_rds(file.path(OUT, "panel_industry.rds")) |> mutate(bin = factor(as.character(bin), BINS))

## Helpers-----
FAILED <- character()
CHECKS <- 0

# NOTE: check() compares a value with the printed figure; tol = half the printed rounding
check <- function(label, got, want, tol = 0){
  CHECKS <<- CHECKS + 1
  ok <- all(abs(got - want) <= tol)
  cat(sprintf("%-3s  %-62s  %-26s %s\n", if (ok) "ok" else "NG", label,
              paste(format(want, trim = TRUE), collapse = " "),
              paste(format(round(got, 4), trim = TRUE), collapse = " ")))
  if (!ok) FAILED <<- c(FAILED, label)
}

checkeq <- function(label, got, want){
  CHECKS <<- CHECKS + 1
  ok <- identical(as.character(got), as.character(want))
  cat(sprintf("%-3s  %-62s  %-26s %s\n", if (ok) "ok" else "NG", label,
              paste(want, collapse = ","), paste(got, collapse = ",")))
  if (!ok) FAILED <<- c(FAILED, label)
}

# NOTE: crude rate per 100,000 employee-years; byclass() gives it for the six classes
rate <- function(d, num) sum(d[[num]]) / sum(d$py) * 1e5

byclass <- function(d, num){
  map_dbl(BINS, function(b){
    x <- d |> filter(bin == b)
    rate(x, num)
  })
}

# NOTE: Spearman's rho of the value against position (smallest class or first year); NA left out
rho <- function(x){
  k <- which(!is.na(x))
  cor(k, x[k], method = "spearman")
}

# NOTE: rows of one slice in class order (1-9 first)
inorder <- function(x) x[match(BINS, as.character(x$bin)), ]

# NOTE: rho with the ranks, d and 1 - 6 * sum(d^2) / (n * (n^2 - 1)) shown
rho_shown <- function(x, labels, against = NULL){
  k      <- which(!is.na(x))
  r_pos  <- if (is.null(against)) rank(k) else rank(against[k])
  r_val  <- rank(x[k])
  d      <- r_pos - r_val
  n      <- length(k)
  shown  <- tibble(label = labels[k], value = round(x[k], 4), rank_of_order = r_pos, rank_of_value = r_val, d2 = d^2)
  print(as.data.frame(shown), row.names = FALSE)
  cat(sprintf("  rho = 1 - 6 * %g / (%d * (%d^2 - 1)) = %.4f\n", sum(d^2), n, n, 1 - 6 * sum(d^2) / (n * (n^2 - 1))))
  invisible(1 - 6 * sum(d^2) / (n * (n^2 - 1)))
}

# NOTE: prints the sentence the next block checks
sentence <- function(txt, where = NULL){
  cat("\n")
  if (!is.null(where)) cat("[", where, "] ", sep = "")
  cat("\"", txt, "\"\n", sep = "")
}

## Print the six tables-----
# NOTE: printed to compare with the Word tables
show <- function(title, d){ cat("\n--", title, "--\n"); print(as.data.frame(d), row.names = FALSE) }

# NOTE: Table 1 is GRPMAP and SUBMOVE of 00_setup.R; adjacent ranges of the same group are joined
majorspan <- function(from, to) if_else(from == to, sprintf("%02d", from), sprintf("%02d-%02d", from, to))
t0 <- GRPMAP |>
  arrange(from) |>
  mutate(run = cumsum(grp != lag(grp, default = ""))) |>
  group_by(run, grp) |>
  summarise(from = min(from), to = max(to), .groups = "drop") |>
  group_by(grp) |>
  summarise(major = paste(majorspan(from, to), collapse = ", ")) |>
  left_join(SUBMOVE |> group_by(grp = to)   |> summarise(added   = paste(minor, collapse = ", ")), by = "grp") |>
  left_join(SUBMOVE |> group_by(grp = from) |> summarise(removed = paste(minor, collapse = ", ")), by = "grp") |>
  mutate(across(c(added, removed), ~ replace_na(.x, "")))
# NOTE: same order of groups as Table 3
ord <- ind |> group_by(grp) |> summarise(n = sum(death)) |> arrange(desc(n)) |> pull(grp)
t0 <- t0 |> arrange(match(grp, ord)) |> mutate(grp = GRP_EN[grp])
show("Table 1. JSIC major and minor groups in each industry group", t0)

t1 <- dat |>
  group_by(bin) |>
  summarise(deaths = sum(death), reported = sum(injury), py = sum(py), .groups = "drop") |>
  arrange(match(bin, BINS)) |>
  transmute(bin, deaths, reported, employee_years_000 = round(py/1e3),
            fatal_rate = round(deaths/py*1e5, 2), reported_rate = round(reported/py*1e5))
show("Table 2. By establishment size, 2009-2025", t1)

# NOTE: Table 3 is ordered by deaths and Table 4 by reported injuries, as the manuscript prints them
indwide <- function(num, digits){
  ord <- ind |> group_by(grp) |> summarise(n = sum(.data[[num]]), .groups = "drop") |>
    arrange(desc(n)) |> pull(grp)
  ind |>
    group_by(grp, bin) |>
    summarise(v = round(sum(.data[[num]])/sum(py)*1e5, digits), .groups = "drop") |>
    mutate(grp = factor(grp, ord)) |>
    pivot_wider(names_from = bin, values_from = v) |>
    arrange(grp) |>
    mutate(grp = GRP_EN[as.character(grp)])
}
show("Table 3. Fatal injury rate by industry group and size", indwide("death", 2))
show("Table 4. Reported injury rate by industry group and size", indwide("injury", 0))

yearwide <- function(num, digits) dat |>
  group_by(year, bin) |>
  summarise(v = round(sum(.data[[num]])/sum(py)*1e5, digits), .groups = "drop") |>
  pivot_wider(names_from = bin, values_from = v) |>
  arrange(year)
show("Table 5. Fatal injury rate by size and year", yearwide("death", 2))
show("Table 6. Reported injury rate by size and year", yearwide("injury", 0))

## Quantities used by several sentences-----
# NOTE: byclass = six classes pooled, byyear = one row per year, bygroup = nine industry groups
fatal    <- byclass(dat, "death")
reported <- byclass(dat, "injury")
names(fatal) <- BINS; names(reported) <- BINS
fatal
reported

byyear <- dat |>
  group_by(year) |>
  summarise(death = sum(death), injury = sum(injury), py = sum(py), .groups = "drop") |>
  arrange(year) |>
  mutate(fatal = death / py * 1e5, reported = injury / py * 1e5)
byyear

bygroup <- ind |>
  group_by(grp) |>
  summarise(death = sum(death), injury = sum(injury), py = sum(py), .groups = "drop") |>
  mutate(fatal = death / py * 1e5, reported = injury / py * 1e5) |>
  arrange(desc(death))
bygroup

# NOTE: per year, rho across the six classes (the last column of Tables 5 and 6)
yearrho <- dat |> group_by(year) |> group_modify(function(x, k){
  x <- inorder(x)
  tibble(fatal = rho(x$death / x$py), reported = rho(x$injury / x$py))
}) |> ungroup()
yearrho

# NOTE: per class, rho over the 17 years (the bottom row of Tables 5 and 6)
classrho <- dat |> group_by(bin) |> group_modify(function(x, k){
  x <- x |> arrange(year)
  tibble(fatal = rho(x$death / x$py), reported = rho(x$injury / x$py))
}) |> ungroup() |> arrange(match(bin, BINS))
classrho

## ABSTRACT-----
cat("\n== ABSTRACT ==\n")

sentence("Results: 15,635 deaths and 2,106,256 reported injuries occurred over 836,146 thousand regular employee-years.")
totals <- dat |> summarise(deaths = sum(death), reported = sum(injury), py_000 = sum(py) / 1e3)
totals
check("15,635 deaths",                      totals$deaths,   15635)
check("2,106,256 reported injuries",        totals$reported, 2106256)
check("836,146 thousand regular employee-years", totals$py_000, 836146, 0.5)

sentence("Fatal injury rates fell from 3.59 per 100,000 at 1-9 workers to 0.39 at 300 or more, a 9.2-fold difference (rho = -1.00).")
# (the same sentence, with the six values, is in RESULTS below)
check("3.59 per 100,000 at 1-9 workers",   fatal["1-9"],  3.59, 0.005)
check("0.39 at 300 or more",               fatal["300+"], 0.39, 0.005)
check("a 9.2-fold difference",             fatal["1-9"] / fatal["300+"], 9.2, 0.05)
check("rho = -1.00 (fatal, six classes)",  rho(fatal), -1.00, 0.005)

sentence("Reported injury rates peaked at 30-49 workers (330) and fell to 124 at 300 or more (rho = -0.54).")
checkeq("peaked at 30-49 workers",           names(which.max(reported)), "30-49")
check("(330)",                               reported["30-49"], 330, 0.5)
check("124 at 300 or more",                  reported["300+"],  124, 0.5)
check("rho = -0.54 (reported, six classes)", rho(reported), -0.54, 0.005)

sentence("Annual rho stayed between -0.94 and -1.00 for fatal injury rates but moved from -0.83 in 2009 to -0.09 in 2025 for reported injuries.")
checkeq("between -0.94 and -1.00 (the only values taken)", sort(unique(round(yearrho$fatal, 2))), c(-1, -0.94))
check("-0.83 in 2009", yearrho$reported[yearrho$year == 2009], -0.83, 0.005)
check("-0.09 in 2025", yearrho$reported[yearrho$year == 2025], -0.09, 0.005)

sentence("Across industries, rankings by count and by rate correlated at 0.13 for deaths and -0.48 for reported injuries.")
countrate <- bygroup |>
  summarise(deaths   = cor(death,  fatal,    method = "spearman"),
            reported = cor(injury, reported, method = "spearman"))
countrate
check("0.13 for deaths",             countrate$deaths,   0.13, 0.005)
check("-0.48 for reported injuries", countrate$reported, -0.48, 0.005)

## SUBJECTS AND METHODS-----
cat("\n== SUBJECTS AND METHODS ==\n")

cat("\n[Occupational injury counts]\n")
sentence("For 2011, 1,314 deaths caused directly by the Great East Japan Earthquake are tabulated separately and are not included in the published figure of 1,024 used here.")
deaths2011 <- byyear$death[byyear$year == 2011]
deaths2011
check("the published figure of 1,024", deaths2011, 1024)

cat("\n[Number of workers by establishment size]\n")
sentence("The gap between the two measures is concentrated in the smaller classes, where regular employees were 74.3% of persons engaged in the 1-9 class in 2021 against 98.5% in the 300 or more class.")
# NOTE: persons engaged (cat03 = 0) comes in the same 2021 request as regular employees (cat03 = 4)
statusfile <- file.path(CACHE, "verify_status_2021.rds")

if (!file.exists(statusfile)) {
  cls <- estat_meta("0004005662", "cat02") |>
    inner_join(classtobin, by = c("name" = "class")) |> select(code, bin)
  one <- function(status){
    estat("0004005662", "113-2021", c(cdCat03 = status, cdCat01 = "AR")) |>
      transmute(code = `@cat02`, v = suppressWarnings(as.numeric(`$`))) |>
      inner_join(cls, by = "code") |>
      group_by(bin) |> summarise(v = sum(v, na.rm = TRUE), .groups = "drop")
  }
  engaged <- one("0") |> rename(engaged = v)
  regular <- one("4") |> rename(regular = v)
  status  <- left_join(engaged, regular, by = "bin")
  write_rds(status, statusfile)
}

status <- read_rds(statusfile) |> arrange(match(bin, BINS)) |> mutate(share = regular / engaged * 100)
status
check("regular employees of the status table add up to the 2021 denominator", sum(status$regular), sum(dat$py[dat$year == 2021]), 1)
check("74.3% of persons engaged in the 1-9 class",  status$share[status$bin == "1-9"],  74.3, 0.05)
check("98.5% in the 300 or more class",             status$share[status$bin == "300+"], 98.5, 0.05)

sentence("The six benchmark years account for 35% of the total employee-years.")
benchshare <- sum(dat$py[dat$year %in% BENCHMARK]) / sum(dat$py) * 100
benchshare
check("35% of the total employee-years", benchshare, 35, 0.5)

cat("\n[Industry groups]\n")
sentence("The nine groups account for 98.8% to 100.0% of the all-industry denominator.")
sentence("The shortfall is 1.2% in 2012 and 0.1% or less in the other five benchmark years.")
# NOTE: stated for the six benchmark years, where the denominator is a census count
grouppy <- ind |> group_by(year) |> summarise(groups = sum(py), .groups = "drop")
allpy   <- dat |> group_by(year) |> summarise(all = sum(py), .groups = "drop")
coverage <- grouppy |>
  left_join(allpy, by = "year") |>
  mutate(pct = groups / all * 100, shortfall = 100 - pct) |>
  filter(year %in% BENCHMARK)
coverage
check("98.8% to 100.0%",                        range(coverage$pct), c(98.8, 100.0), 0.05)
check("1.2% in 2012",                           coverage$shortfall[coverage$year == 2012], 1.2, 0.05)
check("0.1% or less in the other five",         max(coverage$shortfall[coverage$year != 2012]), 0.1, 0.05)

cat("\n[Data availability]\n")
sentence("The repository also contains the raw data, that is 38 workbooks of the finalized industrial accident statistics, 18 e-Stat API responses with their 17 classification dictionaries, ...")
manifest <- read_csv(file.path(ROOT, "evidence/manifest.csv"), show_col_types = FALSE)
filecounts <- manifest |>
  summarise(workbooks    = sum(str_detect(file, "/(death|injury)_.*[.]xlsx?$")),
            responses    = sum(str_detect(file, "/estat_.*[.]rds$")),
            dictionaries = sum(str_detect(file, "/meta_")))
filecounts
check("38 workbooks",                 filecounts$workbooks,    38)
check("18 e-Stat API responses",      filecounts$responses,    18)
check("17 classification dictionaries", filecounts$dictionaries, 17)

## RESULTS-----
cat("\n== RESULTS ==\n")

sentence("The fatal injury rate fell at every step, from 3.59 in the 1-9 class to 0.39 at 300 or more, a 9.2-fold difference (rho = -1.00).")
# NOTE: fatal = deaths / employee-years * 100,000. The inputs are the first columns of Table 2.
t1 |> mutate(fatal = deaths / (employee_years_000 * 1000) * 1e5) |> select(bin, deaths, employee_years_000, fatal)
rho_shown(fatal, BINS)
check("fell at every step",         sum(diff(fatal) < 0), 5)
check("from 3.59 in the 1-9 class", fatal["1-9"],  3.59, 0.005)
check("to 0.39 at 300 or more",     fatal["300+"], 0.39, 0.005)
check("a 9.2-fold difference",      fatal["1-9"] / fatal["300+"], 9.2, 0.05)
check("rho = -1.00",                rho(fatal), -1.00, 0.005)

sentence("Across the six classes it was 271, 269, 330, 280, 259 and 124 (rho = -0.54).")
sentence("The highest value was at 30-49, and the rate fell at every step above that class.")
sentence("The two classes below 30-49 were level with each other.")
t1 |> mutate(reported_rate = reported / (employee_years_000 * 1000) * 1e5) |> select(bin, reported, employee_years_000, reported_rate)
rho_shown(reported, BINS)
check("271, 269, 330, 280, 259 and 124",       reported, c(271, 269, 330, 280, 259, 124), 0.5)
check("rho = -0.54",                            rho(reported), -0.54, 0.005)
checkeq("The highest value was at 30-49",       names(which.max(reported)), "30-49")
check("fell at every step above that class",   sum(diff(reported[3:6]) < 0), 3)
check("level with each other (1-9 against 10-29, per 100,000)", abs(reported["1-9"] - reported["10-29"]), 2, 2)

sentence("The class with the highest reported injury rate, 30-49 at 330, ranks fourth by count, and the class with the largest count, 10-29 with 552,415, ranks fourth by rate.")
counts <- t1 |> select(bin, reported) |> mutate(rank_by_count = rank(-reported), rank_by_rate = rank(-reported / t1$employee_years_000))
counts
check("30-49 ranks fourth by count",     counts$rank_by_count[counts$bin == "30-49"], 4)
checkeq("the class with the largest count, 10-29", as.character(counts$bin[which.max(counts$reported)]), "10-29")
check("10-29 with 552,415",              counts$reported[counts$bin == "10-29"], 552415)
check("10-29 ranks fourth by rate",      rank(-reported)["10-29"], 4)

sentence("Establishments with fewer than 50 workers accounted for 55.1% of regular employees, 80.6% of deaths and 61.6% of reported injuries.")
below50 <- dat |>
  mutate(small = bin %in% BINS[1:3]) |>
  group_by(small) |>
  summarise(py = sum(py), deaths = sum(death), reported = sum(injury), .groups = "drop") |>
  mutate(across(c(py, deaths, reported), ~ .x / sum(.x) * 100, .names = "{.col}_pct")) |>
  filter(small)
below50
check("55.1% of regular employees",   below50$py_pct,       55.1, 0.05)
check("80.6% of deaths",              below50$deaths_pct,   80.6, 0.05)
check("61.6% of reported injuries",   below50$reported_pct, 61.6, 0.05)

sentence("For deaths, rho between the rank by count and the rank by rate is 0.13 (Table 3).")
sentence("For reported injuries it is -0.48 (Table 4).")
# NOTE: the nine groups ranked by count and by rate; d is the difference between the two rankings
cat("deaths against the fatal injury rate:\n")
rho_shown(bygroup$fatal,    GRP_EN[bygroup$grp], against = bygroup$death)
cat("reported injuries against the reported injury rate:\n")
rho_shown(bygroup$reported, GRP_EN[bygroup$grp], against = bygroup$injury)
countrate
check("0.13 (Table 3)", countrate$deaths,   0.13, 0.005)
check("-0.48 (Table 4)", countrate$reported, -0.48, 0.005)

sentence("Comparing the two end classes, the smaller (1-9) had the higher fatal injury rate in all nine groups (Table 3).")
# NOTE: the larger end is the 300 or more class; a class with no death has rate 0
cellgroup <- ind |>
  group_by(grp, bin) |>
  summarise(death = sum(death), py = sum(py), .groups = "drop")

ends <- cellgroup |> group_by(grp) |> group_modify(function(x, k){
  x     <- inorder(x)
  fr    <- x$death / x$py * 1e5
  tibble(small = fr[1], large = fr[6])
}) |> ungroup() |> mutate(small_is_higher = small > large)
ends
check("the higher fatal injury rate in all nine groups", sum(ends$small_is_higher), 9)

sentence("The highest fatal injury rate was in mining at 43.78, which ranks ninth by the number of deaths with 128.")
sentence("Construction, first by deaths with 5,170, ranks third by rate at 10.47 (Table 3).")
sentence("Mining has the fewest reported injuries of the nine groups, 3,570, and the second highest rate at 1,221 (Table 4).")
ranks <- bygroup |>
  mutate(rank_deaths = rank(-death), rank_fatal = rank(-fatal),
         rank_reported = rank(-injury), rank_reported_rate = rank(-reported)) |>
  select(grp, death, fatal, rank_deaths, rank_fatal, injury, reported, rank_reported, rank_reported_rate)
ranks
mining       <- ranks |> filter(grp == "鉱業")
construction <- ranks |> filter(grp == "建設業")
check("mining at 43.78",                mining$fatal,       43.78, 0.005)
check("highest fatal injury rate",      mining$rank_fatal,  1)
check("ranks ninth by the number of deaths", mining$rank_deaths, 9)
check("with 128",                       mining$death,       128)
check("Construction, first by deaths",  construction$rank_deaths, 1)
check("with 5,170",                     construction$death, 5170)
check("ranks third by rate",            construction$rank_fatal, 3)
check("at 10.47",                       construction$fatal, 10.47, 0.005)
check("the fewest reported injuries of the nine groups", mining$rank_reported, 9)
check("3,570",                          mining$injury,      3570)
check("the second highest rate",        mining$rank_reported_rate, 2)
check("at 1,221",                       mining$reported,    1221, 0.5)

sentence("For the whole country the fatal injury rate fell from 2.25 in 2009 to 1.37 in 2025, a fall of 39% (rho = -0.99).")
byyear |> select(year, death, py, fatal)
rho_shown(byyear$fatal, as.character(byyear$year))
check("from 2.25 in 2009",   byyear$fatal[byyear$year == 2009], 2.25, 0.005)
check("to 1.37 in 2025",     byyear$fatal[byyear$year == 2025], 1.37, 0.005)
check("a fall of 39%",       (1 - byyear$fatal[17] / byyear$fatal[1]) * 100, 39, 0.5)
check("rho = -0.99 (country, fatal, over the years)", rho(byyear$fatal), -0.99, 0.005)

sentence("Within each year rho across the six classes was -0.94 or -1.00.")
sentence("In 7 of the 17 years the six classes did not fall at every step.")
sentence("In each of those years the reversal was between the 10-29 and 30-49 classes.")
yearrho
checkeq("-0.94 or -1.00",                          sort(unique(round(yearrho$fatal, 2))), c(-1, -0.94))
check("In 7 of the 17 years",                      sum(yearrho$fatal > -1), 7)
reversal <- dat |> group_by(year) |> group_modify(function(x, k){
  x  <- inorder(x)
  fr <- x$death / x$py * 1e5
  up <- which(diff(fr) > 0)          # step 2 is the step from 10-29 to 30-49
  tibble(n_up = length(up), where = paste(up, collapse = ","))
}) |> ungroup() |> filter(n_up > 0)
reversal
checkeq("between the 10-29 and 30-49 classes (step 2, in every such year)", unique(reversal$where), "2")

sentence("Over the 17 years every size class fell, with rho between -0.85 and -0.93, except for the 300 or more class, where rho was -0.52.")
classrho
check("rho between -0.85 and -0.93 (1-9 to 100-299)", range(classrho$fatal[1:5]), c(-0.93, -0.85), 0.005)
check("the 300 or more class, where rho was -0.52",   classrho$fatal[6], -0.52, 0.005)

sentence("For the whole country the reported injury rate rose from 238 in 2009 to 265 in 2025, an increase of 11.3% (rho = 0.66).")
# NOTE: taken from the printed endpoints 238 and 265 (unrounded values give 11.0%)
byyear |> select(year, injury, py, reported)
rho_shown(byyear$reported, as.character(byyear$year))
check("from 238 in 2009",  byyear$reported[byyear$year == 2009], 238, 0.5)
check("to 265 in 2025",    byyear$reported[byyear$year == 2025], 265, 0.5)
check("an increase of 11.3%", (round(byyear$reported[17]) / round(byyear$reported[1]) - 1) * 100, 11.3, 0.05)
check("rho = 0.66 (country, reported, over the years)", rho(byyear$reported), 0.66, 0.005)

sentence("Within each year rho across the six classes ranged from -0.09 to -0.83.")
sentence("In all 17 years the 30-49 class was the highest and the 300 or more class the lowest, and the rate fell at every step above 30-49.")
sentence("Below 30-49 the order changed. The 1-9 class was above 10-29 until 2020 and below it from 2021.")
check("ranged from -0.09 to -0.83", range(yearrho$reported), c(-0.83, -0.09), 0.005)
shape <- dat |> group_by(year) |> group_modify(function(x, k){
  x  <- inorder(x)
  ir <- x$injury / x$py * 1e5
  tibble(highest = BINS[which.max(ir)], lowest = BINS[which.min(ir)],
         falls_above_30_49 = all(diff(ir[3:6]) < 0), gap_1_9_minus_10_29 = ir[1] - ir[2])
}) |> ungroup()
shape
check("In all 17 years the 30-49 class was the highest",   sum(shape$highest == "30-49"), 17)
check("and the 300 or more class the lowest",              sum(shape$lowest == "300+"), 17)
check("the rate fell at every step above 30-49",           sum(shape$falls_above_30_49), 17)
checkeq("The 1-9 class was above 10-29 until 2020",        max(shape$year[shape$gap_1_9_minus_10_29 > 0]), 2020)
checkeq("and below it from 2021",                          min(shape$year[shape$gap_1_9_minus_10_29 < 0]), 2021)

sentence("Over the 17 years only the 1-9 class fell, at rho = -0.78, and the other five rose, with rho from 0.46 to 0.97.")
check("only the 1-9 class fell, at rho = -0.78", classrho$reported[1], -0.78, 0.005)
check("the other five rose, with rho from 0.46 to 0.97", range(classrho$reported[2:6]), c(0.46, 0.97), 0.005)

cat("\n[Sensitivity analyses]\n")
sentence("When we substituted the published counts for 2020 and 2021, which include occupational COVID-19, the counts rose by 107 deaths and 25,371 reported injuries.")
# NOTE: the main series excludes COVID-19 in every year; the published counts are the *_pub columns
published <- dat |> mutate(death = death_pub, injury = injury_pub)
added <- tibble(deaths = sum(published$death) - sum(dat$death), reported = sum(published$injury) - sum(dat$injury))
added
check("rose by 107 deaths",             added$deaths,   107)
check("and 25,371 reported injuries",   added$reported, 25371)

sentence("The largest change to any fatal injury rate in Table 2 was in the 100-299 class, from 0.91 to 0.93, and the largest change to any reported injury rate was in the 300 or more class, from 124 to 129.")
withcovid <- tibble(bin = BINS, fatal = fatal, fatal_pub = byclass(published, "death"),
                    reported = reported, reported_pub = byclass(published, "injury")) |>
  mutate(fatal_change = fatal_pub - fatal, reported_change = reported_pub - reported)
withcovid
checkeq("the largest change ... fatal ... the 100-299 class", withcovid$bin[which.max(abs(withcovid$fatal_change))], "100-299")
check("from 0.91 to 0.93", c(withcovid$fatal[5], withcovid$fatal_pub[5]), c(0.91, 0.93), 0.005)
checkeq("the largest change ... reported ... the 300 or more class", withcovid$bin[which.max(abs(withcovid$reported_change))], "300+")
check("from 124 to 129", c(withcovid$reported[6], withcovid$reported_pub[6]), c(124, 129), 0.5)

sentence("For 2020 it moved from -0.54 to -0.26, and for 2021 from -0.26 to -0.09.")
covidrho <- map_dfr(EXYEARS, function(y){
  a <- dat       |> filter(year == y) |> inorder()
  b <- published |> filter(year == y) |> inorder()
  tibble(year = y, main = rho(a$injury / a$py), with_covid = rho(b$injury / b$py))
})
covidrho
check("For 2020 it moved from -0.54 to -0.26", c(covidrho$main[1], covidrho$with_covid[1]), c(-0.54, -0.26), 0.005)
check("for 2021 from -0.26 to -0.09",          c(covidrho$main[2], covidrho$with_covid[2]), c(-0.26, -0.09), 0.005)

sentence("When we pooled only the six benchmark years and used no interpolation, the fatal injury rates across the six classes were 3.57, 2.44, 2.36, 1.28, 0.92 and 0.40.")
benchmark <- dat |> filter(year %in% BENCHMARK)
benchmark |> group_by(bin) |> summarise(deaths = sum(death), py_000 = round(sum(py) / 1e3), .groups = "drop") |> arrange(match(bin, BINS))
fatal_benchmark <- byclass(benchmark, "death")
fatal_benchmark
check("3.57, 2.44, 2.36, 1.28, 0.92 and 0.40", fatal_benchmark, c(3.57, 2.44, 2.36, 1.28, 0.92, 0.40), 0.005)

sentence("When we removed the 36 deaths from the single incident in 2019, the all-industry rate at 50-99 moved from 1.23 to 1.20.")
noincident <- dat |> mutate(death = death - if_else(year == 2019 & bin == "50-99", 36, 0))
fatal_noincident <- byclass(noincident, "death")
class5099 <- dat |> filter(bin == "50-99") |> summarise(deaths = sum(death), py = sum(py))
cat(sprintf("  50-99: %d deaths / %.0f employee-years * 100,000 = %.4f; with 36 fewer deaths %.4f\n",
            class5099$deaths, class5099$py, class5099$deaths / class5099$py * 1e5, (class5099$deaths - 36) / class5099$py * 1e5))
check("at 50-99 moved from 1.23 to 1.20", c(fatal["50-99"], fatal_noincident[4]), c(1.23, 1.20), 0.005)

## DISCUSSION-----
cat("\n== DISCUSSION ==\n")

sentence("Establishments with fewer than 50 workers held 55% of the regular employees but 81% of the deaths.")
below50
check("held 55% of the regular employees", below50$py_pct,     55, 0.5)
check("but 81% of the deaths",             below50$deaths_pct, 81, 0.5)

sentence("The 30-49 class is fourth by count of reported injuries and first by rate, and mining is ninth by deaths and first by fatal injury rate.")
check("The 30-49 class is fourth by count of reported injuries", counts$rank_by_count[counts$bin == "30-49"], 4)
check("and first by rate",                                       rank(-reported)["30-49"], 1)
check("mining is ninth by deaths",                               mining$rank_deaths, 9)
check("and first by fatal injury rate",                          mining$rank_fatal, 1)

sentence("Comparing the pooled rate of 2009 to 2013 with that of 2021 to 2025 (Table 5), the fatal injury rate fell by 34% to 42% in five of the six classes.")
sentence("In the 1-9 class it fell by 23%, the smallest fall.")
sentence("Its first and last years happen to be equal at 0.38, whereas its pooled rate fell from 0.50 to 0.33.")
# NOTE: pooled means sum(deaths)/sum(employee-years) over the five years, as in Table 2
first5 <- dat |> filter(year <= 2013)
last5  <- dat |> filter(year >= 2021)
# NOTE: the inputs: deaths and employee-years summed over each five-year period, per class
pooledinputs <- bind_rows(first5 |> mutate(period = "2009-2013"), last5 |> mutate(period = "2021-2025")) |>
  group_by(period, bin) |>
  summarise(deaths = sum(death), py_000 = round(sum(py) / 1e3), rate = sum(death) / sum(py) * 1e5, .groups = "drop") |>
  arrange(period, match(bin, BINS))
print(as.data.frame(pooledinputs), row.names = FALSE)
pooled <- tibble(bin = BINS, first = byclass(first5, "death"), last = byclass(last5, "death")) |>
  mutate(fall_pct = (1 - last / first) * 100)
pooled
check("fell by 34% to 42% in five of the six classes", range(pooled$fall_pct[2:6]), c(34, 42), 0.6)
check("In the 1-9 class it fell by 23%",                pooled$fall_pct[1], 23, 0.6)
check("the smallest fall",                              which.min(pooled$fall_pct), 1)
top <- dat |> filter(bin == "300+") |> arrange(year) |> mutate(fatal = death / py * 1e5)
check("first and last years happen to be equal at 0.38", c(top$fatal[1], top$fatal[17]), c(0.38, 0.38), 0.005)
check("its pooled rate fell from 0.50 to 0.33",          c(pooled$first[6], pooled$last[6]), c(0.50, 0.33), 0.005)

sentence("We compare five-year periods because a single year of the 300 or more class holds only 21 to 46 deaths.")
range(top$death)
check("only 21 to 46 deaths", range(top$death), c(21, 46))

sentence("The 1-9 class is also the only class in which the reported injury rate fell, from 276 to 243, and that fall changed the shape.")
smallest <- dat |> filter(bin == "1-9") |> arrange(year) |> mutate(reported = injury / py * 1e5)
check("the only class in which the reported injury rate fell", sum(classrho$reported < 0), 1)
check("from 276 to 243", c(smallest$reported[1], smallest$reported[17]), c(276, 243), 0.5)

sentence("Construction accounts for 33% of all deaths and 50% of the deaths in the 1-9 class against 6% of regular employee-years.")
constructionshare <- ind |>
  summarise(construction_deaths = sum(death[grp == "建設業"]), all_deaths_n = sum(death),
            construction_deaths_1_9 = sum(death[grp == "建設業" & bin == "1-9"]), all_deaths_1_9 = sum(death[bin == "1-9"]),
            construction_py_000 = round(sum(py[grp == "建設業"]) / 1e3), all_py_000 = round(sum(py) / 1e3)) |>
  mutate(all_deaths     = construction_deaths / all_deaths_n * 100,
         deaths_1_9     = construction_deaths_1_9 / all_deaths_1_9 * 100,
         employee_years = construction_py_000 / all_py_000 * 100)
print(as.data.frame(constructionshare), row.names = FALSE)
check("33% of all deaths",                   constructionshare$all_deaths,     33, 0.5)
check("50% of the deaths in the 1-9 class",  constructionshare$deaths_1_9,     50, 0.5)
check("against 6% of regular employee-years", constructionshare$employee_years, 6, 0.5)

sentence("Without construction, the fatal injury rate fell from 2.03 at 1-9 to 0.39 at 300 or more, a 5.2-fold difference (rho = -0.94).")
noconstruction <- ind |> filter(grp != "建設業")
fatal_noconstruction <- byclass(noconstruction, "death")
fatal_noconstruction
check("from 2.03 at 1-9",          fatal_noconstruction[1], 2.03, 0.005)
check("to 0.39 at 300 or more",    fatal_noconstruction[6], 0.39, 0.005)
check("a 5.2-fold difference",     fatal_noconstruction[1] / fatal_noconstruction[6], 5.2, 0.05)
check("rho = -0.94",               rho(fatal_noconstruction), -0.94, 0.005)

sentence("Fatal injuries are rare events whose counts vary from year to year, and only 35% of employee-years are direct census counts, ...")
check("only 35% of employee-years are direct census counts", benchshare, 35, 0.5)

## TABLE FOOTNOTES-----
cat("\n== TABLE FOOTNOTES ==\n")

sentence("Six of the 17 years are Economic Census counts (2009, 2012, 2014, 2016, 2021 and 2024) and account for 35% of the employee-years. The other ten years are interpolated log-linearly between them, and 2025 carries the 2024 value forward.", "Table 2, note c")
check("Six of the 17 years",         length(BENCHMARK), 6)
check("The other ten years are interpolated (17 - 6 - 2025)", length(YEARS) - length(BENCHMARK) - 1, 10)
check("account for 35% of the employee-years", benchshare, 35, 0.5)

sentence("The top class is open-ended. Establishments in it averaged 687 regular employees over the six benchmark years.", "Table 2, note e")
# NOTE: the mean size is carried in the panel as m and is used nowhere else
meansize <- dat |> filter(bin == "300+") |> summarise(m = first(m)) |> pull(m)
meansize
check("averaged 687 regular employees", meansize, 687, 0.5)

# Table 5, rows "2009-2013" and "2021-2025": pooled rates, Total and the six classes
pooledtotal <- c(rate(first5, "death"), rate(last5, "death"))
pooledtotal
check("Table 5 row 2009-2013: 2.30 / 4.07 / 2.82 / 2.92 / 1.53 / 1.16 / 0.50", c(pooledtotal[1], pooled$first), c(2.30, 4.07, 2.82, 2.92, 1.53, 1.16, 0.50), 0.005)
check("Table 5 row 2021-2025: 1.47 / 3.13 / 1.75 / 1.77 / 0.89 / 0.70 / 0.33", c(pooledtotal[2], pooled$last),  c(1.47, 3.13, 1.75, 1.77, 0.89, 0.70, 0.33), 0.005)

## Result-----
cat("\n")
if (length(FAILED)) {
  cat("合わなかった項目:\n")
  for (f in FAILED) cat("  -", f, "\n")
  stop(sprintf("%d 件が本文の記述と一致しない", length(FAILED)))
}
cat(sprintf("本文の数値はすべてデータから再現できた（%d 項目）\n", CHECKS))
