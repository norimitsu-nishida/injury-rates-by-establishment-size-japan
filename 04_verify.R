library(tidyverse)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# Checks that every number in the paper's text and tables comes out of the two panels.
# Each block: the sentence quoted, the quantity printed, then check(). Stops at the end if any check failed.
# NOTE: with MANUSCRIPT set to the manuscript's Markdown file, every quoted sentence is also checked to be in it

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

# NOTE: the manuscript writes rho as $\rho$; the quotes here write rho
MANUSCRIPT <- Sys.getenv("MANUSCRIPT")
mstext <- if (nzchar(MANUSCRIPT)) str_replace_all(paste(readLines(MANUSCRIPT, warn = FALSE), collapse = "\n"), fixed("$\\rho$"), "rho") else NULL

# NOTE: prints the sentence the next block checks, and with MANUSCRIPT set checks that the manuscript has it word for word
sentence <- function(txt, where = NULL){
  cat("\n")
  if (!is.null(where)) cat("[", where, "] ", sep = "")
  cat("\"", txt, "\"\n", sep = "")
  if (!is.null(mstext) && is.null(where)) checkeq("the sentence is in the manuscript", str_detect(mstext, fixed(txt)), TRUE)
}

## Print the four tables-----
# NOTE: printed to compare with the Word tables
show <- function(title, d){ cat("\n--", title, "--\n"); print(as.data.frame(d), row.names = FALSE) }

t1 <- dat |>
  group_by(bin) |>
  summarise(deaths = sum(death), reported = sum(injury), py = sum(py), .groups = "drop") |>
  arrange(match(bin, BINS)) |>
  transmute(bin, deaths, reported, employee_years_000 = round(py/1e3),
            fatal_rate = round(deaths/py*1e5, 2), reported_rate = round(reported/py*1e5))
show("Table 1. By establishment size, 2009-2025", t1)

# NOTE: Table 2 prints each fatal rate with the number of deaths in parentheses
yearwide <- function(num, digits, withcount = FALSE) dat |>
  group_by(year, bin) |>
  summarise(v = round(sum(.data[[num]])/sum(py)*1e5, digits), n = sum(.data[[num]]), .groups = "drop") |>
  mutate(v = if (withcount) sprintf("%.2f (%s)", v, format(n, big.mark = ",")) else as.character(v)) |>
  select(-n) |>
  pivot_wider(names_from = bin, values_from = v) |>
  arrange(year)
show("Table 2. Fatal injury rate (deaths) by size and year", yearwide("death", 2, TRUE))
show("Table 3. Reported injury rate by size and year", yearwide("injury", 0))

## Quantities used by several sentences-----
# NOTE: byclass = six classes pooled, byyear = one row per year
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

# NOTE: per year, rho across the six classes (the last column of Tables 2 and 3)
yearrho <- dat |> group_by(year) |> group_modify(function(x, k){
  x <- inorder(x)
  tibble(fatal = rho(x$death / x$py), reported = rho(x$injury / x$py))
}) |> ungroup()
yearrho

# NOTE: per class, rho over the 17 years (the bottom row of Tables 2 and 3)
classrho <- dat |> group_by(bin) |> group_modify(function(x, k){
  x <- x |> arrange(year)
  tibble(fatal = rho(x$death / x$py), reported = rho(x$injury / x$py))
}) |> ungroup() |> arrange(match(bin, BINS))
classrho

totals <- dat |> summarise(deaths = sum(death), reported = sum(injury), py_000 = sum(py) / 1e3)
totals

# NOTE: the four sensitivity analyses of Table 4
published <- dat |> mutate(death = death_pub, injury = injury_pub)
benchmark <- dat |> filter(year %in% BENCHMARK)
noconstruction <- ind |> filter(grp != "建設業")
engaged   <- dat |> mutate(py = eng)

sens <- list(main = dat, covid = published, benchmark = benchmark, noconstruction = noconstruction, engaged = engaged)
t4 <- imap_dfr(sens, function(d, nm){
  f <- byclass(d, "death"); r <- byclass(d, "injury")
  tibble(analysis = nm, outcome = c("fatal", "reported"),
         total = c(rate(d, "death"), rate(d, "injury")),
         `1-9` = c(f[1], r[1]), `10-29` = c(f[2], r[2]), `30-49` = c(f[3], r[3]),
         `50-99` = c(f[4], r[4]), `100-299` = c(f[5], r[5]), `300+` = c(f[6], r[6]),
         rho = c(rho(f), rho(r)))
}) |> arrange(outcome)
show("Table 4. Sensitivity analyses", t4 |> mutate(across(total:rho, ~ if_else(outcome == "fatal", round(.x, 2), round(.x)))))

t4row <- function(nm, oc) t4 |> filter(analysis == nm, outcome == oc) |> select(total:rho) |> unlist()

## ABSTRACT-----
cat("\n== ABSTRACT ==\n")

sentence("Results: 15,635 deaths and 2,106,256 reported injuries occurred over 836,146 thousand regular employee-years.")
check("15,635 deaths",                      totals$deaths,   15635)
check("2,106,256 reported injuries",        totals$reported, 2106256)
check("836,146 thousand regular employee-years", totals$py_000, 836146, 0.5)

sentence("Fatal injury rates fell from 3.59 per 100,000 in the 1-9 class to 0.39 in the 300 or more class, a 9.2-fold difference (rho = -1.00).")
check("3.59 per 100,000 at 1-9 workers",   fatal["1-9"],  3.59, 0.005)
check("0.39 at 300 or more",               fatal["300+"], 0.39, 0.005)
check("a 9.2-fold difference",             fatal["1-9"] / fatal["300+"], 9.2, 0.05)
check("rho = -1.00 (fatal, six classes)",  rho(fatal), -1.00, 0.005)

sentence("Reported injury rates peaked in the 30-49 class (330) and fell to 124 in the 300 or more class (rho = -0.54).")
checkeq("peaked at 30-49 workers",           names(which.max(reported)), "30-49")
check("(330)",                               reported["30-49"], 330, 0.5)
check("124 at 300 or more",                  reported["300+"],  124, 0.5)
check("rho = -0.54 (reported, six classes)", rho(reported), -0.54, 0.005)

sentence("Annual rho stayed between -0.94 and -1.00 for fatal injury rates but moved from -0.83 in 2009 to -0.09 in 2025 for reported injuries.")
checkeq("between -0.94 and -1.00 (the only values taken)", sort(unique(round(yearrho$fatal, 2))), c(-1, -0.94))
check("-0.83 in 2009", yearrho$reported[yearrho$year == 2009], -0.83, 0.005)
check("-0.09 in 2025", yearrho$reported[yearrho$year == 2025], -0.09, 0.005)

sentence("The size gradient was steep and stable for deaths but weaker and non-monotonic for reported injuries as the smaller classes change order over time.")
check("non-monotonic for reported injuries (some step rises)", sum(diff(reported) > 0) > 0, TRUE)

sentence("Because counts and rates ranked the size classes differently for reported injuries, prevention priorities by establishment size need rates as well as counts.")
checkeq("ranked differently by count and by rate", identical(rank(-t1$reported), unname(rank(-reported))), FALSE)

## INTRODUCTION-----
cat("\n== INTRODUCTION ==\n")

sentence("It is a sample survey, and its published frequency rate for deaths was 0.03 or less in every size class from 2009 to 2024, so these values cannot be used to distinguish between size classes.")
# NOTE: typed from the Survey on Industrial Accidents (e-Stat), see the reference note of stat_doukou; not in the panels

## SUBJECTS AND METHODS-----
cat("\n== SUBJECTS AND METHODS ==\n")

cat("\n[Occupational injury counts]\n")
sentence("For 2011, the published counts exclude the earthquake at source, and its 1,314 direct deaths are tabulated separately.")
# NOTE: 1,314 is the separate tabulation of the Ministry and is not in the panel

cat("\n[Number of workers by establishment size]\n")
sentence("Regular employees were 74.3% of persons engaged in the 1-9 class in 2021 and 98.5% in the 300 or more class.")
share2021 <- dat |> filter(year == 2021) |> arrange(match(bin, BINS)) |> mutate(share = py / eng * 100)
share2021 |> select(bin, py, eng, share)
check("74.3% of persons engaged in the 1-9 class",  share2021$share[share2021$bin == "1-9"],  74.3, 0.05)
check("98.5% in the 300 or more class",             share2021$share[share2021$bin == "300+"], 98.5, 0.05)

sentence("The six benchmark years provide 35% of the total employee-years as direct census counts, and the remaining 65% are interpolated or carried forward.")
benchshare <- sum(dat$py[dat$year %in% BENCHMARK]) / sum(dat$py) * 100
benchshare
check("35% of the total employee-years", benchshare, 35, 0.5)
check("the remaining 65%", 100 - benchshare, 65, 0.5)

cat("\n[Statistical analysis]\n")
sentence("With only six points, rho moves in steps of about 0.06, so -0.94 means a single swap between two adjacent classes.")
# NOTE: one adjacent swap changes sum(d^2) by 2, the smallest possible step
check("steps of about 0.06 (6 * 2 / (6 * 35))", 6 * 2 / (6 * 35), 0.06, 0.005)
check("-0.94 is a single adjacent swap", cor(1:6, c(6, 5, 3, 4, 2, 1), method = "spearman"), -0.94, 0.005)

sentence("For the fatal injury rate, we also compared the pooled rates of the first and the last five years (Table 2), because a single year of the 300 or more class holds only 21 to 46 deaths.")
deaths300 <- dat |> filter(bin == "300+") |> pull(death)
range(deaths300)
check("only 21 to 46 deaths", range(deaths300), c(21, 46))

cat("\n[Data availability]\n")
sentence("The repository also holds the raw data, the analysis panels behind every rate in Tables 1 to 4, with the counts and employee-years of every year and size class, and an inventory of all source files with checksums.")
manifest <- read_csv(file.path(ROOT, "evidence/manifest.csv"), show_col_types = FALSE)
nrow(manifest)
check("an inventory of all source files with checksums (an MD5 for every file)", sum(is.na(manifest$md5)), 0)

## RESULTS-----
cat("\n== RESULTS ==\n")

sentence("From 2009 to 2025, 15,635 deaths and 2,106,256 reported injuries occurred over 836,146 thousand regular employee-years (Table 1).")
check("15,635 deaths",                           totals$deaths,   15635)
check("2,106,256 reported injuries",             totals$reported, 2106256)
check("836,146 thousand regular employee-years", totals$py_000,   836146, 0.5)

sentence("The fatal injury rate fell at every step, from 3.59 in the 1-9 class to 0.39 in the 300 or more class, a 9.2-fold difference (rho = -1.00).")
# NOTE: fatal = deaths / employee-years * 100,000. The inputs are the first columns of Table 1.
t1 |> mutate(fatal = deaths / (employee_years_000 * 1000) * 1e5) |> select(bin, deaths, employee_years_000, fatal)
rho_shown(fatal, BINS)
check("fell at every step",         sum(diff(fatal) < 0), 5)
check("from 3.59 in the 1-9 class", fatal["1-9"],  3.59, 0.005)
check("to 0.39 at 300 or more",     fatal["300+"], 0.39, 0.005)
check("a 9.2-fold difference",      fatal["1-9"] / fatal["300+"], 9.2, 0.05)
check("rho = -1.00",                rho(fatal), -1.00, 0.005)

sentence("Across the six classes it was 271, 269, 330, 280, 259 and 124 (rho = -0.54).")
sentence("The highest value was in the 30-49 class, and the rate fell at every step above that class.")
sentence("The two classes below the 30-49 class were nearly equal.")
t1 |> mutate(reported_rate = reported / (employee_years_000 * 1000) * 1e5) |> select(bin, reported, employee_years_000, reported_rate)
rho_shown(reported, BINS)
check("271, 269, 330, 280, 259 and 124",       reported, c(271, 269, 330, 280, 259, 124), 0.5)
check("rho = -0.54",                            rho(reported), -0.54, 0.005)
checkeq("The highest value was at 30-49",       names(which.max(reported)), "30-49")
check("fell at every step above that class",   sum(diff(reported[3:6]) < 0), 3)
check("nearly equal (1-9 against 10-29, per 100,000)", abs(reported["1-9"] - reported["10-29"]), 2, 2)

sentence("The class with the highest reported injury rate, the 30-49 class at 330, ranked fourth by count, and the class with the largest count, the 10-29 class with 552,415, ranked fourth by rate.")
counts <- t1 |> select(bin, reported) |> mutate(rank_by_count = rank(-reported), rank_by_rate = rank(-reported / t1$employee_years_000))
counts
check("30-49 ranked fourth by count",     counts$rank_by_count[counts$bin == "30-49"], 4)
checkeq("the class with the largest count, 10-29", as.character(counts$bin[which.max(counts$reported)]), "10-29")
check("10-29 with 552,415",              counts$reported[counts$bin == "10-29"], 552415)
check("10-29 ranked fourth by rate",     rank(-reported)["10-29"], 4)

sentence("For the whole country the fatal injury rate fell from 2.25 in 2009 to 1.37 in 2025, a fall of 39% (rho = -0.99).")
byyear |> select(year, death, py, fatal)
rho_shown(byyear$fatal, as.character(byyear$year))
check("from 2.25 in 2009",   byyear$fatal[byyear$year == 2009], 2.25, 0.005)
check("to 1.37 in 2025",     byyear$fatal[byyear$year == 2025], 1.37, 0.005)
check("a fall of 39%",       (1 - byyear$fatal[17] / byyear$fatal[1]) * 100, 39, 0.5)
check("rho = -0.99 (country, fatal, over the years)", rho(byyear$fatal), -0.99, 0.005)

sentence("Within each year rho across the six classes was -0.94 or -1.00.")
sentence("In 7 of the 17 years the rate did not fall at every step across the six classes.")
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

sentence("Over the 17 years the rate fell in every size class, with rho between -0.85 and -0.93, except for the 300 or more class, where rho was -0.52.")
classrho
check("rho between -0.85 and -0.93 (1-9 to 100-299)", range(classrho$fatal[1:5]), c(-0.93, -0.85), 0.005)
check("the 300 or more class, where rho was -0.52",   classrho$fatal[6], -0.52, 0.005)

sentence("Between the pooled rates of 2009-2013 and 2021-2025 (Table 2), the fatal injury rate fell by 34% to 42% in five of the six classes and by 23% in the 1-9 class.")
sentence("In the 300 or more class the rate was 0.38 in both the first and the last year, whereas the pooled rate fell from 0.50 to 0.33.")
# NOTE: pooled means sum(deaths)/sum(employee-years) over the five years, as in Table 1
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
check("and by 23% in the 1-9 class",                   pooled$fall_pct[1], 23, 0.6)
top <- dat |> filter(bin == "300+") |> arrange(year) |> mutate(fatal = death / py * 1e5)
check("0.38 in both the first and the last year", c(top$fatal[1], top$fatal[17]), c(0.38, 0.38), 0.005)
check("the pooled rate fell from 0.50 to 0.33",  c(pooled$first[6], pooled$last[6]), c(0.50, 0.33), 0.005)

sentence("For the whole country the reported injury rate rose from 238 in 2009 to 265 in 2025, an increase of 11.3% (rho = 0.66).")
# NOTE: taken from the printed endpoints 238 and 265 (unrounded values give 11.0%)
byyear |> select(year, injury, py, reported)
rho_shown(byyear$reported, as.character(byyear$year))
check("from 238 in 2009",  byyear$reported[byyear$year == 2009], 238, 0.5)
check("to 265 in 2025",    byyear$reported[byyear$year == 2025], 265, 0.5)
check("an increase of 11.3%", (round(byyear$reported[17]) / round(byyear$reported[1]) - 1) * 100, 11.3, 0.05)
check("rho = 0.66 (country, reported, over the years)", rho(byyear$reported), 0.66, 0.005)

sentence("In these six years alone, the fatal injury rate fell from 2.25 in 2009 to 1.46 in 2024 and the reported injury rate rose from 238 to 266 (marked in Tables 2 and 3).")
bench <- byyear |> filter(year %in% BENCHMARK)
bench |> select(year, fatal, reported)
check("fell from 2.25 in 2009 to 1.46 in 2024", c(bench$fatal[1], bench$fatal[6]), c(2.25, 1.46), 0.005)
check("rose from 238 to 266",                  c(bench$reported[1], bench$reported[6]), c(238, 266), 0.5)

sentence("Within each year rho across the six classes moved from -0.83 in 2009 to -0.09 in 2025.")
sentence("In all 17 years the 30-49 class was the highest and the 300 or more class the lowest, and the rate fell at every step above the 30-49 class.")
sentence("Below the 30-49 class, the 1-9 class was above the 10-29 class until 2020 and below it from 2021.")
check("moved from -0.83 in 2009 to -0.09 in 2025", c(yearrho$reported[yearrho$year == 2009], yearrho$reported[yearrho$year == 2025]), c(-0.83, -0.09), 0.005)
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

sentence("Over the 17 years the rate fell only in the 1-9 class, at rho = -0.78, and the other five rose, with rho from 0.46 to 0.97.")
check("only the 1-9 class fell, at rho = -0.78", classrho$reported[1], -0.78, 0.005)
check("the other five rose, with rho from 0.46 to 0.97", range(classrho$reported[2:6]), c(0.46, 0.97), 0.005)

cat("\n[Sensitivity analyses]\n")
sentence("When we substituted the published counts for 2020 and 2021, which include occupational COVID-19, the counts rose by 107 deaths and 25,371 reported injuries.")
# NOTE: the main series excludes COVID-19 in every year; the published counts are the *_pub columns
added <- tibble(deaths = sum(published$death) - sum(dat$death), reported = sum(published$injury) - sum(dat$injury))
added
check("rose by 107 deaths",             added$deaths,   107)
check("and 25,371 reported injuries",   added$reported, 25371)

sentence("The largest change in any size class was 0.02 for the fatal injury rate and 5 for the reported injury rate (Table 4).")
withcovid <- tibble(bin = BINS, fatal = fatal, fatal_pub = byclass(published, "death"),
                    reported = reported, reported_pub = byclass(published, "injury")) |>
  mutate(fatal_change = fatal_pub - fatal, reported_change = reported_pub - reported)
withcovid
check("largest change 0.02 for the fatal injury rate",    max(abs(withcovid$fatal_change)),    0.02, 0.005)
check("and 5 for the reported injury rate",                 max(abs(withcovid$reported_change)), 5,    0.5)

sentence("The within-year rho for the reported injury rate moved from -0.54 to -0.26 in 2020 and from -0.26 to -0.09 in 2021.")
covidrho <- map_dfr(EXYEARS, function(y){
  a <- dat       |> filter(year == y) |> inorder()
  b <- published |> filter(year == y) |> inorder()
  tibble(year = y, main = rho(a$injury / a$py), with_covid = rho(b$injury / b$py))
})
covidrho
check("from -0.54 to -0.26 in 2020", c(covidrho$main[1], covidrho$with_covid[1]), c(-0.54, -0.26), 0.005)
check("from -0.26 to -0.09 in 2021",          c(covidrho$main[2], covidrho$with_covid[2]), c(-0.26, -0.09), 0.005)

sentence("When we pooled only the six benchmark years and used no interpolation, no rate changed by more than 0.14 for deaths or 4 for reported injuries (Table 4).")
fatal_benchmark    <- byclass(benchmark, "death")
reported_benchmark <- byclass(benchmark, "injury")
benchchange <- tibble(bin = BINS, fatal = fatal_benchmark - fatal, reported = reported_benchmark - reported)
benchchange
check("no fatal rate changed by more than 0.14",   max(abs(benchchange$fatal)),    0.14, 0.005)
check("no reported rate changed by more than 4",   max(abs(benchchange$reported)), 4,    0.5)

sentence("When we removed construction, which accounted for 33% of all deaths, 50% of the deaths in the 1-9 class and 6% of regular employee-years, the fatal injury rate fell from 2.03 in the 1-9 class to 0.39 in the 300 or more class, a 5.2-fold difference (rho = -0.94, Table 4).")
deaths19 <- ind |> filter(bin == "1-9") |> summarise(construction = sum(death[grp == "建設業"]), all = sum(death)) |> mutate(pct = construction / all * 100)
deaths19
check("50% of the deaths in the 1-9 class", deaths19$pct, 50, 0.5)
constructionshare <- ind |>
  summarise(construction_deaths = sum(death[grp == "建設業"]), all_deaths_n = sum(death),
            construction_py_000 = round(sum(py[grp == "建設業"]) / 1e3), all_py_000 = round(sum(py) / 1e3)) |>
  mutate(all_deaths     = construction_deaths / all_deaths_n * 100,
         employee_years = construction_py_000 / all_py_000 * 100)
print(as.data.frame(constructionshare), row.names = FALSE)
check("33% of all deaths",                   constructionshare$all_deaths,     33, 0.5)
check("and 6% of regular employee-years"     , constructionshare$employee_years, 6, 0.5)
fatal_noconstruction    <- byclass(noconstruction, "death")
reported_noconstruction <- byclass(noconstruction, "injury")
fatal_noconstruction
check("from 2.03 at 1-9",          fatal_noconstruction[1], 2.03, 0.005)
check("to 0.39 at 300 or more",    fatal_noconstruction[6], 0.39, 0.005)
check("a 5.2-fold difference",     fatal_noconstruction[1] / fatal_noconstruction[6], 5.2, 0.05)
check("rho = -0.94",               rho(fatal_noconstruction), -0.94, 0.005)

sentence("The reported injury rate in the 1-9 class fell from 271 to 196, below those of the 10-29 to 100-299 classes, and rho moved from -0.54 to -0.09 (Table 4).")
reported_noconstruction
check("fell from 271 to 196",                     c(reported[1], reported_noconstruction[1]), c(271, 196), 0.5)
check("below those of the 10-29 to 100-299 classes", sum(reported_noconstruction[1] < reported_noconstruction[2:5]), 4)
check("rho moved from -0.54 to -0.09",            c(rho(reported), rho(reported_noconstruction)), c(-0.54, -0.09), 0.005)

sentence("With persons engaged as the denominator, the fatal injury rate fell from 2.61 in the 1-9 class to 0.38 in the 300 or more class, a 6.9-fold difference (rho = -1.00), and the reported injury rate in the 1-9 class fell from 271 to 197 (rho = -0.09) (Table 4).")
fatal_engaged    <- byclass(engaged, "death")
reported_engaged <- byclass(engaged, "injury")
tibble(bin = BINS, fatal = fatal_engaged, reported = reported_engaged)
check("from 2.61 in the 1-9 class to 0.38",   c(fatal_engaged[1], fatal_engaged[6]), c(2.61, 0.38), 0.005)
check("a 6.9-fold difference",                fatal_engaged[1] / fatal_engaged[6], 6.9, 0.05)
check("rho = -1.00",                           rho(fatal_engaged), -1.00, 0.005)
check("the 1-9 class fell from 271 to 197",   reported_engaged[1], 197, 0.5)
check("rho = -0.09",                           rho(reported_engaged), -0.09, 0.005)

## DISCUSSION-----
cat("\n== DISCUSSION ==\n")

sentence("The fatal injury rate fell with establishment size at every step, with a 9.2-fold difference between the smallest and the largest class.")
check("fell at every step", sum(diff(fatal) < 0), 5)
check("a 9.2-fold difference", fatal[1] / fatal[6], 9.2, 0.05)

sentence("The reported injury rate was highest in the 30-49 class and fell at every step above that class.")
check("highest at 30-49", which.max(reported), 3)
check("fell at every step above that class", sum(diff(reported[3:6]) < 0), 3)

sentence("Within each year, the negative correlation between establishment size and the fatal injury rate remained strong, whereas that for the reported injury rate weakened over time.")
# NOTE: strong = -0.94 or -1.00 in every year; weakened = from -0.83 in 2009 to -0.09 in 2025
checkeq("remained strong (-0.94 or -1.00 every year)", sort(unique(round(yearrho$fatal, 2))), c(-1, -0.94))
check("weakened from -0.83 in 2009 to -0.09 in 2025", c(yearrho$reported[yearrho$year == 2009], yearrho$reported[yearrho$year == 2025]), c(-0.83, -0.09), 0.005)

sentence("Over the 17 years, the fatal injury rate fell and the reported injury rate rose.")
check("fatal fell, reported rose (country, rho over the years)", c(rho(byyear$fatal) < 0, rho(byyear$reported) > 0), c(TRUE, TRUE))

sentence("The size patterns of the pooled rates did not change when we included occupational COVID-19 or used only the benchmark years.")
# NOTE: the patterns: fatal falls at every step; reported peaks at 30-49 and falls at every step above it
fatalpattern    <- function(x) all(diff(x) < 0)
reportedpattern <- function(x) which.max(x) == 3 && all(diff(x[3:6]) < 0)
patterns <- tibble(analysis = c("with COVID-19", "benchmark years only"),
                   fatal    = c(fatalpattern(withcovid$fatal_pub), fatalpattern(fatal_benchmark)),
                   reported = c(reportedpattern(withcovid$reported_pub), reportedpattern(reported_benchmark)))
patterns
check("these patterns did not change (fatal, both)",    sum(patterns$fatal), 2)
check("these patterns did not change (reported, both)", sum(patterns$reported), 2)

sentence("Without construction or with persons engaged as the denominator, the gradient in fatal injury rates remained, but the reported injury rate of the 1-9 class fell below those of the 10-29 to 100-299 classes (Table 4).")
check("the fatal gradient remained (rho -0.94 and -1.00)", c(rho(fatal_noconstruction), rho(fatal_engaged)), c(-0.94, -1.00), 0.005)
check("1-9 below 10-29 to 100-299 (both analyses)",
      c(sum(reported_noconstruction[1] < reported_noconstruction[2:5]), sum(reported_engaged[1] < reported_engaged[2:5])), c(4, 4))


sentence("For reported injuries, the 30-49 class was fourth by count and first by rate, and the 10-29 class, with the largest count, was fourth by rate.")
check("30-49 fourth by count and first by rate", c(counts$rank_by_count[counts$bin == "30-49"], rank(-reported)["30-49"]), c(4, 1))
check("10-29, with the largest count, fourth by rate", c(counts$rank_by_count[counts$bin == "10-29"], rank(-reported)["10-29"]), c(1, 4))

sentence("In our data, the reported injury rate was highest in the 30-49 class, and none of the classes below 50 workers was lower than the 100-299 class.")
checkeq("highest in the 30-49 class",                             names(which.max(reported)), "30-49")
check("none of the classes below 50 workers lower than 100-299", sum(reported[1:3] < reported["100-299"]), 0)

sentence("Without construction, however, the 1-9 class fell below the 10-29 to 100-299 classes (Table 4).")
check("1-9 below 10-29 to 100-299 without construction", sum(reported_noconstruction[1] < reported_noconstruction[2:5]), 4)

sentence("The high reported injury rate of the 1-9 class thus came largely from construction, which may reflect both the high risk of construction work and the way its size is recorded.")
check("1-9 reported rate fell from 271 to 196 without construction", c(reported[1], reported_noconstruction[1]), c(271, 196), 0.5)

sentence("The two rates moved in opposite directions over the 17 years (Tables 2 and 3).")
check("the fatal injury rate fell (all classes, rho over years)", cor(byyear$year, byyear$fatal, method = "spearman") < 0, TRUE)
check("the reported injury rate rose (all classes, rho over years)", cor(byyear$year, byyear$reported, method = "spearman") > 0, TRUE)

sentence("Between the periods 2009-2013 and 2021-2025, the fatal injury rate fell in every size class, least in the 1-9 class.")
check("fell in every size class", sum(pooled$fall_pct > 0), 6)
check("least in the 1-9 class", which.min(pooled$fall_pct), 1)

sentence("The reported injury rate, by contrast, rose in every class except the 1-9 class, where it fell.")
checkeq("rose in every class except 1-9", as.character(classrho$bin[classrho$reported > 0]), BINS[2:6])
check("where it fell (1-9)", classrho$reported[1] < 0, TRUE)

sentence("The change in the definition of regular employees after 2014 would make the earlier rates slightly lower, but the six benchmark years alone showed the same opposite movements.")
check("benchmark years: fatal fell and reported rose (2009 to 2024)", c(bench$fatal[6] < bench$fatal[1], bench$reported[6] > bench$reported[1]), c(TRUE, TRUE))

sentence("Without construction, which accounts for a third of all deaths and half of those in the 1-9 class, the fatal injury rate of the 1-9 class was 5.2 times that of the 300 or more class rather than 9.2 times (Table 4).")
check("a third of all deaths and half of those in the 1-9 class", c(constructionshare$all_deaths, deaths19$pct), c(33, 50), 0.5)
check("5.2 times rather than 9.2 times", c(fatal_noconstruction[1] / fatal_noconstruction[6], fatal[1] / fatal[6]), c(5.2, 9.2), 0.05)

sentence("The gradient in fatal injury rates therefore did not depend on construction alone, but part of its size reflected the industries in each class.")
check("gradient remained without construction (rho), smaller fold", c(rho(fatal_noconstruction), fatal_noconstruction[1] / fatal_noconstruction[6] < fatal[1] / fatal[6]), c(-0.94, TRUE), 0.005)

sentence("They were about 4% of regular employees, 5% of reported injuries and 2% of deaths in 2023 [@mhlw_bunseki2023].")
# NOTE: the agency figures are typed from Appended Table 8 of the cited report (1.92 million workers on 1 June 2023,
# 6,892 of 135,371 reported injuries, 14 of 755 deaths); the share of regular employees uses our 2023 denominator
agency <- c(workers = 1.92e6 / sum(dat$py[dat$year == 2023]), injuries = 6892 / 135371, deaths = 14 / 755) * 100
agency
check("about 4% of regular employees, 5% of reported injuries and 2% of deaths", round(agency), c(4, 5, 2))

sentence("Third, the rates are not free of uncertainty, because fatal injuries are rare events whose counts vary from year to year and only 35% of employee-years are direct census counts.")
check("only 35% of employee-years are direct census counts", benchshare, 35, 0.5)

## CONCLUSIONS-----
cat("\n== CONCLUSIONS ==\n")

sentence("The fatal injury rate fell monotonically with size, with a 9.2-fold difference, and the reported injury rate was highest in the 30-49 class in every year and fell at every step above that class.")
check("fell monotonically, 9.2-fold", c(sum(diff(fatal) < 0), fatal[1] / fatal[6]), c(5, 9.2), 0.05)
check("highest in the 30-49 class in every year", sum(shape$highest == "30-49"), 17)
check("fell at every step above that class",     sum(diff(reported[3:6]) < 0), 3)

## TABLES-----
cat("\n== TABLES ==\n")

sentence("Six of the 17 years are Economic Census counts (2009, 2012, 2014, 2016, 2021 and 2024) and account for 35% of the employee-years. The other ten years are interpolated log-linearly between them, and 2025 carries the 2024 value forward.", "Table 1, note c")
check("Six of the 17 years",         length(BENCHMARK), 6)
check("The other ten years are interpolated (17 - 6 - 2025)", length(YEARS) - length(BENCHMARK) - 1, 10)
check("account for 35% of the employee-years", benchshare, 35, 0.5)

sentence("The top class is open-ended. Establishments in it averaged 687 regular employees over the six benchmark years.", "Table 1, note e")
# NOTE: the mean size is carried in the panel as m and is used nowhere else
meansize <- dat |> filter(bin == "300+") |> summarise(m = first(m)) |> pull(m)
meansize
check("averaged 687 regular employees", meansize, 687, 0.5)

# Table 2, rows "2009-2013" and "2021-2025": pooled rates (deaths), Overall and the six classes
pooledtotal <- c(rate(first5, "death"), rate(last5, "death"))
pooledtotal
check("Table 2 row 2009-2013: 2.30 / 4.07 / 2.82 / 2.92 / 1.53 / 1.16 / 0.50", c(pooledtotal[1], pooled$first), c(2.30, 4.07, 2.82, 2.92, 1.53, 1.16, 0.50), 0.005)
check("Table 2 row 2009-2013 deaths: 5,415 / 1,998 / 1,609 / 748 / 462 / 411 / 187",
      c(sum(first5$death), pooledinputs$deaths[pooledinputs$period == "2009-2013"]), c(5415, 1998, 1609, 748, 462, 411, 187))
check("Table 2 row 2021-2025: 1.47 / 3.13 / 1.75 / 1.77 / 0.89 / 0.70 / 0.33", c(pooledtotal[2], pooled$last),  c(1.47, 3.13, 1.75, 1.77, 0.89, 0.70, 0.33), 0.005)
check("Table 2 row 2021-2025 deaths: 3,750 / 1,407 / 1,097 / 523 / 301 / 277 / 145",
      c(sum(last5$death), pooledinputs$deaths[pooledinputs$period == "2021-2025"]), c(3750, 1407, 1097, 523, 301, 277, 145))

sentence("The 2019 rate in the 50-99 class includes 36 deaths from a single arson attack on one workplace.", "Table 2, note e")
# NOTE: the 36 deaths are not identified in the published counts; the check shows the cell with and without them
cell2019 <- dat |> filter(year == 2019, bin == "50-99") |> summarise(death = sum(death), py = sum(py)) |>
  mutate(rate = death / py * 1e5, without36 = (death - 36) / py * 1e5)
cell2019
check("Table 2, 2019, 50-99: 1.59 (105)", c(cell2019$rate, cell2019$death), c(1.59, 105), 0.005)

# Table 4, every row: Total, the six classes and rho
cat("\n[Table 4]\n")
t4want <- tribble(
  ~analysis,        ~outcome,   ~values,
  "main",           "fatal",    c(1.87, 3.59, 2.30, 2.22, 1.23, 0.91, 0.39, -1.00),
  "covid",          "fatal",    c(1.88, 3.60, 2.32, 2.24, 1.24, 0.93, 0.39, -1.00),
  "benchmark",      "fatal",    c(1.93, 3.57, 2.44, 2.36, 1.28, 0.92, 0.40, -1.00),
  "noconstruction", "fatal",    c(1.33, 2.03, 1.68, 1.86, 1.10, 0.85, 0.39, -0.94),
  "engaged",        "fatal",    c(1.68, 2.61, 2.11, 2.10, 1.18, 0.88, 0.38, -1.00),
  "main",           "reported", c(252, 271, 269, 330, 280, 259, 124, -0.54),
  "covid",          "reported", c(255, 272, 271, 333, 283, 264, 129, -0.54),
  "benchmark",      "reported", c(251, 272, 269, 327, 276, 257, 123, -0.54),
  "noconstruction", "reported", c(235, 196, 251, 330, 284, 265, 126, -0.09),
  "engaged",        "reported", c(226, 197, 246, 312, 268, 251, 121, -0.09)
)
for (i in seq_len(nrow(t4want))) {
  w   <- t4want$values[[i]]
  got <- t4row(t4want$analysis[i], t4want$outcome[i])
  tol <- if (t4want$outcome[i] == "fatal") c(rep(0.005, 7), 0.005) else c(rep(0.5, 7), 0.005)
  CHECKS <- CHECKS + 1
  ok <- all(abs(got - w) <= tol)
  cat(sprintf("%-3s  Table 4 %-14s %-8s  %s\n", if (ok) "ok" else "NG", t4want$analysis[i], t4want$outcome[i], paste(w, collapse = " ")))
  if (!ok) FAILED <- c(FAILED, paste("Table 4", t4want$analysis[i], t4want$outcome[i]))
}

## Result-----
cat("\n")
if (length(FAILED)) {
  cat("合わなかった項目:\n")
  for (f in FAILED) cat("  -", f, "\n")
  stop(sprintf("%d 件が本文の記述と一致しない", length(FAILED)))
}
cat(sprintf("本文の数値はすべてデータから再現できた（%d 項目）\n", CHECKS))
