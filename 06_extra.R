library(tidyverse)

source(file.path(Sys.getenv("V2_HOME", unset = "."), "00_setup.R"))

# NOTE: analyses run for reviewer questions, not reported in the paper. Nothing is written to disk.

dat <- read_rds(file.path(OUT, "panel_all.rds"))      |> mutate(bin = factor(as.character(bin), BINS))
ind <- read_rds(file.path(OUT, "panel_industry.rds")) |> mutate(bin = factor(as.character(bin), BINS))

rho <- function(x){
  k <- which(!is.na(x))
  cor(k, x[k], method = "spearman")
}

## 1. Poisson 95% intervals for the Table 2 rates, and rate ratios against 300 or more-----
# NOTE: the intervals are narrow and exclude the uncertainty of the interpolated denominator
byclass <- dat |>
  group_by(bin) |>
  summarise(deaths = sum(death), reported = sum(injury), py = sum(py), .groups = "drop") |>
  arrange(bin)

ref <- byclass |> filter(bin == "300+")

ci1 <- byclass |>
  mutate(
    fatal_rate  = deaths / py * 1e5,
    fatal_lo    = qchisq(0.025, 2 * deaths) / 2 / py * 1e5,         # exact Poisson limits
    fatal_hi    = qchisq(0.975, 2 * (deaths + 1)) / 2 / py * 1e5,
    fatal_rr    = fatal_rate / (ref$deaths / ref$py * 1e5),
    fatal_rr_se = sqrt(1 / deaths + 1 / ref$deaths),
    fatal_rr_lo = exp(log(fatal_rr) - 1.96 * fatal_rr_se),
    fatal_rr_hi = exp(log(fatal_rr) + 1.96 * fatal_rr_se),
    rep_rate    = reported / py * 1e5,
    rep_rr      = rep_rate / (ref$reported / ref$py * 1e5),
    rep_rr_se   = sqrt(1 / reported + 1 / ref$reported),
    rep_rr_lo   = exp(log(rep_rr) - 1.96 * rep_rr_se),
    rep_rr_hi   = exp(log(rep_rr) + 1.96 * rep_rr_se)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  select(bin, fatal_rate, fatal_lo, fatal_hi, fatal_rr, fatal_rr_lo, fatal_rr_hi,
         rep_rate, rep_rr, rep_rr_lo, rep_rr_hi)

ci1 |> print(width = 200)
# FOOTNOTE: fatal 1-9 3.59 (3.50-3.68), rate ratio against 300+ 9.25 (8.46-10.1); reported 1-9 rate ratio 2.18 (2.17-2.19)

## 2. Direct standardisation by industry, and a Mantel-Haenszel rate ratio-----
# NOTE: is the all-industry gradient the industry mix of the size classes?
cellind <- ind |>
  group_by(grp, bin) |>
  summarise(death = sum(death), injury = sum(injury), py = sum(py), .groups = "drop")

# NOTE: standard population = all-industry employee-years over the nine groups
weights <- ind |>
  group_by(grp) |>
  summarise(py = sum(py), .groups = "drop") |>
  mutate(w = py / sum(py)) |>
  select(grp, w)

std <- cellind |>
  inner_join(weights, by = "grp") |>
  mutate(fatal = death / py * 1e5, reported = injury / py * 1e5) |>
  group_by(bin) |>
  summarise(
    crude_fatal    = sum(death)  / sum(py) * 1e5,
    std_fatal      = sum(fatal * w),
    crude_reported = sum(injury) / sum(py) * 1e5,
    std_reported   = sum(reported * w),
    .groups = "drop"
  ) |>
  arrange(bin) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

std
cat("1-9 over 300+: crude", round(std$crude_fatal[1] / std$crude_fatal[6], 2),
    " standardised", round(std$std_fatal[1] / std$std_fatal[6], 2), "\n")
# FOOTNOTE: standardised fatal rates 3.30 / 2.34 / 2.16 / 1.21 / 0.88 / 0.31; ratio 10.65 against 9.23 crude

# NOTE: Mantel-Haenszel rate ratio, fewer than 50 against 50 or more, stratified by the nine groups
mh <- cellind |>
  mutate(small = bin %in% BINS[1:3]) |>
  group_by(grp, small) |>
  summarise(death = sum(death), injury = sum(injury), py = sum(py), .groups = "drop") |>
  pivot_wider(names_from = small, values_from = c(death, injury, py),
              names_glue = "{.value}_{ifelse(small, 'lt50', 'ge50')}") |>
  mutate(T = py_lt50 + py_ge50)

mh_fatal    <- sum(mh$death_lt50  * mh$py_ge50 / mh$T) / sum(mh$death_ge50  * mh$py_lt50 / mh$T)
mh_reported <- sum(mh$injury_lt50 * mh$py_ge50 / mh$T) / sum(mh$injury_ge50 * mh$py_lt50 / mh$T)
crude_fatal    <- (sum(mh$death_lt50)  / sum(mh$py_lt50)) / (sum(mh$death_ge50)  / sum(mh$py_ge50))
crude_reported <- (sum(mh$injury_lt50) / sum(mh$py_lt50)) / (sum(mh$injury_ge50) / sum(mh$py_ge50))

cat(sprintf("below 50 against 50 or more: fatal MH %.2f (crude %.2f), reported MH %.2f (crude %.2f)\n",
            mh_fatal, crude_fatal, mh_reported, crude_reported))
# FOOTNOTE: fatal 3.14 (crude 3.38); reported 1.32 (crude 1.31)

## 3. The all-industry gradient without construction-----
noconstruction <- ind |>
  filter(grp != "建設業") |>
  group_by(bin) |>
  summarise(fatal = sum(death) / sum(py) * 1e5, reported = sum(injury) / sum(py) * 1e5, .groups = "drop") |>
  arrange(bin) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

noconstruction
cat("rho without construction: fatal", round(rho(noconstruction$fatal), 2),
    " reported", round(rho(noconstruction$reported), 2), "\n")
# FOOTNOTE: fatal 2.03 / 1.68 / 1.86 / 1.10 / 0.85 / 0.39, rho -0.94; reported 196 / 251 / 330 / 284 / 265 / 126, rho -0.09

## 4. Deaths per 1,000 reported injuries by size class-----
# NOTE: not used in the paper; the ratio mixes severity and reporting
perthousand <- byclass |>
  mutate(deaths_per_1000_reported = round(deaths / reported * 1000, 2),
         reported_per_death       = round(reported / deaths, 1)) |>
  select(bin, deaths, reported, deaths_per_1000_reported, reported_per_death)

perthousand |> print()
# FOOTNOTE: 13.3 / 8.6 / 6.7 / 4.4 / 3.5 / 3.1 deaths per 1,000 reported injuries, falling at every step

## 5. Year-to-year variation against Poisson variation-----
# NOTE: Pearson chi-square / df of a Poisson trend model per class; above 1 means more than Poisson variation
dispersion <- function(d, num){
  m <- glm(d[[num]] ~ d$year + offset(log(d$py)), family = poisson)
  sum(residuals(m, type = "pearson")^2) / df.residual(m)
}

disp <- map_dfr(BINS, function(b){
  d <- dat |> filter(bin == b) |> arrange(year)
  tibble(bin = b, deaths = round(dispersion(d, "death"), 2), reported = round(dispersion(d, "injury"), 1))
})

national <- dat |>
  group_by(year) |>
  summarise(death = sum(death), injury = sum(injury), py = sum(py), .groups = "drop")
disp <- bind_rows(disp, tibble(bin = "Total", deaths = round(dispersion(national, "death"), 2),
                               reported = round(dispersion(national, "injury"), 1)))

disp
# FOOTNOTE: reported injuries 14 to 47 (country 98); deaths 0.7 to 1.9 (country 2.3)

## 6. The reported injury rate of the 1-9 class against the 10-29 class, by year-----
# NOTE: is the change of order between 1-9 and 10-29 (Table 6) more than noise?
pair <- dat |>
  filter(bin %in% c("1-9", "10-29")) |>
  mutate(rate = injury / py * 1e5, se = sqrt(injury) / py * 1e5) |>
  select(year, bin, injury, rate, se) |>
  pivot_wider(names_from = bin, values_from = c(injury, rate, se))

crossing <- pair |>
  mutate(
    diff    = `rate_1-9` - `rate_10-29`,
    z       = diff / sqrt(`se_1-9`^2 + `se_10-29`^2),
    ratio   = `rate_1-9` / `rate_10-29`,
    ratio_se = sqrt(1 / `injury_1-9` + 1 / `injury_10-29`),
    ratio_lo = exp(log(ratio) - 1.96 * ratio_se),
    ratio_hi = exp(log(ratio) + 1.96 * ratio_se)
  ) |>
  transmute(year, `rate_1-9` = round(`rate_1-9`, 1), `rate_10-29` = round(`rate_10-29`, 1),
            diff = round(diff, 1), z = round(z, 1),
            ratio = sprintf("%.3f (%.3f-%.3f)", ratio, ratio_lo, ratio_hi))

crossing |> print(n = 17)
# FOOTNOTE: 2019 +8.3 (z 3.7), 2020 +0.7 (z 0.3), 2021 -7.5 (z -3.3), 2025 -38.0 (z -17.1)
