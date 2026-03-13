# Estimate private K–12 enrollment shares (2011 vs. 2021) for US and NC
# Private denominators: PSS microdata (excludes PreK, uses final survey weight)
# Public denominators: NCES Digest Table 203.40 (public total minus public PreK)
#   Fall 2011 — https://nces.ed.gov/programs/digest/d13/tables/dt13_203.40.asp
#   Fall 2021 — https://nces.ed.gov/programs/digest/d22/tables/dt22_203.40.asp

library(readr)
library(dplyr)

# --- 1. Load PSS microdata ----
# 2011–12: tab-delimited; 2021–22: CSV
pss_1112 <- read_tsv("enrollments/pss1112_pu.txt", show_col_types = FALSE)
pss_2122 <- read_csv("enrollments/pss2122_pu.csv", show_col_types = FALSE)

# --- 2. Weighted private K–12 totals ----
# NUMSTUDS = K–12 enrollment (excludes PreK); PFNLWT = final survey weight
# Compute weighted totals for US and NC separately, then bind into one tibble

priv_k12_2011 <- pss_1112 |>
  transmute(state = pstabb, k12 = numstuds, w = pfnlwt) |>
  summarise(
    year = 2011,
    us_priv_k12 = sum(k12 * w, na.rm = TRUE),
    nc_priv_k12 = sum(k12[state == "NC"] * w[state == "NC"], na.rm = TRUE)
  )

priv_k12_2021 <- pss_2122 |>
  transmute(state = PSTABB, k12 = NUMSTUDS, w = PFNLWT) |>
  summarise(
    year = 2021,
    us_priv_k12 = sum(k12 * w, na.rm = TRUE),
    nc_priv_k12 = sum(k12[state == "NC"] * w[state == "NC"], na.rm = TRUE)
  )

# --- 3. Public K–12 denominators (NCES Table 203.40) ----
# Values: public total enrollment minus public PreK enrollment
pub_k12 <- tibble(
  year = c(2011, 2021),
  us_pub_k12 = c(49521669 - 1290977, 49433092 - 1410554),
  nc_pub_k12 = c(1507864 - 33603, 1525223 - 21304)
)

# --- 4. Combine and compute summary statistics ----
pct_change <- function(a, b) (b - a) / a * 100
priv_share <- function(priv, pub) priv / (priv + pub) * 100

out <- bind_rows(priv_k12_2011, priv_k12_2021) |>
  left_join(pub_k12, by = "year") |>
  reframe(
    geography = c("US", "NC"),
    priv_k12_2011 = c(us_priv_k12[year == 2011], nc_priv_k12[year == 2011]),
    priv_k12_2021 = c(us_priv_k12[year == 2021], nc_priv_k12[year == 2021]),
    priv_k12_growth_pct = pct_change(priv_k12_2011, priv_k12_2021),
    priv_k12_share_2011 = priv_share(
      priv_k12_2011,
      c(us_pub_k12[year == 2011], nc_pub_k12[year == 2011])
    ),
    priv_k12_share_2021 = priv_share(
      priv_k12_2021,
      c(us_pub_k12[year == 2021], nc_pub_k12[year == 2021])
    ),
    share_change_pp = priv_k12_share_2021 - priv_k12_share_2011
  )

out
