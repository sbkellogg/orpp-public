# Private K–12 (exclude PreK) using PSS microdata
# Public K–12 denominators use NCES Digest Table 203.40 (public total minus public preK):
# Fall 2011: https://nces.ed.gov/programs/digest/d13/tables/dt13_203.40.asp
# Fall 2021: https://nces.ed.gov/programs/digest/d22/tables/dt22_203.40.asp

library(readr)
library(dplyr)

# --- 1) Read your two PSS public-use files ---
# 2011–12 file is tab-delimited text
pss_1112 <- read_tsv("enrollments/pss1112_pu.txt", show_col_types = FALSE)

# 2021–22 file is CSV
pss_2122 <- read_csv("enrollments/pss2122_pu.csv", show_col_types = FALSE)

# --- 2) Compute weighted private K–12 totals (excluding PreK) ---
# In both files, NUMSTUDS is the non-PreK student total (K–12 only).
# We use the final weight: pfnlwt (2011–12) and PFNLWT (2021–22).

priv_k12_2011 <- pss_1112 %>%
    transmute(state = pstabb, k12 = numstuds, w = pfnlwt) %>%
    summarise(
        us_priv_k12_2011 = sum(k12 * w, na.rm = TRUE),
        nc_priv_k12_2011 = sum(
            k12[state == "NC"] * w[state == "NC"],
            na.rm = TRUE
        )
    )

priv_k12_2021 <- pss_2122 %>%
    transmute(state = PSTABB, k12 = NUMSTUDS, w = PFNLWT) %>%
    summarise(
        us_priv_k12_2021 = sum(k12 * w, na.rm = TRUE),
        nc_priv_k12_2021 = sum(
            k12[state == "NC"] * w[state == "NC"],
            na.rm = TRUE
        )
    )

# Pull scalars
us_priv_2011 <- priv_k12_2011$us_priv_k12_2011
nc_priv_2011 <- priv_k12_2011$nc_priv_k12_2011
us_priv_2021 <- priv_k12_2021$us_priv_k12_2021
nc_priv_2021 <- priv_k12_2021$nc_priv_k12_2021

# Percent change (2011 -> 2021)
pct_change <- function(a, b) (b - a) / a * 100
us_priv_growth_pct <- pct_change(us_priv_2011, us_priv_2021)
nc_priv_growth_pct <- pct_change(nc_priv_2011, nc_priv_2021)

# --- 3) Public K–12 denominators from NCES Table 203.40 (public total minus public preK) ---
# Fall 2011 (Table 203.40, Digest 2013):
pub_total_us_2011 <- 49521669
pub_prek_us_2011 <- 1290977
pub_total_nc_2011 <- 1507864
pub_prek_nc_2011 <- 33603

# Fall 2021 (Table 203.40, Digest 2022):
pub_total_us_2021 <- 49433092
pub_prek_us_2021 <- 1410554
pub_total_nc_2021 <- 1525223
pub_prek_nc_2021 <- 21304

pub_k12_us_2011 <- pub_total_us_2011 - pub_prek_us_2011
pub_k12_nc_2011 <- pub_total_nc_2011 - pub_prek_nc_2011
pub_k12_us_2021 <- pub_total_us_2021 - pub_prek_us_2021
pub_k12_nc_2021 <- pub_total_nc_2021 - pub_prek_nc_2021

# --- 4) Private share of total K–12 (private / (public + private)) ---
share <- function(priv, pub) priv / (priv + pub) * 100

us_share_2011 <- share(us_priv_2011, pub_k12_us_2011)
us_share_2021 <- share(us_priv_2021, pub_k12_us_2021)
nc_share_2011 <- share(nc_priv_2011, pub_k12_nc_2011)
nc_share_2021 <- share(nc_priv_2021, pub_k12_nc_2021)

# --- 5) Print results ---
out <- tibble(
    geography = c("US", "NC"),
    priv_k12_2011 = c(us_priv_2011, nc_priv_2011),
    priv_k12_2021 = c(us_priv_2021, nc_priv_2021),
    priv_k12_growth_pct = c(us_priv_growth_pct, nc_priv_growth_pct),
    priv_k12_share_2011 = c(us_share_2011, nc_share_2011),
    priv_k12_share_2021 = c(us_share_2021, nc_share_2021),
    share_change_pp = c(
        us_share_2021 - us_share_2011,
        nc_share_2021 - nc_share_2011
    )
)

print(out)

# import file
