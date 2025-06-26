# File: SMSS_paper_data_filtering.R
# Author: [Walker Haskins] ([walkersth@gmail.com])
# School: University of Amsterdam
# Programme: Research MA Social Sciences
# Course: Statistical Models in the Social Sciences
# Teacher: Dr. Sara Geven
# Last Modified: 2025-06-26
#
# Description: Script preparing data for regression analysis.
#
# Usage:
# Should be run before the results script, SMSS_paper_data_filtering.R
#
# ------------------------------------------------------------------------

library(peacesciencer) # terrain data
library(readxl)
library(haven)
library(dplyr)
library(countrycode) # country codes conversion
library(clipr) # copy-paste for my sanity
library(xml2)
library(tidyr)
library(janitor)  # somehow fixes bugs?
library(remotes)
library(stringr)
library(writexl)
library(vdemdata) #vdemdata for concatenation later
library(gtsummary)
library(gt)

data(rugged) # this is the terrain data from the peacesciencer package
setwd("C:\\Users\\walke\\Downloads")


# DATA LOADING -----------------------------------------------------------------
#FL_rep <- read_stata("repdata.dta")             # F&L's rep data for reference
UDCP2 <- read.csv2(                                   # UDCP data on civil wars
  "UcdpPrioConflict_v25_1.csv",
  sep = ","
) 
Penn_income <- read_xlsx(                       # Penn world tables income data
  "income.xlsx",
  sheet = "Data"
) 
PS_terrain <- rugged                                    # Renaming terrain data
PolV <- read_xls("p5v2018.xls")                                 # Polity V data
Reilly_SC <- read_xlsx("statecap_finalv14.xlsx")          # State capacity data 
                                                         # for robustness check
ncontig_data <- read_xlsx("Ncontig manual coding.xlsx")  # Non-contig territory
                                                          # coding, done myself
OP_WB <- read_xlsx("Pop_oil_WB_3.xlsx")             # World Bank pop & oil data


# BUILDING THE COUNTRY-GRID ----------------------------------------------------
# --- Getting Civil War data ---
refilter <- UDCP2 %>%                # storing necessary UCDP data in an object
  filter(
    year %in% 2000:2018,                 # set to 2018 for congruence with PolV
    type_of_conflict %in% c(3)                                # Only civil wars
  ) %>% 
  arrange(gwno_a, year) %>%
  mutate(                              # conv'ng to iso3 w/ countrycode package
    gwno_a = as.numeric(gwno_a),       # after lots of experiments iso3 is best
    iso3_code = countrycode(
      gwno_a,                                                # for avoiding NAs
      origin = "gwn",
      destination = "iso3c"
    )
  )

# --- quick definitions ---
rf_states <- as.vector(                      # storing list of states for later
  unique(refilter$iso3_code)
)

years <- 2000:2018                                # storing year span for later

# ---Creating Country-Grid ---
RRF <- expand_grid(                                # Creating country-year grid
  iso3_code = rf_states,
  year = years
) 

RRF <- left_join(           # adding civil war onsets data to country-year grid
  RRF,
  refilter %>%
    group_by(iso3_code, year) %>%                   # merging in the UCDP data
    summarise( 
      civil_war_active = as.numeric(
        any(
          type_of_conflict %in% c(3, 4),
          na.rm = TRUE
        )
      ),
      start_date2 = min(
        as.Date(
          start_date2,
          format = "%Y-%m-%d"
        ),
        na.rm = TRUE
      ),
      ep_end = max(
        as.Date(
          ep_end,
          format = "%Y-%m-%d"
        ),
        na.rm = TRUE
      ),
      gwno_a = first(gwno_a),
      region = first(region),
      type_of_conflict = first(type_of_conflict),
      conflict_id = first(conflict_id),
      .groups = "drop"
    ),
  by = c("iso3_code", "year")
) %>%
  mutate(civil_war_active = replace_na(civil_war_active, 0)) %>%
  mutate(start_date2 = ifelse(is.infinite(start_date2), NA, start_date2)) %>%
  mutate(ep_end = ifelse(is.infinite(ep_end), NA, ep_end))


# DEPDENDENT VARIABLE CREATION -------------------------------------------------
RRF <- RRF %>%                               # creating civil war onset, the DV
  arrange(iso3_code, year) %>%
  group_by(iso3_code) %>%
  mutate(
    prev_civil_war_active = lag(civil_war_active, n = 1, default = 0),
    civil_war_onset = ifelse(civil_war_active == 1 & prev_civil_war_active == 0, 1, 0)
  ) %>%
  ungroup()


# GDP DATA ---------------------------------------------------------------------

# --- NA check pre-merge ---
sum(is.na(Penn_income$rgdpe)) # 0 NAs

# --- Filtering Penn WT data ---
Penn_income <- Penn_income %>%
  filter(year %in% 2000:2018) %>%                  # Filtering years to 2000-18
  filter(countrycode %in% rf_states) %>%
  mutate(
    gwcode = countrycode(
      countrycode,
      origin = "iso3c",
      destination = "gwn"
    )
  ) %>%                                                     # No ambig. matches
  
  dplyr::select(                                  # selecting necessary columns
    countrycode,
    country,
    year,
    pop,
    rgdpe,
    gwcode
  )

# --- Merging to main grid ---
RRF <- left_join(
  RRF, 
  Penn_income,
  by = c(
    "iso3_code" = "countrycode",
    "year" = "year"
  )
)

# --- NA check post-merge ---
sum(is.na(RRF$rgdpe))                                           # 74 NAs...why?


# TERRAIN DATA -----------------------------------------------------------------
# --- NA check pre-merge ---
sum(is.na(rugged$newlmtnest))                                # 1 NA - just #327

# --- Filtering Peacesciencer data ---
rugged <- rugged %>%
  mutate(
    iso3c = countrycode(
      ccode,
      origin = "cown",
      destination = "iso3c"
    )
  )                                                    # 678, 816 ambig matches

# --- Merging to main grid ---
RRF <- left_join(
  RRF,
  rugged,
  by = c("iso3_code" = "iso3c")                    # 753 and 38 had multi-match
) 

# -- NA Check post-merge ---
sum(is.na(RRF$newlmtest))                                          # yay, none!


# OIL RENTS % AND NAT'L POPULATION DATA ----------------------------------------
# --- NA check pre-merge ---
sum(is.na(OP_WB$WB_pop))                                    # 0 NAs in pop data
sum(is.na(OP_WB$WB_oil_v))                                  # 0 NAs in oil data

# --- Filtering World Bank data ---
OP_WB <- OP_WB %>%
  janitor::clean_names() %>%                                  # magical bug-fix
  filter(year %in% 2000:2018) %>%                             # filtering years
  dplyr::select(                                               # filtering vars
    country_code,
    value,
    wb_oil_v,
    year
  ) %>%
  filter(country_code %in% rf_states) %>%
  mutate(
    country_code = str_trim(country_code)                     # magical bug fix
  ) %>%
  mutate(
    country_code = toupper(country_code)                   # magical bug fix #2
  ) %>%
  rename(
    WB_pop = value,
    WB_oil_v = wb_oil_v
  )

# --- Merging to main grid ---
RRF <- left_join(
  RRF, 
  OP_WB,
  by = c(
    "iso3_code" = "country_code",
    "year" = "year"
  )
)

# -- NA Check post-merge ---
sum(is.na(RRF$WB_pop))                                                    # 30?
sum(is.na(RRF$WB_oil_v))                                                  # 30?


# STATE CAPACITY ---------------------------------------------------------------
# --- NA check pre-merge ---
sum(is.na(Reilly_SC$statecap_baseline))                                  # many                                    
sum(is.na(Reilly_SC$statecap_base_fiscal))                          # also many

# --- Filtering State Capacity Index data ---
statecap <- Reilly_SC %>%                                     # filtering years
  filter(year %in% c(2000:2018)) %>%
  filter(country_text_id %in% rf_states) %>%
  dplyr::select(                                               # filtering vars
    country_text_id,
    year,
    statecap_baseline,
    statecap_base_fiscal
  )

# --- Merging to main grid ---
RRF <- left_join(
  RRF, 
  statecap,
  by = c(
    "iso3_code" = "country_text_id",
    "year" = "year"
  )
)

# -- NA Check post-merge ---
sum(is.na(RRF$statecap_baseline))                                          # 41                                               
sum(is.na(RRF$statecap_base_fiscal))                                       # 56                                                


# ANOCRACY & POL. INSTABILITY --------------------------------------------------
# --- NA check pre-merge --
sum(is.na(PolV$democ))                                              # all 0 NAs
sum(is.na(PolV$autoc))
sum(is.na(PolV$polity))
sum(is.na(PolV$polity2))

# --- Filtering Polity V data ---
PolV <- PolV %>%                                              # filtering years
  filter(year %in% c(2000:2018))

PolV <- PolV %>%                                     # converting country codes
  mutate(
    iso3c = countrycode(
      scode,
      origin = "p5c",
      destination = "iso3c"                              # KOS, SUD, YGS ambig.
    )
  ) 


PolV <- PolV %>%
  filter(iso3c %in% rf_states) %>%        # use gwcode for filtering, not scode
  dplyr::select(                                               # filtering vars
    year,
    cyear,
    country,
    polity,
    polity2,
    byear,
    democ,
    autoc,
    scode,
    iso3c
  )

# --- Merging to main grid ---
RRF <- left_join(
  RRF,
  PolV,
  by = c(
    "iso3_code" = "iso3c",
    "year" = "year"
  )
)                                                        # 450, 855 muiltimatch

# --- Merge double-check - had problems earlier ---
missing_polity_data <- anti_join(
  RRF,
  PolV,
  by = c("iso3_code" = "iso3c", "year" = "year")
)


# -- NA Check post-merge ---
sum(is.na(RRF$democ))                                                  # 22 NAs
sum(is.na(RRF$autoc))                                                      # 22
sum(is.na(RRF$polity))                                                     # 22
sum(is.na(RRF$polity2))                                                  # 48??



# NON-CONTINGOUOS DATA ---------------------------------------------------------
# See note at top of script - manually coded this with help from the FL_rep data
# and articles from CIA world factbook. No NAs because I did it myself.

# --- Merging to main grid ---
RRF <- left_join(
  RRF,
  ncontig_data,
  by = c("iso3_code" = "iso3c")
)

# -- NA Check post-merge ---
sum(is.na(RRF$ncontig))                                                    # 30


# V_DEM (USED FOR SUPPLEMENTING GDP/POP DATA -----------------------------------
# --- NA check pre-merge --
sum(is.na(vdem$e_gdppc))                                                 # 5581
sum(is.na(vdem$e_pop))                                                   # 5581

# --- Filtering V-Dem data ---
vdem_data <- vdem %>%
  dplyr::select(
    year,
    country_name,
    country_text_id,
    e_gdppc,
    e_pop
  ) %>%
  filter(year %in% 2000:2018)

# --- Merging to main grid ---
RRF <- left_join(
  RRF,
  vdem_data,
  by = c(
    "iso3_code" = "country_text_id",
    "year" = "year"
  )
)

# -- NA Check post-merge ---
sum(is.na(RRF$e_gdppc))                                                    # 41
sum(is.na(RRF$e_pop))                                                      # 41


# PROCESSING DATA --------------------------------------------------------------
pro_rrf <- RRF %>% #Filtering to the needed variables
  dplyr::select(
    iso3_code,
    year,
    start_date2,
    ccode,
    region,
    pop,
    WB_pop,
    WB_oil_v,
    statecap_baseline,
    statecap_base_fiscal,
    polity,
    polity2,
    byear,
    democ,
    autoc,
    newlmtnest,
    rugged,
    rgdpe,
    ncontig,
    e_gdppc,
    e_pop,
    conflict_id,
    civil_war_onset
  )

pro_rrf <- pro_rrf %>%                                    # arranging as needed
  arrange(iso3_code, year)

# --- checking NAs to check country-year grid works ---
sum(is.na(pro_rrf$iso3_code))                           # 0! all countries here
sum(is.na(pro_rrf$year))                                # 0! all years are good


# NA VALUE REDUCTION -----------------------------------------------------------

# --- Coalescing WB POP & GDP with V-Dem measures ---
sum(is.na(pro_rrf$WB_pop))
sum(is.na(pro_rrf$rgdpe))

pro_rrf <- pro_rrf %>%                                        # coalescing here
  mutate(rgdpe = coalesce(rgdpe, e_gdppc))

pro_rrf <- pro_rrf %>%
  filter(!is.na(iso3_code))

pro_rrf %>%                                                # checking the merge
  filter(is.na(rgdpe)) %>%
  dplyr::select(iso3_code, year, rgdpe) %>%
  View()


# --- Unifying NA format ---
pro_rrf <- pro_rrf %>%
  mutate(
    across(
      c(WB_oil_v),
      ~ na_if(., "NA")
    )
  )

# --- fixing region and byear ---
# region and birthyear are identical for all countries across the time series
# so using downup to fix - they're from UCDP so theyre NA in non-occurance rows

pro_rrf <- pro_rrf %>%                                                 # region
  arrange(iso3_code, year) %>%
  group_by(iso3_code) %>%
  fill(region, .direction = "downup") %>% 
  ungroup()

pro_rrf <- pro_rrf %>%                                                  # byear
  arrange(iso3_code, year) %>%
  group_by(iso3_code) %>%
  fill(byear, .direction = "downup") %>% 
  ungroup()

sum(is.na(pro_rrf$region))
sum(is.na(pro_rrf$byear))


# --- fixing start year ---
# same situation as above re downup
pro_rrf <- pro_rrf %>%
  mutate(start_year_numeric = as.numeric(substr(start_date2, 1, 4)))

pro_rrf <- pro_rrf %>%
  arrange(iso3_code, year) %>%
  group_by(iso3_code) %>%
  fill(start_year_numeric, .direction = "down") %>%
  fill(start_year_numeric, .direction = "downup") %>%
  ungroup()


pro_rrf <- pro_rrf %>%
  mutate(
    across(
      c(democ, autoc, polity, polity2),
      ~ case_when(
        . %in% c(-99) ~ NA_real_,
        TRUE ~ .
      )
    )
  )

# --- pre CCA NA tibble ---
NA_tibble_wide <- pro_rrf %>%
  summarise(
    across(
      everything(),
      list(
        NA_count = ~sum(is.na(.)),
        NA_pct = ~round(mean(is.na(.)) * 100, 1)
      )
    )
  )


NA_tibble_long <- NA_tibble_wide %>%            # long version - easier to read
  pivot_longer(
    cols = everything(), 
    names_to = c("Variable", ".value"), 
    names_sep = "_"
  )

# View(NA_tibble_long) # comemnting out to avoid pop-ups


# COUNTRY FOUNDING YEARS (UCDP - EXTRACTED MANUALLY) ---------------------------

country_founding_years <- tibble(
  iso3_code = c(
    "AFG", "AGO", "AZE", "BDI", "BFA", "BGD", "CAF", "CHN", "CIV", "CMR",
    "COD", "COG", "COL", "DZA", "EGY", "ERI", "ETH", "GEO", "GIN", "HTI",
    "IDN", "IND", "IRN", "IRQ", "ISR", "JOR", "KEN", "LBN", "LBR", "LBY",
    "LKA", "MKD", "MLI", "MMR", "MOZ", "MRT", "MYS", "NER", "NGA", "NPL",
    "PAK", "PER", "PHL", "RUS", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD",
    "SYR", "TCD", "THA", "TJK", "TUN", "TUR", "UGA", "UKR", "USA", "UZB"
  ),
  founding_year = c(
    1919, 1975, 1991, 1962, 1960, 1971, 1960, 1949, 1960, 1960,
    1960, 1960, 1810, 1962, 1922, 1993, 1270, 1991, 1958, 1804,
    1945, 1947, 1979,
    1932, 1948, 1946, 1963, 1943, 1847, 1951, 1948, 1991, 1960,
    1948, 1975, 1960, 1957, 1960, 1960, 1768, 1947, 1821, 1946,
    1991, 1962, 1956, 1960, 1961, 1960, 2011, 1946, 1960, 1782,
    1991, 1956, 1923, 1962, 1991, 1776, 1991
  )
)

# --- merging them in ---
pro_rrf <- left_join(pro_rrf, country_founding_years, by = "iso3_code")



# VARIABLE CONSTRUCTIONS -------------------------------------------------------

# --- new state indicator ---
pro_rrf <- pro_rrf %>%
  mutate(                       # (checking if state is new according to byear)
    year_since_founding = year - founding_year,
    newstate = ifelse(
      year_since_founding >= 0 & year_since_founding <= 3,
      1,
      0
    )
  )


# --- Anocracy --- 
pro_rrf <- pro_rrf %>%
  mutate(                               # (1 if autoc-democ greater than abs 5)
    anoc = ifelse(abs(autoc - democ) < 5, 1, 0)
  )

# --- Oil exporter --- 
pro_rrf <- pro_rrf %>%                 # 1 if over 30% of GDP in a country-year
  mutate(
    WB_oil_dummy = ifelse(
      WB_oil_v >= 30, 1, 0)) 

# --- Nat'l population (lagged 1 year; log) ---
pro_rrf <- pro_rrf %>%
  group_by(iso3_code) %>%                             # Grouped by country code
  mutate(log_WB_pop_lag1 = lag(log(WB_pop), n = 1, order_by = year)) %>%
  ungroup()

# --- Log of Mountainous Terrain % ---
pro_rrf <- pro_rrf %>%
  mutate(log_newlmtnest = log(newlmtnest))

# --- GDP (lagged 1 year; log) ---
pro_rrf <- pro_rrf %>%
  group_by(iso3_code) %>%
  mutate(log_GDP_lag1 = log(lag(rgdpe, n = 1, order_by = year))) %>%
  ungroup()

# --- Political instability dummy ---
# a la F&L, -66 & -77 are coded as instability, - 88 as missing
instability_codes_vec <- c(-66, -77, -88) # storing Pol V's NA codes in an object

pro_rrf <- pro_rrf %>%
  group_by(iso3_code) %>% 
  mutate(
    polity2_cleaned = case_when(
      polity2 %in% instability_codes_vec ~ NA_real_,
      TRUE ~ as.numeric(polity2)
    ),
    polity2_change_abs_lag1 = abs(polity2_cleaned - lag(polity2_cleaned, n = 1, order_by = year)),
    polity2_change_abs_lag2 = abs(polity2_cleaned - lag(polity2_cleaned, n = 2, order_by = year)),
    polity2_change_abs_lag3 = abs(polity2_cleaned - lag(polity2_cleaned, n = 3, order_by = year)),
    dummy_change_lag1_L = (polity2_change_abs_lag1 >= 3),
    dummy_change_lag2_L = (polity2_change_abs_lag2 >= 3),
    dummy_change_lag3_L = (polity2_change_abs_lag3 >= 3),
    code_instability_current_year_L = (polity2 %in% instability_codes_vec),
    code_instability_lag1_year_L = (lag(polity2, n = 1, order_by = year) %in% instability_codes_vec),
    code_instability_lag2_year_L = (lag(polity2, n = 2, order_by = year) %in% instability_codes_vec),
    code_instability_lag3_year_L = (lag(polity2, n = 3, order_by = year) %in% instability_codes_vec),
    polity_instability_dummy = as.numeric(
      (dummy_change_lag1_L == TRUE & !is.na(dummy_change_lag1_L)) |
        (dummy_change_lag2_L == TRUE & !is.na(dummy_change_lag2_L)) |
        (dummy_change_lag3_L == TRUE & !is.na(dummy_change_lag3_L)) |
        (code_instability_current_year_L == TRUE & !is.na(code_instability_current_year_L)) |
        (code_instability_lag1_year_L == TRUE & !is.na(code_instability_lag1_year_L)) |
        (code_instability_lag2_year_L == TRUE & !is.na(code_instability_lag2_year_L)) |
        (code_instability_lag3_year_L == TRUE & !is.na(code_instability_lag3_year_L))
    )
  ) %>%
  ungroup() %>%
  dplyr::select(
    -polity2_cleaned,
    -polity2_change_abs_lag1, -polity2_change_abs_lag2, -polity2_change_abs_lag3,
    -dummy_change_lag1_L, -dummy_change_lag2_L, -dummy_change_lag3_L,
    -code_instability_current_year_L, -code_instability_lag1_year_L,
    -code_instability_lag2_year_L, -code_instability_lag3_year_L
  )


#territorial distance/noncontigous territory
#this is just ncontig now
unique(pro_rrf$iso3_code)


count_of_onsets <- sum(pro_rrf$civil_war_onset == 1, na.rm = TRUE)

print(count_of_onsets)



# DESCRIPTIVE STATS (PRE-CLEANING) ----------------------------------------

model_vars <- c(                                          # Defining model vars
  "civil_war_onset",
  "log_WB_pop_lag1",
  "log_GDP_lag1",
  "newstate",
  "anoc",
  "WB_oil_dummy",
  "polity_instability_dummy",
  "ncontig",
  "log_newlmtnest"
)

dirty_desc_table <- pro_rrf %>%
  select(all_of(model_vars)) %>%
  tbl_summary(
    missing = "always",
    missing_text = "Missing",
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    label = list(
      civil_war_onset ~ "Civil War Onset",
      log_WB_pop_lag1 ~ "Log Population (lag)",
      log_GDP_lag1 ~ "Log GDP (lag)",
      anoc ~ "Anocracy",
      WB_oil_dummy ~ "Oil Exporter",
      polity_instability_dummy ~ "Political Instability",
      ncontig ~ "Noncontiguous",
      log_newlmtnest ~ "Log Terrain Ruggedness"
    ),
    
    digits = all_continuous() ~ 2
  ) %>% 
  modify_caption("**Descriptive Statistics of Variables (Pre-listwise deletion)**")

dirty_desc_table

dirty_desc_table %>%
  as_gt() %>%  # Convert to gt object
  gt::as_latex() %>%  # Convert to LaTeX
  writeLines("dirty_descriptive_stats.tex")  # Save to file
