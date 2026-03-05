#' Pull Commercial Fisheries Data from Oracle
#'
#' translated from Stata code 2/24/2026
#' Owner: Samantha Werner
#' 
#' overview-- This code creates six economic commercial fishing indicators used in ESP work.  
#' The code below pulls data, deflates any monetary values and formats data to what is needed for the time series plots. 
#' Must have access to the 'NEFSC_GARFO' schema in Oracle to run this code.
#' Indicators created: Commercial Landings (LBS): The total weight of the species landed (e.g., Commercial_LONGFINSQUID_Landings_LBS).
#'
#' Number of Commercial Vessels: The count of unique permits landing that species (e.g., N_Commercial_Vessels_Landing_LONGFINSQUID).
#' Average Price per Pound: The average annual price, winsorized to handle outliers and adjusted for inflation (e.g., AVGPRICE_LONGFINSQUID_2024_DOLlb).
#' Total Annual Revenue: The total value of all landings for that species, adjusted for inflation (e.g., TOTALANNUALREV_LONGFINSQUID_2024Dols).
#' Average Revenue per Vessel: The average revenue earned per permit per year, adjusted for inflation (e.g., AVGVESREVperYr_LONGFINSQUID_2024_DOLlb).
#' Average Annual Diesel Price: The price of Ultra-Low-Sulfur No. 2 Diesel (from FRED), adjusted for inflation (e.g., AVGANNUAL_DIESEL_PRICE2024dols).
#'
#' This uses CFDERS data but may need to be updated to CAMs 
#' 
#' before running!!!
#' make sure you are connected to VPN
#' ensure all packages are installed 
#' you have a folder in your directory called "data"
#'
#' @param ora_id username for Oracle connection (in quotation marks)
#' @param oraprod_pw password for Oracle connection (in quotation marks)
#' @param spp_name the name of the species you want to pull (e.g., "LONGFINSQUID")
#' @param nespp3_codes the NESPP3 codes for the species you want to (e.g., "('801')") - note the single quotes inside the string for SQL
#' @param start_year the first year you want to pull (e.g., 1996)
#' @param end_year the last year you want to pull (e.g., 2025)
#' @param deflate_yr the year you want to deflate to (e.g, 2025)
#' @export
#' 

library(ROracle)
library(DBI)
library(fredr)
library(tidyverse)
library(DescTools) # For Winsorize

get_commercial_data <- function(
    ora_id,
    oraprod_pw,
    spp_name,
    nespp3_codes,
    start_year,
    end_year,
    deflate_yr
) {


# This looks for a folder named 'data' then 'intermediate' inside your current directory
data_intermediate <- file.path("data/intermediate")


#################################################################
##no editiing should be needed past this point
#####################################################

source("\\nefscdata\\SOE_ESP_Data\\ESPs\\connect_socioeco_oracle.r")

# Using the name consistent with your loop
nefscusers.connect.string <- paste0(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", shost, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SERVICE_NAME=", ssid, ")))"
)

# 3. Connect ONCE outside the loop
drv <- dbDriver("Oracle")
conn <- dbConnect(drv, ora_id, password = oraprod_pw, dbname = nefscusers.connect.string)

##### 4. Price deflator 

gdp_deflator <- fredr(
  series_id = "GDPDEF",
  observation_start = as.Date(paste0(START.YEAR, "-01-01")),
  observation_end = as.Date(paste0(END.YEAR, "-12-31")),
  frequency = "a" # Annual
) %>%
  mutate(YEAR = as.numeric(format(date, "%Y"))) %>%
  select(YEAR, GDPDEF = value)


# 1. Build the query string using your R variables
# We use paste0 to insert the species name into the column alias 
# and the codes into the WHERE clause.
query_landings <- paste0(
  "SELECT YEAR, SUM(SPPLNDLB) AS total_", spp_name, 
  " FROM NEFSC_GARFO.CFDERS_ALL_YEARS ",
  " WHERE NESPP3 IN ", nespp3_codes, 
  "AND YEAR BETWEEN ", START.YEAR, " AND ", END.YEAR,
  " GROUP BY YEAR ORDER BY YEAR"
)

# 2. Execute the query
landings_data <- dbGetQuery(conn, query_landings)

# 3. View the result
print(head(landings_data))


# Standardize the data frame to match the required indicator format
landings_final <- landings_data %>%
  mutate(
    CATEGORY = "Commercial",
    INDICATOR_NAME = paste0("Commercial_", spp_name, "_Landings_LBS"),
    SIGN = "N/A",
    INDICATOR_TYPE = "Socioeconomic"
  ) %>%
  # Rename the sum column to DATA_VALUE (Equivalent to Stata rename)
  rename(DATA_VALUE = !!paste0("TOTAL_", spp_name)) %>%
  select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_NAME, SIGN, INDICATOR_TYPE)

#

######################### n vessels pull #############

# 1. Build the query string 
# I simplified the alias to N_VESSELS so the rename() below actually works.
# I also added a space before 'FROM' to prevent syntax errors.
query_Nvessels <- paste0(
  "SELECT YEAR, count(distinct PERMIT) AS N_VESSELS ", 
  "FROM NEFSC_GARFO.CFDERS_ALL_YEARS ",
  "WHERE NESPP3 IN ", nespp3_codes, 
  " AND YEAR BETWEEN ", START.YEAR, " AND ", END.YEAR,
  " GROUP BY YEAR ORDER BY YEAR"
)

# 2. Execute the query
Nvessels_data <- dbGetQuery(conn, query_Nvessels)

# 3. Format to match Stata indicators
Nvessels_final <- Nvessels_data %>%
  mutate(
    CATEGORY = "Commercial",
    # This creates the long name you want in the final table
    INDICATOR_NAME = paste0("N_Commercial_Vessels_Landing_", spp_name),
    SIGN = "N/A",
    INDICATOR_TYPE = "Socioeconomic"
  ) %>%
  # Now this rename will work because the SQL alias matches 'N_VESSELS'
  # Note: Oracle often returns names in UPPERCASE, so we check for both.
  rename(DATA_VALUE = any_of(c("N_VESSELS", "N_vessels"))) %>%
  select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_NAME, SIGN, INDICATOR_TYPE)

# 4. View the result
print(head(Nvessels_final))

##################average prices##################

#1. Pull Price Data from Oracle ---
# Uses the 'conn' object you already established
query_price <- paste0(
  "SELECT SPPVALUE, SPPLNDLB, YEAR FROM NEFSC_GARFO.CFDERS_ALL_YEARS ",
  "WHERE NESPP3 IN ", nespp3_codes, 
  " AND YEAR BETWEEN ", START.YEAR, " AND ", END.YEAR
)

price_raw <- dbGetQuery(conn, query_price)

#--- 2. Calculate Average Annual Prices (Manual Winsorize) ---
price_annual <- price_raw %>%
  mutate(price_lb = SPPVALUE / SPPLNDLB) %>%
  # Remove Infinity or NA if pounds were 0
  filter(is.finite(price_lb)) %>% 
  group_by(YEAR) %>%
  mutate(
    # Calculate the 1st and 99th percentiles for this year
    p01 = quantile(price_lb, 0.01, na.rm = TRUE),
    p99 = quantile(price_lb, 0.99, na.rm = TRUE),
    # "Squish" values outside that range (This is Winsorizing!)
    price_lb_w = case_when(
      price_lb < p01 ~ p01,
      price_lb > p99 ~ p99,
      TRUE ~ price_lb
    )
  ) %>%
  summarise(AVG_NOMINAL_PRICE = mean(price_lb_w, na.rm = TRUE)) %>%
  ungroup()

# --- 3. Adjust for Inflation (Deflate) ---
# (Keep this the same as before)

base_index_val <- gdp_deflator$GDPDEF[gdp_deflator$YEAR == deflate_yr]

price_final <- price_annual %>%
  left_join(gdp_deflator, by = "YEAR") %>%
  mutate(
    DATA_VALUE = (AVG_NOMINAL_PRICE / GDPDEF) * base_index_val,
    CATEGORY = "Commercial",
    INDICATOR_NAME = paste0("AVGPRICE_", spp_name, "_", deflate_yr, "_DOLlb"),
    SIGN = "N/A",
    INDICATOR_TYPE = "Socioeconomic"
  ) %>%
  select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_NAME, SIGN, INDICATOR_TYPE)

# --- 4. Cleanup ---


print(price_final)


##To ensure all indicators respect your START.YEAR and END.YEAR variables, I have integrated those filters into both the SQL queries (for database pulls) and the fredr calls (for external diesel data).




################## Total Annual Revenues ##################

# 1. Pull Revenue Data
query_revs <- paste0(
  "SELECT YEAR, SUM(SPPVALUE) AS TOTAL_REV ",
  "FROM NEFSC_GARFO.CFDERS_ALL_YEARS ",
  "WHERE NESPP3 IN ", nespp3_codes, 
  " AND YEAR BETWEEN ", START.YEAR, " AND ", END.YEAR,
  " GROUP BY YEAR ORDER BY YEAR"
)
revs_raw <- dbGetQuery(conn, query_revs)

# 2. Deflate and Format
revs_final <- revs_raw %>%
  left_join(gdp_deflator, by = "YEAR") %>%
  mutate(
    DATA_VALUE = (TOTAL_REV / GDPDEF) * base_index_val,
    CATEGORY = "Commercial",
    INDICATOR_NAME = paste0("TOTALANNUALREV_", spp_name, "_", deflate_yr, "Dols"),
    SIGN = "N/A",
    INDICATOR_TYPE = "Socioeconomic"
  ) %>%
  select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_NAME, SIGN, INDICATOR_TYPE)

################## Fuel Prices ##################

# 1. Pull Diesel Price from FRED with Year Range
fuel_raw <- fredr(
  series_id = "DDFUELNYH",
  observation_start = as.Date(paste0(START.YEAR, "-01-01")),
  observation_end = as.Date(paste0(END.YEAR, "-12-31")),
  frequency = "a" 
) %>%
  mutate(YEAR = as.numeric(format(date, "%Y"))) %>%
  # Stata 'drop if missing(DDFUELNYH)' equivalent:
  filter(!is.na(value)) %>%
  select(YEAR, DDFUELNYH = value)

# 2. Deflate and Format
fuel_final <- fuel_raw %>%
  left_join(gdp_deflator, by = "YEAR") %>%
  mutate(
    DATA_VALUE = (DDFUELNYH / GDPDEF) * base_index_val,
    CATEGORY = "Commercial",
    INDICATOR_NAME = paste0("AVGANNUAL_DIESEL_PRICE", deflate_yr, "dols"),
    SIGN = "N/A",
    INDICATOR_TYPE = "Socioeconomic"
  ) %>%
  select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_NAME, SIGN, INDICATOR_TYPE)

############ Average Revenue Per Vessel ##################

# 1. Pull Revenue per Permit/Year with Year Range
query_ves_rev <- paste0(
  "SELECT YEAR, PERMIT, SUM(SPPVALUE) AS VESSEL_TOTAL_REV ",
  "FROM NEFSC_GARFO.CFDERS_ALL_YEARS ",
  "WHERE NESPP3 IN ", nespp3_codes, 
  " AND YEAR BETWEEN ", START.YEAR, " AND ", END.YEAR,
  " GROUP BY YEAR, PERMIT"
)
ves_rev_raw <- dbGetQuery(conn, query_ves_rev)

# 2. Calculate Mean per Year and Deflate
av_ves_rev_final <- ves_rev_raw %>%
  group_by(YEAR) %>%
  summarise(AVG_VESSEL_REV = mean(VESSEL_TOTAL_REV, na.rm = TRUE)) %>%
  left_join(gdp_deflator, by = "YEAR") %>%
  mutate(
    DATA_VALUE = (AVG_VESSEL_REV / GDPDEF) * base_index_val,
    CATEGORY = "Commercial",
    INDICATOR_NAME = paste0("AVGVESREVperYr_", spp_name, "_", deflate_yr, "_DOLlb"),
    SIGN = "N/A",
    INDICATOR_TYPE = "Socioeconomic"
  ) %>%
  select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_NAME, SIGN, INDICATOR_TYPE)

##disconnect from oracle 
dbDisconnect(conn)
################## MASTER APPEND ##################

# 1. Create a list of all your final data frames
# This acts like a 'stack' of all the indicators you just created
indicator_list <- list(
  landings_final, 
  Nvessels_final, 
  price_final, 
  revs_final, 
  fuel_final, 
  av_ves_rev_final
)

# 2. Use bind_rows to stack them into one long file
# This is identical to running 'append' multiple times in Stata
final_master_file <- bind_rows(indicator_list)

# 3. Final Quality Check (Filtering by your start/end years)
final_master_file <- final_master_file %>%
  filter(YEAR >= START.YEAR & YEAR <= END.YEAR) %>%
  arrange(INDICATOR_NAME, YEAR)

# 4. Save the file (Equivalent to Stata's 'save ..., replace')
# Use file.path to make sure the folder and filename are joined correctly
write.csv(
  final_master_file, 
  file = file.path(data_intermediate, paste0(spp_name, "_Commercial_Indicators_Master.csv")), 
  row.names = FALSE
)

# 5. View a summary of what you appended
print(table(final_master_file$INDICATOR_NAME))

}

get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  spp_name = "LONGFINSQUID",
  nespp3_codes = "('801')",
  start_year = 1996,
  end_year = 2025,
  deflate_yr = 2025
)
