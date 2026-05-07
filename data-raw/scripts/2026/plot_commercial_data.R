### DIESEL PRICE (ALL)
diesel_price <- read.csv(here::here("data/intermediate/ATLANTICHALIBUT_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGANNUAL_DIESEL_PRICE2025dols") 

NEesp2::plt_indicator(data = diesel_price,
                      ar = 1/4,
                      include_trends = TRUE)

### HALIBUT
avgprice <- read.csv(here::here("data/intermediate/ATLANTICHALIBUT_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_ATLANTICHALIBUT_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/ATLANTICHALIBUT_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_ATLANTICHALIBUT_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/ATLANTICHALIBUT_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_ATLANTICHALIBUT_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/ATLANTICHALIBUT_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_ATLANTICHALIBUT") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/ATLANTICHALIBUT_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_ATLANTICHALIBUT_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)

### POLLOCK
avgprice <- read.csv(here::here("data/intermediate/ATLANTICPOLLOCK_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_ATLANTICPOLLOCK_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/ATLANTICPOLLOCK_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_ATLANTICPOLLOCK_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/ATLANTICPOLLOCK_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_ATLANTICPOLLOCK_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/ATLANTICPOLLOCK_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_ATLANTICPOLLOCK") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/ATLANTICPOLLOCK_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_ATLANTICPOLLOCK_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)

### MACKEREL
avgprice <- read.csv(here::here("data/intermediate/ATLANTICMACKEREL_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_ATLANTICMACKEREL_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/ATLANTICMACKEREL_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_ATLANTICMACKEREL_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/ATLANTICMACKEREL_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_ATLANTICMACKEREL_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/ATLANTICMACKEREL_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_ATLANTICMACKEREL") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/ATLANTICMACKEREL_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_ATLANTICMACKEREL_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)

### SCALLOP
avgprice <- read.csv(here::here("data/intermediate/SEASCALLOP_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_SEASCALLOP_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/SEASCALLOP_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_SEASCALLOP_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/SEASCALLOP_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_SEASCALLOP_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/SEASCALLOP_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_SEASCALLOP") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/SEASCALLOP_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_SEASCALLOP_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)

### WITCH FLOUNDER
avgprice <- read.csv(here::here("data/intermediate/WITCHFLOUNDER_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_WITCHFLOUNDER_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/WITCHFLOUNDER_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_WITCHFLOUNDER_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/WITCHFLOUNDER_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_WITCHFLOUNDER_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/WITCHFLOUNDER_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_WITCHFLOUNDER") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/WITCHFLOUNDER_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_WITCHFLOUNDER_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)

### REDHAKE
avgprice <- read.csv(here::here("data/intermediate/ATLANTICREDHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_ATLANTICREDHAKE_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/ATLANTICREDHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_ATLANTICREDHAKE_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/ATLANTICREDHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_ATLANTICREDHAKE_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/ATLANTICREDHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_ATLANTICREDHAKE") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/ATLANTICREDHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_ATLANTICREDHAKE_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)

### SILVER HAKE
avgprice <- read.csv(here::here("data/intermediate/SILVERHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGPRICE_SILVERHAKE_2025_DOLlb") 

NEesp2::plt_indicator(data = avgprice,
                      ar = 1/4,
                      include_trends = TRUE) 

vesselrev <- read.csv(here::here("data/intermediate/SILVERHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "AVGVESREVperYr_SILVERHAKE_2025_DOLlb") 

NEesp2::plt_indicator(data = vesselrev,
                      ar = 1/4,
                      include_trends = TRUE)

landings <- read.csv(here::here("data/intermediate/SILVERHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "Commercial_SILVERHAKE_Landings_LBS") 

NEesp2::plt_indicator(data = landings,
                      ar = 1/4,
                      include_trends = TRUE)

nvessels <- read.csv(here::here("data/intermediate/SILVERHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "N_Commercial_Vessels_Landing_SILVERHAKE") 

NEesp2::plt_indicator(data = nvessels,
                      ar = 1/4,
                      include_trends = TRUE)

totalrev <- read.csv(here::here("data/intermediate/SILVERHAKE_Commercial_Indicators_Master.csv")) |>
  dplyr::filter(INDICATOR_NAME == "TOTALANNUALREV_SILVERHAKE_2025Dols") 

NEesp2::plt_indicator(data = totalrev,
                      ar = 1/4,
                      include_trends = TRUE)
