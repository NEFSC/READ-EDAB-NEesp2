# This script is used to pull commercial data for the 2026 data update stocks. 
# It uses the 'get_commercial_data' function from the R/get_commercial_data.R script.
# Need to run on network drive to source the 'connect_socioeco_oracle' script.

NEesp2::get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  START.YEAR = 1996,
  END.YEAR = 2025,
  deflate_yr = 2025,
  spp_name = "LONGFINSQUID",
  nespp3_codes = "('801')"
)

NEesp2::get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  START.YEAR = 1996,
  END.YEAR = 2025,
  deflate_yr = 2025,
  spp_name = "ATLANTICHALIBUT",
  nespp3_codes = "('159')"
)

NEesp2::get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  START.YEAR = 1996,
  END.YEAR = 2025,
  deflate_yr = 2025,
  spp_name = "ATLANTICPOLLOCK",
  nespp3_codes = "('269')"
)

NEesp2::get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  START.YEAR = 1996,
  END.YEAR = 2025,
  deflate_yr = 2025,
  spp_name = "ATLANTICREDHAKE",
  nespp3_codes = "('152')"
)

NEesp2::get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  START.YEAR = 1996,
  END.YEAR = 2025,
  deflate_yr = 2025,
  spp_name = "SILVERHAKE",
  nespp3_codes = "('509')"
)

NEesp2::get_commercial_data(
  ora_id = "user",
  oraprod_pw = "password",
  START.YEAR = 1996,
  END.YEAR = 2025,
  deflate_yr = 2025,
  spp_name = "WITCHFLOUNDER",
  nespp3_codes = "('122')"
)

