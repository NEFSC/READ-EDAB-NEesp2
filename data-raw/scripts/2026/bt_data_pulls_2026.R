# Notes:
# halibut + pollock have the same strata
# red and silver hake have the same strata
# check the witch flounder strata, may be out of bounds for hubert/glorys BT

devtools::load_all()

### FILEPATHS FOR EXTRACTING VAR.NAME
glorys <- ncdf4::nc_open(here::here('data-raw/2026/glorys_2021_2026.nc'))
hubert <- ncdf4::nc_open(here::here(
  'data-raw/2026/duPontavice_bottom_temp_1959_2021.nc'
))
fishbot <- ncdf4::nc_open(here::here('data-raw/2026/fishbot_2000_2026.nc'))

## create stock shapefile from strata provided
shp <- terra::vect(here::here('data-raw/shapefiles', 'BTS_STRATA.shp'))

create_shp <- function(strata, orig_shp = shp) {
  shp_out <- orig_shp[orig_shp$STRATUMA %in% strata, ] |>
    terra::aggregate()
  # add dummy attribute so it works with edab_utils
  shp_out$region <- "stock_area"

  return(shp_out)
}

### HALIBUT

halibut_shp <- create_shp(
  strata = c(
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400"
  )
)

#not recognizing var.name as "temperature"
fishbot_halibut <- create_spatial_indicator(
  indicator_name = "fishbot_bottomT",
  units = "degC",
  data.in = c(here::here('data-raw/2026', 'fishbot_2000_2026.nc')),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'fishbot_halibut_bottomT.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = 'temperature',
  area.names = c(
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)

bt_halibut <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here(
    'data-raw/2026',
    'duPontavice_bottom_temp_1959_2021.nc'
  )),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'halibut_bottomT.nc')),
  shp.file = halibut_shp,
  var.name = "sea_water_temperature_at_sea_floor",
  area.names = "stock_area",
  statistic = 'mean',
  agg.time = 'months',
  tz = NA,
  touches = TRUE,
  write.out = F
)

write.csv(
  bt_halibut,
  here::here('data-raw/2026', 'halibut_hubert_bottomT.csv'),
  row.names = FALSE
)

glorys_halibut <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here('data-raw/2026', 'glorys_2021_2026.nc')),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'halibut_glorys.nc')),
  shp.file = halibut_shp,
  var.name = "bottomT",
  area.names = "stock_area",
  statistic = 'mean',
  agg.time = 'months',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  glorys_halibut,
  here::here('data-raw/2026', 'halibut_glorys_bottomT.csv'),
  row.names = FALSE
)

### POLLOCK
bt_pollock <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here(
    'data-raw/2026',
    'duPontavice_bottom_temp_1959_2021.nc'
  )),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'pollock_bottomT.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = "sea_water_temperature_at_sea_floor",
  area.names = c(
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  bt_pollock,
  here::here('data-raw/2026', 'pollock_hubert_bottomT.csv'),
  row.names = FALSE
)

glorys_pollock <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here('data-raw/2026', 'glorys_2021_2026.nc')),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'pollock_glorys.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = "bottomT",
  area.names = c(
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  glorys_pollock,
  here::here('data-raw/2026', 'pollock_glorys_bottomT.csv'),
  row.names = FALSE
)


### RED HAKE (AS UNIT, NEED STRATA FOR N/S)
bt_redhake <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here(
    'data-raw/2026',
    'duPontavice_bottom_temp_1959_2021.nc'
  )),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'redhake_bottomT.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = "sea_water_temperature_at_sea_floor",
  area.names = c(
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400",
    "01010",
    "01020",
    "01030",
    "01040",
    "01050",
    "01060",
    "01070",
    "01080",
    "01090",
    "01100",
    "01110",
    "01120",
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01610",
    "01620",
    "01630",
    "01640",
    "01650",
    "01660",
    "01670",
    "01680",
    "01690",
    "01700",
    "01710",
    "01720",
    "01730",
    "01740",
    "01750",
    "01760"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  bt_redhake,
  here::here('data-raw/2026', 'redhake_hubert_bottomT.csv'),
  row.names = FALSE
)

glorys_redhake <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here('data-raw/2026', 'glorys_2021_2026.nc')),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'redhake_glorys.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = "bottomT",
  area.names = c(
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400",
    "01010",
    "01020",
    "01030",
    "01040",
    "01050",
    "01060",
    "01070",
    "01080",
    "01090",
    "01100",
    "01110",
    "01120",
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01610",
    "01620",
    "01630",
    "01640",
    "01650",
    "01660",
    "01670",
    "01680",
    "01690",
    "01700",
    "01710",
    "01720",
    "01730",
    "01740",
    "01750",
    "01760"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  glorys_redhake,
  here::here('data-raw/2026', 'redhake_glorys_bottomT.csv'),
  row.names = FALSE
)

### SILVER HAKE (AS UNIT, NEED STRATA FOR N/S)
bt_silverhake <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here(
    'data-raw/2026',
    'duPontavice_bottom_temp_1959_2021.nc'
  )),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'silverhake_bottomT.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = "sea_water_temperature_at_sea_floor",
  area.names = c(
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400",
    "01010",
    "01020",
    "01030",
    "01040",
    "01050",
    "01060",
    "01070",
    "01080",
    "01090",
    "01100",
    "01110",
    "01120",
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01610",
    "01620",
    "01630",
    "01640",
    "01650",
    "01660",
    "01670",
    "01680",
    "01690",
    "01700",
    "01710",
    "01720",
    "01730",
    "01740",
    "01750",
    "01760"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  bt_silverhake,
  here::here('data-raw/2026', 'silverhake_hubert_bottomT.csv'),
  row.names = FALSE
)

glorys_silverhake <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here('data-raw/2026', 'glorys_2021_2026.nc')),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'silverhake_glorys.nc')),
  shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
  var.name = "bottomT",
  area.names = c(
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400",
    "01010",
    "01020",
    "01030",
    "01040",
    "01050",
    "01060",
    "01070",
    "01080",
    "01090",
    "01100",
    "01110",
    "01120",
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01610",
    "01620",
    "01630",
    "01640",
    "01650",
    "01660",
    "01670",
    "01680",
    "01690",
    "01700",
    "01710",
    "01720",
    "01730",
    "01740",
    "01750",
    "01760"
  ),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  glorys_redhake,
  here::here('data-raw/2026', 'silverhake_glorys_bottomT.csv'),
  row.names = FALSE
)

### WITCH FLOUNDER
# USING BTS_STRATA.shp ERRORS OUT WITH A CROPPING ERROR (Error: [crop] cannot crop a SpatRaster with an empty extent, [mask] CRS do not match )
bt_witchflounder_EPU <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here(
    'data-raw/2026',
    'duPontavice_bottom_temp_1959_2021.nc'
  )),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'witchflounder_bottomT.nc')),
  shp.file = here::here('data-raw/shapefiles', 'EPU_NOESTUARIES.shp'),
  var.name = "sea_water_temperature_at_sea_floor",
  area.names = c("MAB", "GB", "GOM", "SS"),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  bt_witchflounder_EPU,
  here::here('data-raw/2026', 'witchflounder_hubert_bottomT.csv'),
  row.names = FALSE
)

# glorys_witchflounder_BTS <- create_spatial_indicator(indicator_name = "bottomT",
#                                               units = "degC",
#                                               data.in = c(here::here('data-raw/2026','glorys_2021_2026.nc')),
#                                               file.time = 'annual',
#                                               output.files = c(here::here('data-raw','witchflounder_glorys.nc')),
#                                               shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
#                                               var.name = "bottomT",
#                                               area.names = c("04010", "04030", "04060", "04080", "09250",
#                                                              "09260", "09270", "09280", "09290", "09300",
#                                                              "09310", "09320", "09330", "09340", "09350",
#                                                              "09360", "01220", "01230", "01240", "01250",
#                                                              "01260", "01270", "01280", "01290", "01300",
#                                                              "01360", "01370", "01380", "01390", "01400"),
#                                               statistic = 'mean',
#                                               agg.time = 'days',
#                                               tz = NA,
#                                               touches = TRUE,
#                                               write.out = F)

glorys_witchflounder_EPU <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = c(here::here('data-raw/2026', 'glorys_2021_2026.nc')),
  file.time = 'annual',
  output.files = c(here::here('data-raw', 'witchflounder_glorys.nc')),
  shp.file = here::here('data-raw/shapefiles', 'EPU_NOESTUARIES.shp'),
  var.name = "bottomT",
  area.names = c("MAB", "GB", "GOM", "SS"),
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  glorys_witchflounder_EPU,
  here::here('data-raw/2026', 'witchflounder_glorys_bottomT.csv'),
  row.names = FALSE
)
