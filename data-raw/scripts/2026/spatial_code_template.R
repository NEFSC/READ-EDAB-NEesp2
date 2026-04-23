species_shp <- create_shp(
  strata = {{ strata }},
  orig_shp = shp
)

bt <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = hubert,
  file.time = 'annual',
  output.files = c(here::here('data-raw', '{{ species }}_bottomT.nc')),
  shp.file = species_shp,
  var.name = "sea_water_temperature_at_sea_floor",
  area.names = "stock_area",
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)

write.csv(
  bt,
  here::here('data-raw/2026', '{{ species }}_hubert_bottomT.csv'),
  row.names = FALSE
)

glorys_species <- create_spatial_indicator(
  indicator_name = "bottomT",
  units = "degC",
  data.in = glorys,
  file.time = 'annual',
  output.files = c(here::here('data-raw', '{{ species }}_glorys.nc')),
  shp.file = species_shp,
  var.name = "bottomT",
  area.names = "stock_area",
  statistic = 'mean',
  agg.time = 'days',
  tz = NA,
  touches = TRUE,
  write.out = F
)
write.csv(
  glorys_species,
  here::here('data-raw/2026', '{{ species }}_glorys_bottomT.csv'),
  row.names = FALSE
)


# #not recognizing var.name as "temperature"
# fishbot_halibut <- create_spatial_indicator(
#   indicator_name = "fishbot_bottomT",
#   units = "degC",
#   data.in = c(here::here('data-raw/2026', 'fishbot_2000_2026.nc')),
#   file.time = 'annual',
#   output.files = c(here::here('data-raw', 'fishbot_halibut_bottomT.nc')),
#   shp.file = here::here('data-raw/shapefiles', 'BTS_STRATA.shp'),
#   var.name = 'temperature',
#   area.names = c(
#     "01130",
#     "01140",
#     "01150",
#     "01160",
#     "01170",
#     "01180",
#     "01190",
#     "01200",
#     "01210",
#     "01220",
#     "01230",
#     "01240",
#     "01250",
#     "01260",
#     "01270",
#     "01280",
#     "01290",
#     "01300",
#     "01360",
#     "01370",
#     "01380",
#     "01390",
#     "01400"
#   ),
#   statistic = 'mean',
#   agg.time = 'days',
#   tz = NA,
#   touches = TRUE,
#   write.out = F
# )
