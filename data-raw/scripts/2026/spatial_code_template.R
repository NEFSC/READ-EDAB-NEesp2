# strata ----
species_shp <- create_shp(
  strata = {{ strata }},
  orig_shp = shp
)
#
# bt <- create_spatial_indicator(
#   indicator_name = "bottomT",
#   units = "degC",
#   data.in = hubert,
#   file.time = 'annual',
#   output.files = c(here::here('data-raw', '{{ species }}_bottomT.nc')),
#   shp.file = species_shp,
#   var.name = "sea_water_temperature_at_sea_floor",
#   area.names = "stock_area",
#   statistic = 'mean',
#   agg.time = 'days',
#   tz = NA,
#   touches = TRUE,
#   write.out = F
# )
#
# write.csv(
#   bt,
#   here::here('data-raw/2026', '{{ species }}_hubert_bottomT.csv'),
#   row.names = FALSE
# )

# glorys ----
# glorys <- create_spatial_indicator(
#   indicator_name = "bottomT",
#   units = "degC",
#   data.in = glorys,
#   file.time = 'annual',
#   output.files = c(here::here('data-raw', '{{ species }}_glorys.nc')),
#   shp.file = species_shp,
#   var.name = "bottomT",
#   area.names = "stock_area",
#   statistic = 'mean',
#   agg.time = 'days',
#   tz = NA,
#   touches = TRUE,
#   write.out = F
# )
# write.csv(
#   glorys,
#   here::here('data-raw/2026', '{{ species }}_glorys_bottomT.csv'),
#   row.names = FALSE
# )

# map ----
us <- geodata::gadm(
  country = "USA",
  level = 1,
  resolution = 2,
  path = here::here("data-raw")
)

terra::plot(species_shp, col = "lightblue")
terra::plot(us, add = TRUE)

ggsave(
  here::here("data-raw/2026", "{{ species }}_map.png"),
  width = 6,
  height = 6,
  units = "in"
)
