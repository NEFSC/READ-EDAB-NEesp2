
### MACKEREL

sst_converted <- EDABUtilities::convert_longitude(data = here::here('data-raw/2026','oisst_monthly_1981_2026.nc'))

sst_mackerel <- create_spatial_indicator(indicator_name = "sst", 
                                       units = "degC",
                                       data.in = sst_converted,
                                       file.time = 'annual',
                                       output.files = c(here::here('data-raw','mackerel_sst.nc')),
                                       shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                       var.name = "sst",
                                       area.names = c("MAB","GB","GOM","SS"),
                                       statistic = 'mean',
                                       agg.time = 'days',
                                       tz = NA,
                                       touches = TRUE,
                                       write.out = F)
write.csv(sst_mackerel, here::here('data-raw/2026','mackerel_sst.csv'), row.names = F)
