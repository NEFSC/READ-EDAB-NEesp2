
#### BOTTOM SALINITY

# extract var.name
glorys <- ncdf4::nc_open(here::here('data-raw/2026/glorys_salinity_1970_2026.nc')) #var.name = 'so'

### MACKEREL
so_mackerel <- create_spatial_indicator(indicator_name = "bottomS", 
                                         units = "1e-3",
                                         data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                         file.time = 'annual',
                                         output.files = c(here::here('data-raw','mackerel_bottomS.nc')),
                                         shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                         var.name = "so",
                                         area.names = c("MAB","GB","GOM","SS"),
                                         statistic = 'mean',
                                         agg.time = 'days',
                                         tz = NA,
                                         touches = TRUE,
                                         write.out = F)
write.csv(so_mackerel, here::here('data-raw/2026','mackerel_bottomS.csv'), row.names = F)

### SCALLOP
so_scallop <- create_spatial_indicator(indicator_name = "bottomS", 
                                        units = "1e-3",
                                        data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                        file.time = 'annual',
                                        output.files = c(here::here('data-raw','scallop_bottomS.nc')),
                                        shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                        var.name = "so",
                                        area.names = c("MAB","GB"),
                                        statistic = 'mean',
                                        agg.time = 'days',
                                        tz = NA,
                                        touches = TRUE,
                                        write.out = F)
                                        
write.csv(so_scallop, here::here('data-raw/2026','scallop_bottomS.csv'), row.names = F)

### HALIBUT
so_halibut <- create_spatial_indicator(indicator_name = "bottomS", 
                                       units = "1e-3",
                                       data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                       file.time = 'annual',
                                       output.files = c(here::here('data-raw','halibut_bottomS.nc')),
                                       shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                       var.name = "so",
                                       area.names = c("01130", "01140", "01150", "01160", "01170", "01180", "01190",
                                                      "01200", "01210", "01220", "01230", "01240", "01250", "01260",
                                                      "01270", "01280", "01290", "01300", "01360", "01370", "01380", "01390", "01400"),
                                       statistic = 'mean',
                                       agg.time = 'days',
                                       tz = NA,
                                       touches = TRUE,
                                       write.out = F)
write.csv(so_halibut, here::here('data-raw/2026','halibut_bottomS.csv'), row.names = F)

### POLLOCK
so_pollock <- create_spatial_indicator(indicator_name = "bottomS", 
                                       units = "1e-3",
                                       data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                       file.time = 'annual',
                                       output.files = c(here::here('data-raw','pollock_bottomS.nc')),
                                       shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                       var.name = "so",
                                       area.names = c("01130", "01140", "01150", "01160", "01170", "01180", "01190",
                                                      "01200", "01210", "01220", "01230", "01240", "01250", "01260",
                                                      "01270", "01280", "01290", "01300", "01360", "01370", "01380", "01390", "01400"),
                                       statistic = 'mean',
                                       agg.time = 'days',
                                       tz = NA,
                                       touches = TRUE,
                                       write.out = F)
write.csv(so_pollock, here::here('data-raw/2026','pollock_bottomS.csv'), row.names = F)

### SILVER HAKE
so_silverhake <- create_spatial_indicator(indicator_name = "bottomS", 
                                       units = "1e-3",
                                       data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                       file.time = 'annual',
                                       output.files = c(here::here('data-raw','silverhake_bottomS.nc')),
                                       shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                       var.name = "so",
                                       area.names = c("01200", "01210", "01220", "01230", "01240", "01250",
                                                      "01260", "01270", "01280", "01290", "01300", "01360",
                                                      "01370", "01380", "01390", "01400", "01010","01020",
                                                      "01030", "01040", "01050", "01060", "01070", "01080",
                                                      "01090", "01100", "01110", "01120", "01130", "01140",
                                                      "01150", "01160", "01170", "01180", "01190", "01610",
                                                      "01620", "01630", "01640", "01650", "01660", "01670",
                                                      "01680", "01690", "01700", "01710", "01720", "01730",
                                                      "01740", "01750", "01760"),
                                       statistic = 'mean',
                                       agg.time = 'days',
                                       tz = NA,
                                       touches = TRUE,
                                       write.out = F)

write.csv(so_silverhake, here::here('data-raw/2026','silverhake_bottomS.csv'), row.names = F)

### RED HAKE
so_redhake <- create_spatial_indicator(indicator_name = "bottomS", 
                                          units = "1e-3",
                                          data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                          file.time = 'annual',
                                          output.files = c(here::here('data-raw','redhake_bottomS.nc')),
                                          shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                          var.name = "so",
                                          area.names = c("01200", "01210", "01220", "01230", "01240", "01250",
                                                         "01260", "01270", "01280", "01290", "01300", "01360",
                                                         "01370", "01380", "01390", "01400", "01010","01020",
                                                         "01030", "01040", "01050", "01060", "01070", "01080",
                                                         "01090", "01100", "01110", "01120", "01130", "01140",
                                                         "01150", "01160", "01170", "01180", "01190", "01610",
                                                         "01620", "01630", "01640", "01650", "01660", "01670",
                                                         "01680", "01690", "01700", "01710", "01720", "01730",
                                                         "01740", "01750", "01760"),
                                          statistic = 'mean',
                                          agg.time = 'days',
                                          tz = NA,
                                          touches = TRUE,
                                          write.out = F)

write.csv(so_redhake, here::here('data-raw/2026','redhake_bottomS.csv'), row.names = F)

### WITCH FLOUNDER
so_witch_flounder <- create_spatial_indicator(indicator_name = "bottomS", 
                                       units = "1e-3",
                                       data.in = (here::here('data-raw/2026/glorys_salinity_1970_2026.nc')),
                                       file.time = 'annual',
                                       output.files = c(here::here('data-raw','witchflounder_bottomS.nc')),
                                       shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                       var.name = "so",
                                       area.names = c("MAB","GB","GOM","SS"),
                                       statistic = 'mean',
                                       agg.time = 'days',
                                       tz = NA,
                                       touches = TRUE,
                                       write.out = F)
write.csv(so_witch_flounder, here::here('data-raw/2026','witchflounder_bottomS.csv'), row.names = F)
