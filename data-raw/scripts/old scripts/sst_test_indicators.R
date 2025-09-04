library(terra)
#monthly file from ERDDAP (https://comet.nefsc.noaa.gov/erddap/griddap/noaa_psl_2d74_d418_a6fb.html)
sst_monthly_converted <- EDABUtilities::convert_longitude(data = here::here('data-raw/sst_monthly.nc'))

terra::plot(subset(terra::rast(here::here('data-raw/sst_monthly.nc')),1))
terra::plot(rotate(subset(rast(here::here('data-raw/sst_monthly.nc')),1)))
terra::plot(sst_monthly_converted)
sst_monthly <- NEesp2::create_spatial_indicator(indicator_name = 'sst',
                                units = 'degC',
                                data.in = sst_monthly_converted,
                                output.files = c(here::here('data-raw','sst_monthly_test.nc')),
                                file.time = 'annual',
                                shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                var.name = 'sst',
                                area.names = c('MAB','GB'),
                                statistic = 'mean',
                                agg.time = 'days',
                                tz = NA,
                                touches = TRUE,
                                write.out = F)

sst_monthly <- EDABUtilities::make_2d_summary_ts(data.in = sst_monthly_converted,
                                                 output.files = c(here::here('data-raw','sst_monthly_test.nc')),
                                                 file.time = 'annual',
                                                 shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                                 var.name = 'sst',
                                                 area.names = c('MAB','GB'),
                                                 statistic = 'mean',
                                                 agg.time = 'days',
                                                 tz = NA,
                                                 touches = TRUE,
                                                 write.out = F)

#weekly file from ERDDAP(https://comet.nefsc.noaa.gov/erddap/griddap/noaa_psl_62b6_f192_98f7.html)
#sst_weekly_converted <- EDABUtilities::convert_longitude(data = here::here('data-raw/sst_weekly.nc'))
v
#sst_weekly <- NEesp2::create_spatial_indicator(indicator_name = 'sst',
 #                                      units = 'degC',
  #                                     data.in = sst_weekly_converted,
   #                                    output.files = c(here::here('data-raw','sst_weekly_test.nc')),
    #                                   file.time = 'annual',
     #                                  shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
      #                                 var.name = 'sst',
       #                                area.names = c('MAB','GB'),
        #                               statistic = 'mean',
         #                              agg.time = 'days',
          #                             tz = NA,
           #                            touches = TRUE,
            #                           write.out = F)

#daily file from PSL (https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/)
filepath <- ncdf4::nc_open(here::here('data-raw/sst.day.mean.2024.nc'))
sst_daily_converted <- EDABUtilities::convert_longitude(data = here::here('data-raw/sst.day.mean.2024.nc'))

sst_daily <- NEesp2::create_spatial_indicator(indicator_name = 'sst',
                                units = 'degC',
                                data.in = sst_daily_converted,
                                output.files = c(here::here('data-raw','sst_test.nc')),
                                file.time = 'annual',
                                shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                var.name = 'sst',
                                area.names = c('MAB','GB'),
                                statistic = 'mean',
                                agg.time = 'days',
                                tz = NA,
                                touches = TRUE,
                                write.out = F)

#monthly ltm file from PSL
sst_ltm_converted <- EDABUtilities::convert_longitude(data = here::here('data-raw/sst.mon.ltm.1991-2020.nc'))

sst_ltm <- NEesp2::create_spatial_indicator(indicator_name = 'sst',
                                              units = 'degC',
                                              data.in = sst_ltm_converted,
                                              output.files = c(here::here('data-raw','sst_test.nc')),
                                              file.time = 'annual',
                                              shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                              var.name = 'sst',
                                              area.names = c('MAB','GB'),
                                              statistic = 'mean',
                                              agg.time = 'days',
                                              tz = NA,
                                              touches = TRUE,
                                              write.out = F)

##This one works
#daily file from ACSPO (https://comet.nefsc.noaa.gov/erddap/griddap/noaa_coastwatch_acspo_v2_nrt.html)
filepath2 <- ncdf4::nc_open(here::here('data-raw/acspo_test.nc')) 

sst_acspo <- NEesp2::create_spatial_indicator(indicator_name = 'sst',
                                     units = 'degC',
                                     data.in = here::here('data-raw/acspo_test.nc'),
                                     output.files = c(here::here('data-raw','acspo_test.nc')),
                                     file.time = 'annual',
                                     shp.file = here::here('data-raw/shapefiles','EPU_NOESTUARIES.shp'),
                                     var.name = 'sea_surface_temperature',
                                     area.names = c('MAB','GB'),
                                     statistic = 'mean',
                                     agg.time = 'days',
                                     tz = NA,
                                     touches = TRUE,
                                     write.out = F)
