library(magrittr)

###ECODATA
cpi <- create_coldpool_index(ecodata::cold_pool)
cpe <- create_coldpool_extent(ecodata::cold_pool)
cpp <- create_coldpool_persistence(ecodata::cold_pool)

gsi <- create_gsi(ecodata::gsi)

wcr <- create_wcr(ecodata::wcr)

###BOTTOM TRAWL
channel <- dbutils::connect_to_database("NEFSC_USERS","SOWEN")
data <- survdat::get_survdat_data(channel, getBio = T, getLengths = T)
species <- survdat::get_species(channel)
species <- species$data

sab <- create_swept_area(surveyData = data$survdat,
                         areaPolygon = "NEFSC strata",
                         areaDescription = "STRATA",
                         filterByArea = "all",
                         filterBySeason = "all",
                         groupDescription = "SVSPP",
                         filterByGroup = "all",
                         mergesexFlag = T,
                         tidy = F,
                         q = NULL,
                         a = 0.0384)

smb <- create_stratified_mean(surveyData = data$survdat,
areaPolygon = "NEFSC strata",
areaDescription = "STRATA",
filterByArea = "all",
filterBySeason = "all",
groupDescription = "SVSPP",
filterByGroup = "all",
mergesexFlag = T,
tidy = F,
returnPrepData = F
)

range <- species_range(data = data, species)

###CONDITION
condition <- species_condition_orig(data=data$survdat, LWparams = LWparams, species.codes = species.codes)

condition <- species_condition(data = data$survdat, LWparams, species.codes)
pkgdown::build_site()

###SPATIAL

#This works 7/8/25
filepath2 <- ncdf4::nc_open(here::here('data-raw/glorys_bottomT.nc')) 

bt <- create_spatial_indicator(indicator_name = "bottomT", 
                               units = "degC",
                               data.in = c(here::here('data-raw','glorys_bottomT.nc')),
                               file.time = 'months',
                               output.files = c(here::here('data-raw','GLORYS_monthly_bottomT.nc')),
                               shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                               var.name = 'bottomT',
                               area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                              '01140', '01150', '01670', '01680', '01710', '01720', 
                                              '01750', '01760'),
                               statistic = 'mean',
                               agg.time = 'annual',
                               tz = NA,
                               touches = TRUE,
                               write.out = F)

#works 7/8/25
filepath3 <- ncdf4::nc_open(here::here('data-raw/pp_occci_199798.nc')) 

pp <- create_spatial_indicator(indicator_name = "primary_production_mean",
                units = 'gCarbon/m^2/day',
                data.in = c(here::here('data-raw','pp_occci_199798.nc')),
                file.time = 'annual',
                output.files = c(here::here('data-raw','OCCCI_PRIM_PROD.nc')),
                shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                var.name = 'primary productivity',
                area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                               '01140', '01150', '01670', '01680', '01710', '01720', 
                               '01750', '01760'),
                statistic = 'mean',
                agg.time = 'days',
                tz = NA,
                touches = TRUE,
                write.out = F)

#works 7/8/25
filepath <- ncdf4::nc_open(here::here('data-raw/glorys_bottomS.nc')) 

sal <- create_spatial_indicator(indicator_name = 'bottomS',
                                units = '1e-3',
                                data.in = c(here::here('data-raw','glorys_bottomS.nc')),
                                output.files = c(here::here('data-raw','GLORYS_daily_bottomS.nc')),
                                file.time = 'annual',
                                shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                var.name = 'bottomS',
                                area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                               '01140', '01150', '01670', '01680', '01710', '01720', 
                                               '01750', '01760'),
                                statistic = 'mean',
                                agg.time = 'months',
                                tz = NA,
                                touches = TRUE,
                                write.out = F)

#doesn't work, need to download other chl file - joe has one?
filepath4 <- ncdf4::nc_open(here::here('data-raw/chl_2015.nc')) 

chl <- create_spatial_indicator(indicator_name = 'chlor_a',
                                units = 'milligram m-3',
                                data.in = c(here::here('data-raw','chl_2015.nc')),
                                output.files = c(here::here('data-raw','occci_chl.nc')),
                                file.time = 'annual',
                                shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                var.name = 'bottomS',
                                area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                               '01140', '01150', '01670', '01680', '01710', '01720', 
                                               '01750', '01760'),
                                statistic = 'mean',
                                agg.time = 'months',
                                tz = NA,
                                touches = TRUE,
                                write.out = F)

#doesn't work, need input file with matching coords
filepath3 <- ncdf4::nc_open(here::here('data-raw/sst_1989.nc')) 

chl <- create_spatial_indicator(indicator_name = 'sst',
                                units = 'degC',
                                data.in = c(here::here('data-raw','sst_1989.nc')),
                                output.files = c(here::here('data-raw','sst.nc')),
                                file.time = 'annual',
                                shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                                var.name = 'bottomS',
                                area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                               '01140', '01150', '01670', '01680', '01710', '01720', 
                                               '01750', '01760'),
                                statistic = 'mean',
                                agg.time = 'months',
                                tz = NA,
                                touches = TRUE,
                                write.out = F)
