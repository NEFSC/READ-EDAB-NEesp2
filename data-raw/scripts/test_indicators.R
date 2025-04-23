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

###SPATIAL

sal <- create_sal(data.in = c(here::here('data-raw','glorys_bottomS.nc')),
                   output.files = c(here::here('data-raw','GLORYS_daily_bottomS.nc')),
                   shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                   var.name = 'bottomS',
                   area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                  '01140', '01150', '01670', '01680', '01710', '01720', 
                                  '01750', '01760'),
                   statistic = 'mean',
                   agg.time = 'days',
                   tz = NA,
                   touches = TRUE,
                   write.out = F)

bt <- create_spatial_indicator(indicator_name = "bottomT", 
                               units = "degC",
                               data.in = c(here::here('data-raw','glorys_bottomT.nc')),
                               output.files = c(here::here('data-raw','GLORYS_monthly_bottomT.nc')),
                               shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                               var.name = 'bottomT',
                               area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                              '01140', '01150', '01670', '01680', '01710', '01720', 
                                              '01750', '01760'),
                               statistic = 'mean',
                               agg.time = 'days',
                               tz = NA,
                               touches = TRUE,
                               write.out = F)

#DOES NOT WORK WITH EDAB_UTILITIES
sst <- create_sst(data.in = c(here::here('data-raw','sst_1989.nc')),
                  output.files = c(here::here('data-raw','OISST_1989.nc')),
                  shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                  var.name = 'sst',
                  area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                 '01140', '01150', '01670', '01680', '01710', '01720', 
                                 '01750', '01760'),
                  statistic = 'mean',
                  agg.time = 'days',
                  tz = NA,
                  touches = TRUE,
                  write.out = F)

#DOES NOT WORK WITH EDAB_UTILITIES
chl <- create_chl(data.in = c(here::here('data-raw','chl_2015.nc')),
                  output.files = c(here::here('data-raw','OCCCI_CHL.nc')),
                  shp.file = here::here('data-raw/shapefiles','BTS_STRATA.shp'),
                  var.name = 'chlorophyll-a',
                  area.names = c('01030', '01040', '01070', '01080', '01110', '01120', 
                                 '01140', '01150', '01670', '01680', '01710', '01720', 
                                 '01750', '01760'),
                  statistic = 'mean',
                  agg.time = 'days',
                  tz = NA,
                  touches = TRUE,
                  write.out = F)

pp <- create_pp(data.in = c(here::here('data-raw','pp_occci_199798.nc')),
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
