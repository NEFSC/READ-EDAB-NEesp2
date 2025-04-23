

# channel <- dbutils::connect_to_database("server name", "user name")

ROracle::dbGetQuery(channel,"SELECT table_name FROM all_tables where regexp_like(TABLE_NAME, 'SVDBS')")

data <- survdat::get_survdat_data(channel = channel, filterByYear = NA, all.season = TRUE, shg.check = T, conversion.factor = T, use.SAD = F, getBio = FALSE, getLengths = FALSE)
data_all <- survdat::get_survdat_data(channel)

# connect to database 

sql <- "select unique year, cruise6, svvessel, season
      from svdbs.mstr_cruise
      where purpose_code = 10
      and (season = 'FALL'
        or season = 'SPRING')
      order by year, cruise6"

d <- DBI::dbGetQuery(channel, sql)

# station data  
sql <- "select unique cruise6, svvessel, station, stratum,
                               tow, decdeg_beglat as lat, decdeg_beglon as lon,
                               begin_est_towdate as est_towdate, avgdepth as depth,
                               surftemp, surfsalin, bottemp, botsalin
                               from svdbs.UNION_FSCS_SVSTA
                               where (SHG <= 136 and cruise6 <= 200900)
                               or (TOGA <= 1324 and cruise6 > 200900)
                               order by cruise6, station"

d <- DBI::dbGetQuery(channel, sql)


message("Getting Species data ...")

sql <- "select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from svdbs.UNION_FSCS_SVCAT
                     where stratum not like 'YT%'
                     order by cruise6, station, svspp"

d <- DBI::dbGetQuery(channel, sql)

sql <- "select cruise6, station, stratum, tow, svspp, catchsex,
                      length, expnumlen as numlen
                      from svdbs.UNION_FSCS_SVLEN
                      where stratum not like 'YT%'
                      order by cruise6, station, svspp, length"

d <- DBI::dbGetQuery(channel, sql)

message("Getting Individual Fish (Bio) Data ...")
sql <- "select cruise6, station, stratum, svspp, catchsex, length, indid,
                  indwt, sex, maturity, age, stom_volume, stom_wgt
                  from svdbs.UNION_FSCS_SVBIO"

d <- DBI::dbGetQuery(channel, sql)

#lengths/widths
lw.qry <- "select svspp, sex, svlwexp, svlwcoeff, svlwexp_spring, svlwcoeff_spring,
               svlwexp_fall, svlwcoeff_fall, svlwexp_winter, svlwcoeff_winter,
               svlwexp_summer, svlwcoeff_summer
               from svdbs.length_weight_coefficients"
d <- DBI::dbGetQuery(channel,lw.qry)

####### functions that do not work
conversion <- survdat::get_conversion_factors(channel)
maturity <- survdat::get_maturity(channel)
sex <- survdat::get_sex(channel)
lw <- survdat::get_length_weight(channel)
