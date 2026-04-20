## Download FISHBOT data from ERDDAP
end = Sys.Date()
start = end - lubridate::days(7)
url = paste0(
  "https://erddap.ondeckdata.com/erddap/tabledap/fishbot_realtime.csvp?time%2Ctemperature%2Cgrid_id%2Clatitude%2Clongitude&time%3E=",
  lubridate::year(start),
  "-",
  lubridate::month(start),
  "-",
  lubridate::day(start),
  "T00%3A00%3A00Z&time%3C=",
  lubridate::year(Sys.Date()),
  "-",
  lubridate::month(Sys.Date()),
  "-",
  lubridate::day(Sys.Date()),
  "T00%3A00%3A00Z"
)
# url=paste0(
#   'https://erddap.ondeckdata.com/erddap/tabledap/fishbot_realtime.csvp?time%2Ctemperature%2Ctemperature_count%2Cdata_provider%2Cgrid_id%2Clatitude%2Clongitude&time%3E=2025-08-21T00%3A00%3A00Z&time%3C=2025-08-28T00%3A00%3A00Z'
# )
data = read.csv(url)
## Download bathymetric data
bath = marmap::getNOAA.bathy(
  lon1 = min(-80.83),
  lon2 = max(-56.79),
  lat1 = min(34),
  lat2 = max(46.89),
  resolution = 1
)
## Create color ramp
blues = c(
  "lightsteelblue4",
  "lightsteelblue3",
  "lightsteelblue2",
  "lightsteelblue1"
)
## Set up plot
filename = paste0(here::here("data-raw/scripts/2026/FISHBOT.png"))
file.create(filename)

png(
  filename,
  height = 1000,
  width = 800,
  units = "px"
)
plot(
  bath,
  step = 100,
  deepest.isobath = -1000,
  shallowest.isobath = 0,
  col = "darkgray",
  image = TRUE,
  land = TRUE,
  lwd = 0.1,
  bpal = list(
    c(0, max(bath), "gray"),
    c(min(bath), 0, blues)
  ),
  main = paste0(
    "Gridded Bottom Temperature Observations ",
    start,
    " to ",
    end
  ),
  xlim = c(-76, -66),
  ylim = c(34, 45)
)
## Load the shapefile
grid = sf::read_sf(here::here(
  "data-raw/scripts/2026/7km_trimmed_fixedgeometries/7km_grid_clipped_fixed_geometry.shp"
))
grid = subset(grid, grid$id %in% data$grid_id)
grid$temp = NA
grid$color = NA
for (i in 1:nrow(grid)) {
  grid$temp[i] = mean(
    subset(data, data$grid_id == grid$id[i])$temperature..degree_C.
  )
}
grid$TEMP = grid$temp * 9 / 5 + 32
mintemp = floor(min(grid$TEMP))
maxtemp = ceiling(max(grid$TEMP))
tempcol = viridis::magma(n = length(seq(mintemp, maxtemp, 1)))
for (i in 1:nrow(grid)) {
  grid$color[i] = tempcol[which(
    seq(mintemp, maxtemp, 1) == round(grid$TEMP[i], 0)
  )]
}
plot(
  sf::st_geometry(grid),
  max.plot = 1,
  add = TRUE,
  border = grid$color,
  col = grid$color
)
datacolors = data.frame(
  TEMP = seq(mintemp, maxtemp, 1),
  COL = tempcol
)
datacolors = subset(
  datacolors,
  datacolors$TEMP %in% round(quantile(datacolors$TEMP), 0)
)
legend(
  'bottomright',
  fill = datacolors$COL,
  border = datacolors$COL,
  legend = datacolors$TEMP,
  title = "Bottom Temperature (F)"
)
dev.off()
