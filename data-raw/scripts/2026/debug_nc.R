file <- here::here("data-raw/2026/fishbot_2000_2026.nc")

terra::rast(file)
terra::sds(file)

nc_check <- ncdf4::nc_open(file)
print(nc_check)
ncdf4::nc_close(nc_check)

# 1. Open the file and see what's inside
nc_file <- ncdf4::nc_open(here::here("data-raw/2026/fishbot_2000_2026.nc"))

# Print to find the exact variable names for lon, lat, and your data
# print(nc_file)

# 2. Extract the coordinates and the data variable
# Replace 'lon', 'lat', and 'variable_name' with the names found in print(nc_file)
lons <- ncdf4::ncvar_get(nc_file, "longitude")
lats <- ncdf4::ncvar_get(nc_file, "latitude")
data_array <- ncdf4::ncvar_get(nc_file, "temperature") # e.g., "temp" or "biomass"

ncdf4::nc_close(nc_file)

# 3. Handle the data structure
# NetCDF data is often stored [lon, lat]. terra expects [row, col]
# We might need to transpose the matrix
data_mat <- t(data_array)

# 4. Manually create the SpatRaster
# We define the extent based on the min/max of our lon/lat vectors
r <- terra::rast(
  data_mat,
  extent = terra::ext(min(lons), max(lons), min(lats), max(lats)),
  crs = "EPSG:4326" # Assuming standard WGS84
)

# 5. Fix orientation
# NetCDF often stores data "upside down" compared to standard GIS formats
r <- terra::flip(r, direction = "vertical")

terra::plot(r)
