# Dongchen's code for data prep ahead of site selection
# A lot of this is inefficient and can be refactored and cleaned up...

knitr::opts_chunk$set(echo = TRUE)
# Set the root directory for all chunks
knitr::opts_knit$set(root.dir = "/shared/users-bucket/mhayden/obs_prep/Kirtland/")

library(pathviewr)
library(purrr)
library(data.table)
library(terra)
library(sf)
library(DBI)
library(RSQLite)

# load basic files.

# load shapefile - create based on installations.
Dod.shapefile <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/DoD_shapefile/Kirtland_AFB.shp")

# Create a mock dist.mat for a single-polygon shapefile to preserve loop structures
# Column 2 contains the polygon index (which is 1)
dist.mat <- matrix(c(1, 1), nrow = 1, ncol = 2)
# when repeating this for multiple shapefiles, I won't need this

# data prep.
# setup base map - these agent maps were created elsewhere but cover all of NA - where were they created?
base.map <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/merged_images/AGENT_2022.tif")

## GEDI-4A.
bb <- terra::ext(Dod.shapefile)

metadata <- PEcAn.data.remote::NASA_DAAC_download(ul_lat = bb[4], 
                                  ul_lon = bb[1], 
                                  lr_lat = bb[3], 
                                  lr_lon = bb[2], 
                                  from = "2019-01-01",
                                  to = "2025-12-31", 
                                  just_path = F,
                                  outdir = "/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/downloads",
                                  credential_path = "~/.netrc",
                                  doi = "10.3334/ORNLDAAC/2056",
                                  ncore = 1)
# where is this next file created? what is it? is it just for this site??
# load("/projectnb/dietzelab/dongchen/SDA_disturbance/site_selection_data/coordinates.Rdata")
# site_info <- list(lat = lon.lat.list[[3]]$lat, lon = lon.lat.list[[3]]$lon)
# which.point.in.which.file <- PEcAn.data.remote::GEDI_L4A_Finder_batch(files = list.files("/projectnb/dietzelab/dongchen/SDA_disturbance/gedi/download", full.names = T), 
#                                                                       outdir = "/projectnb/dietzelab/dongchen/SDA_disturbance/gedi",
#                                                                       num.folder = 30, 
#                                                                       cores = 28,
#                                                                       site_info = site_info, 
#                                                                       buffer = 0.000325308/2)
files <- list.files("/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/downloads/", full.names = T)
# loop over beams.
dat <- c()
for (i in seq_along(files)) {
# load file.
level4a_h5 <- hdf5r::H5File$new(files[i], mode = "r")
# load beams id.
groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level4a_h5,recursive = F)), value = T)
for (j in groups_id) {
  level4a_i <- level4a_h5[[j]]
  
  if (any(hdf5r::list.datasets(level4a_i) == "shot_number")) {
    rhs <- data.table::data.table(
      lat_lowestmode = level4a_i[["lat_lowestmode"]][],
      lon_lowestmode = level4a_i[["lon_lowestmode"]][],
      agbd           = level4a_i[["agbd"]][],
      agbd_se        = level4a_i[["agbd_se"]][],  # <-- Pulls the SD/SE directly from the H5 beam
      delta_time     = level4a_i[["delta_time"]][]
    )
    
    # Extract the raw seconds
    rhs$shot_time <- as.POSIXct(rhs$delta_time, origin = "2018-01-01", tz = "UTC")
    rhs <- rhs[which(rhs$agbd >= 0), ]
    dat <- rbind(dat, rhs)
  }
}
# lat/lon filter based on extends.
dat <- dat[dat$lon_lowestmode >= bb[1] &
           dat$lon_lowestmode <= bb[2] &
           dat$lat_lowestmode >= bb[3] &
           dat$lat_lowestmode <= bb[4], ]
level4a_h5$close_all()
print(i)
}
save(dat, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/dat.Rdata")
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/dat.Rdata")

# 1. Load the reference DEM
ref_dem <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/dem_merged/dem_resample.tif")

# 2. Crop the DEM to the shapefile's extent, then wrap in rast() to create an empty template
r_template <- terra::rast(terra::crop(ref_dem, Dod.shapefile))
# Ensure dat is a data.table (it looks like you already did this, but just in case)
setDT(dat)

# 1. Calculate the cell number for each point (super fast math operation)
coords <- as.matrix(dat[, .(lon_lowestmode, lat_lowestmode)])
dat[, cell_id := cellFromXY(r_template, coords)]

# 2. Count the number of points in each cell (.N is data.table's count function)
# (If you wanted mean AGBD instead, you'd do: mean_agbd = mean(agbd, na.rm=TRUE))
cell_summary <- dat[!is.na(cell_id), .(pt_count = .N, mean_agbd = mean(agbd, na.rm = TRUE)), by = cell_id]

# 3. Write those summarized values directly into your raster template
point_count_raster <- r_template
values(point_count_raster) <- NA # Ensure it's blank to start
point_count_raster[cell_summary$cell_id] <- cell_summary$pt_count
names(point_count_raster) <- "gedi_density"
plot(point_count_raster)
writeRaster(point_count_raster, "/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/point_count_raster.tif", overwrite=TRUE)
gedi_density <- terra::extract(point_count_raster*1, Dod.shapefile)
gedi_density$gedi_density[which(is.na(gedi_density$gedi_density))] <- 0
save(gedi_density, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/gedi_density.Rdata")

# 
point_count_raster <- r_template
values(point_count_raster) <- NA # Ensure it's blank to start
point_count_raster[cell_summary$cell_id] <- cell_summary$mean_agbd
names(point_count_raster) <- "gedi_agb"
plot(point_count_raster)
writeRaster(point_count_raster, "/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/gedi_agb.tif", overwrite=TRUE)

## DEM.
# initialize list.
dem.list <- vector("list", length = length(dist.mat[,2]))
# loop over polygons.
for (i in seq_along(dist.mat[, 2])) {
  # extract.
  temp.poly <- Dod.shapefile[dist.mat[i, 2], ]
  temp.ext <- terra::extract(ref_dem, temp.poly)
  # store.
  dem.list[[i]] <- list(dem.df = temp.ext, poly.id = dist.mat[i, 2])
}
save(dem.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/dem.Rdata")

## Stand Age.
# read the forest age map.
forest.age <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/forest_age_resample.tif")
# initialize list.
agent.raw.list <- forest.age.list <- vector("list", length = length(dist.mat[,2]))
# setup agent ID for wildfire.
agent.num <- 6
# loop over polygons.
for (i in seq_along(agent.raw.list)) {
  temp.vect <- Dod.shapefile[dist.mat[i, 2],]
  # loop over years.
  agent.ext <- c()
  forest.age.list[[i]] <- terra::extract(forest.age, temp.vect)[,2]
  for (j in 1988:2022) {
    # load the corresponding agent raster map.
    temp.agent <- terra::rast(file.path(paste0("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/resample_agent_maps/AGENT_", j, ".tif")))
    temp.agent.ext <- terra::extract(temp.agent, temp.vect) %>% data.frame
    temp.agent.ext[,2][which(temp.agent.ext[,2] != agent.num)] <- 0
    temp.agent.ext[,2][which(temp.agent.ext[,2] == agent.num)] <- 1
    agent.ext <- rbind(agent.ext, temp.agent.ext[,2])
  }
  agent.raw.list[[i]] <- list(agent.ext)
  print(i)
}

stand.age.calc <- function(x, age) {
  ind <- which(x == 1)
  if (length(ind) == 0) {
    if (is.na(age)) {
      return(length(x))
    } else {
      return(age - 10)
    }
  } else {
    dist.year <- ind[length(ind)]
    stand.age <- length(x) - dist.year
    return(stand.age)
  }
}

stand.age.list <- agent.raw.list %>% purrr::map2(forest.age.list, function(agent.raw, forest.age) {
  seq_along(forest.age) %>% purrr::map(function(i) {
    stand.age.calc(agent.raw[[1]][, i], forest.age[i])
  }) %>% unlist
})

save(stand.age.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/stand_age.Rdata")

# plot density distribution.
for (i in seq_along(stand.age.list)) {
  plot(density(stand.age.list[[i]]), xlab="Stand Age (Wildfire)")
}

# gSURRGO alternative option

mukey.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/muraster_30m_resample.tif")

# 1. Extract the raster values FIRST so we know exactly which soils we need
temp.vect <- Dod.shapefile[dist.mat[1, 2], ]
temp.mukey.ext <- terra::extract(mukey.rast, temp.vect)

# Find the unique mukeys for just this site
site_mukeys <- unique(temp.mukey.ext$MUKEY)
site_mukeys <- site_mukeys[!is.na(site_mukeys)]
mukey_sql <- paste(site_mukeys, collapse = "','") # Format for SQL query

# 2. Connect to the massive GPKG in STRICTLY Read-Only Mode (bypasses bucket locks!)
gpkg_path <- "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/2025_ALL_SSURGO.gpkg"
con <- dbConnect(RSQLite::SQLite(), gpkg_path, flags = RSQLite::SQLITE_RO)

# 3. Ask the database ONLY for the components present at our site
comp_query <- sprintf("SELECT rowid AS cokey, mukey FROM component WHERE mukey IN ('%s')", mukey_sql)
components <- dbGetQuery(con, comp_query)

# 4. Ask the database ONLY for the chorizon attributes for those components
cokey_sql <- paste(components$cokey, collapse = "','")
chorizon_query <- sprintf("SELECT cokey, sandtotal_r, silttotal_r, claytotal_r, awc_r, ksat_r, om_r, ph1to1h2o_r FROM chorizon WHERE cokey IN ('%s')", cokey_sql)
attributes <- dbGetQuery(con, chorizon_query)

# Disconnect gracefully
dbDisconnect(con)

# 5. Merge the attributes and components
attributes_with_mukey <- merge(
  attributes, 
  components[, c("mukey", "cokey")], 
  by = "cokey"
)

# 6. Proceed with the rest of your original data prep for this site
col.select <- list("sand" = "sandtotal_r", "silt" = "silttotal_r", "clay" = "claytotal_r", 
                   "whc" = "awc_r", "hc" = "ksat_r", 
                   "omp" = "om_r", "ph" = "ph1to1h2o_r")

soil.temp <- c()

# Quick debug print to see if we found ANY soil data at all
print(paste("Total valid horizons found in database:", nrow(attributes_with_mukey)))

for (j in seq_along(col.select)) {
  temp.lookup <- attributes_with_mukey[, c("mukey", col.select[[j]])] %>% 
    as.data.frame() %>% `colnames<-`(c("mukey", "var"))
  
  # SAFE AGGREGATION: Check if we actually have valid (non-NA) data to aggregate
  if (nrow(temp.lookup) > 0 && any(!is.na(temp.lookup$var))) {
    
    # Aggregate across depths, dropping NAs only for the calculation
    temp.lookup <- aggregate(var ~ mukey, data = temp.lookup, FUN = mean, na.rm = TRUE)
    
    from_values <- as.numeric(as.character(temp.lookup[, 1]))
    to_values <- as.numeric(as.character(temp.lookup[, 2]))
    
    # replace mukey with the actual measurements
    res <- to_values[match(temp.mukey.ext$MUKEY, from_values)]
    
  } else {
    # If the database has no data for this variable (very common for DoD sites),
    # just fill the raster area with NAs gracefully
    res <- rep(NA, nrow(temp.mukey.ext))
  }
  
  soil.temp <- cbind(soil.temp, res)
}
colnames(soil.temp) <- names(col.select)

# Store in your list structure
soil.list <- list(soil.temp)
save(soil.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/soil.Rdata")
print("Soil data successfully extracted and saved!")

### gSURRGO soil data.
## load the resampled raster map.
#mukey.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/muraster_30m_resample.tif")

## Load the attribute table from the GeoPackage (once extraction is 100%)
#gpkg_path <- "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/2025_ALL_SSURGO.gpkg"
#attributes <- st_read(gpkg_path, layer = "chorizon") # 'chorizon' contains the physical properties

## 1. Load the component table from your GPKG
#components <- st_read(gpkg_path, layer = "component")
## If they do, create the column manually:
#components$cokey <- rownames(components)
## This forces the internal rowid (cokey) to be readable
#components <- st_read(gpkg_path, query = "SELECT rowid AS cokey, * FROM component")
## merge the cockey field to the attribute table.
#attributes_with_mukey <- merge(
#  attributes, 
#  components[, c("mukey", "cokey")], 
#  by = "cokey"
#)
## create dictionary for the naming conventions.
#col.select <- list("sand" = "sandtotal_r", "silt" = "silttotal_r", "clay" = "claytotal_r", # sand, silt, and clay percentage.
#                   "whc" = "awc_r", "hc" = "ksat_r", # water holding capacity and conductivity rate.
#                   "omp" = "om_r", "ph" = "ph1to1h2o_r") # organic matter percentage and pH.
## loop over polygons.
#soil.list <- vector("list", length = length(dist.mat[,2]))
#for (i in seq_along(soil.list)) {
#  temp.vect <- Dod.shapefile[dist.mat[i, 2],]
#  soil.temp <- c()
#  # loop over variables.
#  for (j in seq_along(col.select)) {
#    temp.lookup <- attributes_with_mukey[, c("mukey", col.select[[j]])] %>% 
#      as.data.frame() %>% `colnames<-`(c("mukey", "var"))
#    # aggregate across depths.
#    temp.lookup <- aggregate(var ~ mukey, data = temp.lookup, FUN = mean)
#    # filter the 100cm depth.
#    # temp.lookup <- temp.lookup[attributes_with_mukey$hzdept_r == 100, ]
#    # extract mukey.
#    temp.mukey.ext <- terra::extract(mukey.rast, temp.vect)
#    # Passing it as a single table object
#    from_values <- as.numeric(as.character(temp.lookup[, 1]))
#    to_values <- as.numeric(as.character(temp.lookup[, 2]))
#    # replace mukey with the actual measurements.
#    res <- to_values[match(temp.mukey.ext$MUKEY, from_values)]
#    soil.temp <- cbind(soil.temp, res)
#  }
#  colnames(soil.temp) <- names(col.select)
#  soil.list[[i]] <- soil.temp
#}
#save(soil.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/soil.Rdata")

## prism climate.
# "gdalwarp -te -129.2773199 21.8050362 -63.1184616 52.9217197 -tr 0.000325307972546 0.000325307972546 -t_srs EPSG:4326 -r near -co COMPRESS=DEFLATE -co BIGTIFF=YES /projectnb/dietzelab/dongchen/SDA_disturbance/LC/Annual_NLCD_LndCov_2000_CU_C1V1.tif /projectnb/dietzelab/dongchen/SDA_disturbance/LC/lc_resample.tif"
var.names <- c("precip", "temp", "dewpoint", "solarRad", "vpdmax")
# loop over polygons.
climate.list <- vector("list", length = length(dist.mat[,2]))
for (i in seq_along(climate.list)) {
  temp.vect <- Dod.shapefile[dist.mat[i, 2],]
  climate.temp <- c()
  # loop over climate variables.
  for (j in seq_along(var.names)) {
    temp.rast <- terra::rast(file.path("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/climate/resample", paste0(var.names[j], ".tif")))
    temp.ext <- terra::extract(temp.rast, temp.vect)
    climate.temp <- cbind(climate.temp, temp.ext[,2])
  }
  colnames(climate.temp) <- var.names
  climate.list[[i]] <- climate.temp
}
save(climate.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/climate.Rdata")

## AGB.
# loop over polygons.
agb.list <- vector("list", length = length(dist.mat[,2]))
agb.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/AGB/agb_2020_resample.tif")
for (i in seq_along(agb.list)) {
  temp.vect <- Dod.shapefile[dist.mat[i, 2],]
  temp.ext <- terra::extract(agb.rast, temp.vect)
  agb.list[[i]] <- temp.ext[,2]
}
save(agb.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/agb.Rdata")

## LC.
# loop over polygons.
lc.list <- vector("list", length = length(dist.mat[,2]))
lc.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/LC/lc_resample.tif")
for (i in seq_along(lc.list)) {
  temp.vect <- Dod.shapefile[dist.mat[i, 2],]
  temp.ext <- terra::extract(lc.rast, temp.vect)
  lc.list[[i]] <- temp.ext[,2]
}
save(lc.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/lc.Rdata")

## Coordinates - this is created well below where it is used above... switch order to enable dependency.
# loop over polygons.
for (i in seq_along(lc.list)) {
  temp.vect <- Dod.shapefile[dist.mat[i, 2],]
  temp.ext <- terra::extract(lc.rast, temp.vect, xy = TRUE)
  lc.list[[i]] <- temp.ext[,3:4] %>% `colnames<-`(c("lon", "lat"))
}
save(lc.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/coordinates.Rdata")
