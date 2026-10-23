# Next is DoD_data_prep and then DoD_observation?? and DoD_IC??
# EVEN BEFORE THIS, NEED TO EXTRACT DATA - SEE SCRIPTS UNDER 'FUNCTIONS' DIRECTORY
# STEP 3: DoD_data_prep.R
#   - Purpose: Merges the individual shapefiles into a single master layer. 
#              Generates the base `pecan.xml` configuration file, and downloads
#              raw environmental drivers (ERA5, SMAP, MODIS) for the selected sites.
#   - Key Output: `combined_layers.shp`, `pecan.xml`, and raw CSVs/NetCDFs for
#                 LAI, SMAP, and ERA5 weather data.
#
# STEP 4: DoD_data_IC.R
#   - Purpose: Generates the Initial Conditions (IC) for the SIPNET model.
#              Calculates spatial ensembles for starting state variables (AGB, 
#              SOC, LAI, SM) and writes them to site-specific NetCDF folders.
#   - Key Output: `pecanIC.xml` (the final model config) and ensemble lists 
#                 (e.g., `lai_list.Rdata`, `sm_list.Rdata`).
#
# STEP 5: DoD_observation.R
#   - Purpose: Builds the historical covariance matrices required by the SDA 
#              algorithm to calibrate parameters over time. It merges the raw 
#              downloads (Step 2) with the initial condition states (Step 3).
#   - Key Output: `obs.mean_noERA5_SM.Rdata` and `obs.cov_noERA5_SM.Rdata`
#                 (The final files ingested by the data assimilation model).

# Data Prep

knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(xts)
library(PEcAn.all)
library(purrr)
library(furrr)
library(lubridate)
library(nimble)
library(ncdf4)
library(PEcAnAssimSequential)
library(dplyr)
library(sp)
library(raster)
library(zoo)
library(ggplot2)
library(mnormt)
library(sjmisc)
library(stringr)
library(doParallel)
library(doSNOW)
library(Kendall)
library(lgarch)
library(parallel)
library(foreach)
library(terra)
library(sf)

# For ERA5 downloads
library(PEcAn.data.atmosphere)
library(ecmwfr)

# combine shapefiles from different LC.

# 1. Get the list of all shapefiles
# Define the directory for your site
site_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_lc"

# Automatically find all .shp files and get their full file paths
layers <- list.files(path = site_dir, 
                     pattern = "\\.shp$", 
                     full.names = TRUE)

# 2. Read all shapefiles into a list of sf objects
sf_list <- lapply(layers, st_read)

# 3. Combine them into a single sf object
combined_sf <- do.call(rbind, sf_list)

# 4. Save the combined data as a new shapefile
# Define your output directory
output_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all"

# Check if the directory exists; if not, create it
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Now write the combined shapefile
st_write(combined_sf, file.path(output_dir, "combined_layers.shp"))

# load all covariates associated with each site. And save them as an updated shapefile.
data.dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data"
load(file.path(data.dir, "lc.Rdata"))
load(file.path(data.dir, "agb.Rdata"))
load(file.path(data.dir, "dem.Rdata"))
load(file.path(data.dir, "climate.Rdata"))
load(file.path(data.dir, "soil.Rdata"))
load(file.path(data.dir, "stand_age.Rdata"))
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/cluster.Rdata")


# SAFELY LOAD COORDINATES - both coordinates and lc data store as lc.list from data prep...
# Eventually go back and change this in site_selection code
env_coord <- new.env()
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/site_selection_data/coordinates.Rdata", envir = env_coord)
coord_data <- env_coord$lc.list  # Rename it to something safe

tot.inds <- clusters %>% purrr::map(~.x[[2]]) %>% do.call(what = "c")
poly.id <- 1 # There's only one polygon since we're focused on a single installation but this could be useful for later
LC <- lc.list[[poly.id]]
AGB <- agb.list[[poly.id]]
DEM <- dem.list[[poly.id]]$dem.df$dem
CLIMATE <- climate.list[[poly.id]]
SOIL <- soil.list[[poly.id]]
STAND <- stand.age.list[[poly.id]]
LAT <- coord_data[[poly.id]]$lat
LON <- coord_data[[poly.id]]$lon

var.names <- c("lc", "agb", "dem", colnames(CLIMATE), colnames(SOIL), "stand_age", "lat", "lon")
dat <- cbind(LC, AGB, DEM, CLIMATE, SOIL, STAND, LAT, LON) %>% as.data.frame()
colnames(dat) <- var.names
dat$lc <- as.factor(dat$lc)
# handling LC.
non.veg.lc <- c("11", "12", "21", "22", "23", "24", "31")
#veg.inds <- which(!dat$lc %in% non.veg.lc & dat$agb > 0 & complete.cases(dat)) # have to remove complete cases bc of missing data leading to no data
veg.inds <- which(!dat$lc %in% non.veg.lc & dat$agb > 0)

dat <- dat[veg.inds,]
selected.df <- dat[tot.inds,]
nlcd_dict <- c(
  "11" = "Open Water",
  "12" = "Perennial Ice/Snow",
  "21" = "Developed, Open Space",
  "22" = "Developed, Low Intensity",
  "23" = "Developed, Medium Intensity",
  "24" = "Developed, High Intensity",
  "31" = "Barren Land",
  "41" = "Deciduous_Forest",
  "42" = "Evergreen_Forest",
  "43" = "Mixed_Forest",
  "52" = "Shrub",
  "71" = "Grassland",
  "81" = "Pasture/Hay",
  "82" = "Cultivated Crops",
  "90" = "Woody_Wetlands",
  "95" = "Emergent_Herbaceous_Wetlands"
)
selected.df$lc <- nlcd_dict[as.character(selected.df$lc)]
selected.pts <- st_read("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
selected.pts$lc <- NULL #remove duplicated fields.
selected.pts$agb <- NULL #remove duplicated fields.
selected.pts <- dplyr::bind_cols(selected.pts, selected.df)
unlink(list.files("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all", full.names = T))
st_write(selected.pts, "/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")

# Convert shape file to pecan settings.
pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
start_date <- "2001/01/01"
end_date <- "2024/12/31"

#setup working space
outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA"
SDA_run_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/run/"
SDA_out_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/out/"

ERA5_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/ERA5_2000_2024"
XML_out_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/pecan.xml"

pft_csv_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_pft.csv"
modis_phenology_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/leaf_phenology.csv"

#Obs_prep part
#AGB
AGB_indir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/AGB"
allow_download <- TRUE
AGB_export_csv <- TRUE
AGB_timestep <- list(unit="year", num=1)

#LAI
LAI_search_window <- 30
LAI_timestep <- list(unit="year", num=1)
LAI_export_csv <- TRUE
run_parallel <- TRUE
sd_threshold <- 20
upper_quantile <- 0.95
lower_quantile <- 0.05

#SMP
SMP_search_window <- 30
SMP_timestep <- list(unit="year", num=1)
SMP_export_csv <- TRUE
update_csv <- FALSE

#SoilC
SoilC_timestep <- list(unit="year", num=1)
SoilC_export_csv <- TRUE

#Obs Date
obs_start_date <- "2012-07-15"
obs_end_date <- "2021-07-15"
obs_outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation"
timestep <- list(unit="year", num=1)

#specify model binary
model_binary <- "/shared-ebs/software/models/sipnet/sipnet"

#specify host section
#NEED TO UPDATE THIS
host.flag <- "local"
if (host.flag == "remote") {
  #if we submit jobs through tunnel remotely.
  host = structure(list(
    name = "geo.bu.edu",
    usr = "zhangdc",
    folder = SDA_out_dir,
    prerun = "module load R/4.1.2",
    cdosetup = "module load cdo/2.0.6",
    qsub = "qsub -l h_rt=24:00:00 -q &apos;geo*&apos; -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash",
    qsub.jobid = "Your job ([0-9]+) .*",
    qstat = "qstat -j @JOBID@ || echo DONE",
    tunnel = "~/Tunnel/Tunnel",
    outdir = SDA_out_dir,
    rundir = SDA_run_dir
  ))
} else if (host.flag == "local") {
  host = structure(list(
    name = "localhost",
    folder = SDA_out_dir,
    outdir = SDA_out_dir,
    rundir = SDA_run_dir
  ))
} else if (host.flag == "rabbitmq") {
  host = structure(list(
    name = "localhost",
    rabbitmq = structure(list(
      uri = "amqp://guest:guest@pecan-rabbitmq:15672/%2F",
      queue = "SIPNET_r136",
      cp2cmd = "oc rsync @RUNDIR@ $(oc get pod -l app.kubernetes.io/name=pecan-model-sipnet-136 -o name):@RUNDIR@",
      cpfcmd = "/data/bin/oc rsync @OUTDIR@ $(/data/bin/oc get pod -l app=dongchen-sda -o name):@OUTDIR@"
    )),
    folder = SDA_out_dir,
    outdir = SDA_out_dir,
    rundir = SDA_run_dir
  ))
  model_binary <- "/usr/local/bin/sipnet.r136"
}
#Start building template 
# If pasting into R, run source because it's too big for terminal
#source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Kirtland_AFB/settings.R")
template <- PEcAn.settings::Settings(list(
  state.data.assimilation = structure(list(
    process.variance = TRUE,
    aqq.Init = 1,
    bqq.Init = 1,
    adjustment = TRUE,
    censored.data = FALSE,
    free.run = FALSE,
    FullYearNC = TRUE,
    NC.Overwrite = FALSE,
    NC.Prefix = "sipnet.out",
    q.type = "vector",
    by.site = FALSE,
    Localization.FUN = "Local.support",
    scalef = 0,
    chains = 1,
    data = structure(list(format_id = 1000000040, input.id = 1000013298)),
    state.variables = structure(list(
      #you could add more state variables here
      variable = structure(list(variable.name = "AbvGrndWood", unit = "MgC/ha", min_value = 0, max_value = 9999)),
      variable = structure(list(variable.name = "LAI", unit = "", min_value = 0, max_value = 9999)),
      variable = structure(list(variable.name = "SoilMoistFrac", unit = "", min_value = 0, max_value = 1)),
      variable = structure(list(variable.name = "TotSoilCarb", unit = "kg/m^2", min_value = 0, max_value = 9999))
    )),
    forecast.time.step = "year",
    start.date = start_date,
    end.date = end_date,
    
    Obs_Prep = structure(list(
      variables = structure(list(
        LandTrendr = structure(list(var_name = "AbvGrndWood", in_dir = AGB_indir, timestep = AGB_timestep, allow_download = allow_download, export_csv = AGB_export_csv)),
        GEDI = structure(list(var_name = "AbvGrndWood")),
        ERA5_LAI = structure(list(var_name = "LAI")),
        ERA5_SM = structure(list(var_name = "SoilMoistFrac")),
        MODIS = structure(list(var_name = "LAI", search_window = LAI_search_window, timestep = LAI_timestep, export_csv = LAI_export_csv, run_parallel = run_parallel,
                               sd_threshold = sd_threshold, boundary = structure(list(upper_quantile = upper_quantile, lower_quantile = lower_quantile)))),
        SMAP = structure(list(var_name = "SoilMoistFrac", search_window = SMP_search_window, timestep = SMP_timestep, export_csv = SMP_export_csv, update_csv = update_csv)),
        SoilGrids = structure(list(var_name = "TotSoilCarb", timestep = SoilC_timestep, export_csv = SoilC_export_csv))
      )),
      start.date = obs_start_date,
      end.date = obs_end_date,
      outdir = obs_outdir,
      timestep = timestep
    ))
  )),
  info = structure(list(
    notes = NULL, userid = "-1", username = "",
    date = "2017/12/06 21:19:33 +0000"
  )),
  outdir = outdir,
  rundir = SDA_run_dir,
  modeloutdir = SDA_out_dir,
  database = structure(list(
    bety = structure(
      list(user = "bety", password = "bety", host = "10.241.76.27",
           dbname = "bety", driver = "PostgreSQL", write = "FALSE"
      ))
  )),
  pfts = structure(list(
    #you could add more pfts you needed and make sure to modify the outdir of each pft!!!
    pft = structure(
      list(name = "temperate.deciduous.HPDA", 
           constants = structure(list(num = "1")), 
           posteriorid = "1000022311", 
           #outdir = "/shared/users-bucket/mhayden/obs_prep/Kirtland/PFT_Files/temperate.deciduous.HPDA",
           outdir = "/shared/users-bucket/mhayden/example_polygon/PFT_Files/temperate"
      )),
    
    pft = structure(
      list(name = "boreal.coniferous",
           outdir = "/shared/users-bucket/mhayden/example_polygon/PFT_Files/conifer"
      )),
    
    pft = structure(
      list(name = "semiarid.grassland_HPDA",
           constants = structure(list(num = "1")),
           posteriorid = "1000016525",
           outdir = "/shared/users-bucket/mhayden/example_polygon/PFT_Files/grass"
      ))
  )),
  meta.analysis = structure(list(
    iter = "3000", random.effects = FALSE
  )),
  ensemble = structure(list(size = 100, variable = "NPP", 
                            samplingspace = structure(list(
                              parameters = structure(list(method = "sampling")),
                              soil_physics = structure(list(method = "sampling")),
                              met = structure(list(method = "sampling"))
                            ))
  )),
  model = structure(list(id = "1000000022",
                         type = "SIPNET",
                         revision = "ssr",
                         delete.raw = FALSE,
                         binary = model_binary,
                         jobtemplate = "~/sipnet_geo.job"
  )),
  host = host,
  settings.info = structure(list(
    deprecated.settings.fixed = TRUE,
    settings.updated = TRUE,
    checked = TRUE
  )),
  run = structure(list(
    inputs = structure(list(
      met = structure(list(
        source = "ERA5", 
        output = "SIPNET",
        id = "",
        path = ERA5_dir
      )),
      pft.site = structure(list(path = pft_csv_dir)),
      leaf_phenology = structure(list(path = modis_phenology_dir))
    ))
  ))
))
multiRunSettings <- PEcAn.settings::createMultiSiteSettings(templateSettings = template, siteIds = seq_along(pts))
PEcAn.settings::write.settings(multiRunSettings, outputfile = "pecan.xml")

settings <- PEcAn.settings::read.settings("/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/pecan.xml")
#write Lat and Lon into the settings
coords <- terra::crds(pts)
for (i in seq_along(pts)) {
  temp_ID <- settings[[i]]$run$site$id
  settings[[i]]$run$site$lat <- coords[i, "y"]
  settings[[i]]$run$site$lon <- coords[i, "x"]
  settings[[i]]$run$site$name <- temp_ID
}
PEcAn.settings::write.settings(settings, outputfile = "pecan.xml")

# Prepare PFT.
pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
pft <- data.frame(site = seq_along(pts), pft = NA)
for (i in seq_along(pts)) {
  if (pts$lc[i] %in% c("Shrub", "Mixed_Forest", "Deciduous_Forest")) {
    pft$pft[i] <- "temperate.deciduous.HPDA"
  } else if (pts$lc[i] %in% c("Evergreen_Forest", "Woody_Wetlands")) {
    pft$pft[i] <- "boreal.coniferous"
  } else if (pts$lc[i] %in% c("Emergent_Herbaceous_Wetlands", "Grassland")) {
    pft$pft[i] <- "semiarid.grassland_HPDA"
  }
}
write.csv(pft, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/site_pft.csv", row.names = F)

# Prepare ERA5.

# First need to download if not already downloaded
# Code to extract ERA5 weather data for observations and initial conditions

# Download ERA5
raw_ERA5_dir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/ERA5"
dir.create(raw_ERA5_dir, showWarnings = FALSE)

# Set keyring backend for headless cluster node
Sys.setenv(R_KEYRING_BACKEND = "env")

# Download the raw ERA5 weather drivers (Temp, Precip, Wind, Radiation, etc.)
# If the function is in your script, remove 'PEcAn.data.atmosphere::'
buffer <- 0.25 # because the next extraction step requires more than one pixel...
buffered_extent <- c(min(LON) - buffer, max(LON) + buffer, min(LAT) - buffer, max(LAT) + buffer)

PEcAn.data.atmosphere::download.ERA5_cds(
  outfolder = raw_ERA5_dir,
  extent = buffered_extent, 
  start_date = "2000-01-01",
  end_date = "2024-12-31",
  variables = c("2m_temperature",
                "surface_pressure",
                "2m_dewpoint_temperature",
                "total_precipitation",
                "10m_u_component_of_wind",
                "10m_v_component_of_wind",
                "surface_solar_radiation_downwards",
                "surface_thermal_radiation_downwards"),
  user = "meghan.t.hayden@nasa.gov",
  key = "e18bd11d-f60a-4d39-a2be-94a4cf0d5d64",
  product_type = "reanalysis",
  time = c("00:00", "12:00")
)
# downloading results in zip files, need .nc for next step

zip_files <- list.files(raw_ERA5_dir, pattern = "\\.zip$", full.names = TRUE)

for (z in zip_files) {
  # What the final file should be named
  expected_nc <- sub("\\.zip$", ".nc", z)
  
  # See what the files inside the zip are actually named
  inside_files <- unzip(z, list = TRUE)$Name
  
  if (length(inside_files) != 2) {
    warning(sprintf("Expected 2 files in %s, but found %d. Skipping...", basename(z), length(inside_files)))
    next
  }
  
  # Extract the files
  unzip(z, exdir = raw_ERA5_dir)
  
  # Get full paths of the two extracted files
  file1 <- file.path(raw_ERA5_dir, inside_files[1])
  file2 <- file.path(raw_ERA5_dir, inside_files[2])
  
  cat(sprintf("Merging files into %s using CDO...\n", basename(expected_nc)))
  
  # Construct the CDO command
  # -O overwrites existing files
  # -f nc forces the output to be strict NetCDF format
  cdo_cmd <- sprintf("cdo -O -f nc merge %s %s %s", file1, file2, expected_nc)
  
  # Run CDO directly from R
  exit_status <- system(cdo_cmd)
  
  # Only delete files if the merge was successful (exit status 0)
  if (exit_status == 0) {
    unlink(c(file1, file2))
    unlink(z)
    cat(sprintf("Successfully created: %s\n\n", basename(expected_nc)))
  } else {
    warning(sprintf("Failed to merge files for %s. Ensure CDO is loaded on your cluster ('module load cdo').", basename(z)))
  }
}

cat("All files unzipped and merged successfully! The data is now ready for ERA5_met_process.\n")

settings <- PEcAn.settings::read.settings("/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/pecan.xml")
dir.create("/shared/users-bucket/mhayden/obs_prep/Kirtland/ERA5_2000_2024")
# settings$state.data.assimilation$end.date <- "2002/12/31"
# this won't work on own because the extraction is only one pixel, re-download with buffer
out.paths <- PEcAn.data.atmosphere::ERA5_met_process(settings = settings, 
                                                     in.path = "/shared/users-bucket/mhayden/obs_prep/Kirtland/ERA5", 
                                                     out.path = "/shared/users-bucket/mhayden/obs_prep/Kirtland/ERA5_2000_2024", 
                                                     write.db = F, write = T, ncores = 10)

# Prepare phenology.
site_info <- settings %>% 
  purrr::map(~.x[['run']] ) %>% 
  purrr::map('site')%>% 
  purrr::map(function(site.list){
    #conversion from string to number
    site.list$lat <- as.numeric(site.list$lat)
    site.list$lon <- as.numeric(site.list$lon)
    list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
  })%>% 
  dplyr::bind_rows() %>% 
  as.list()
phenology.path <- PEcAn.data.remote::extract_phenology_MODIS(site_info = site_info, 
                                                             start_date = "2001-01-01", 
                                                             end_date = "2024-12-31", 
                                                             outdir = "/shared/users-bucket/mhayden/obs_prep/Kirtland", 
                                                             run_parallel = T, ncores = 10)

# soil texture.
# need to transfer this R code over...
source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Fort_Hunter_Liggett/functions/soilgrids_texture_extract.R")
outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture"
if (!dir.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}

# the soil texture files cover all of NA - stored in SDA_disturbance folder
data_paths <- list(
  list(
    url = NULL,
    local = "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/soil_texture/sand/",
    save_path = "/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/sand_percent.rds"),
  list(
    url = NULL,
    local = "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/soil_texture/clay/",
    save_path = "/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/clay_percent.rds"),
  list(
    url = NULL,
    local = "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/soil_texture/silt/",
    save_path = "/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/silt_percent.rds"))

out <- soilgrids_texture_extraction(site_info, outdir, "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/soil_texture")
dir.create("/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/soil_texture_ensemble")

sand <- readRDS("/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/sand_percent.rds") #sand fraction in percentage
clay <- readRDS("/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/clay_percent.rds") #clay fraction in percentage
silt <- readRDS("/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/silt_percent.rds") #silt fraction in percentage

source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Fort_Hunter_Liggett/functions/fix_soil.R") # had to rewrite this because pecan script wasn't working, temporary fix
#dat <- PEcAn.data.land:::soil_params_ensemble_soilgrids(settings,
#                                                        sand,
#                                                        clay,
#                                                        silt,
#                                                        "/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/soil_texture_ensemble")

dat <- soil_params_ensemble_soilgrids(settings,
                                                        sand,
                                                        clay,
                                                        silt,
                                                        "/shared/users-bucket/mhayden/obs_prep/Kirtland/soil_texture/soil_texture_ensemble")


# Prepare AGB initial condition and observation.
## AGB initial condition.
# Start here tomorrow and look at obs_prep_opt.R
# Need to crop the data on the BU cluster and then transfer, instead of large files
selected.pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
agb.mean <- terra::rast("/shared/users-bucket/mhayden/obs_prep/Kirtland/AGB/biomassfiaald_2000_median.tif")
agb.sd <- terra::rast("/shared/users-bucket/mhayden/obs_prep/Kirtland/AGB/biomassfiaald_2000_stdv.tif")
selected.pts <- terra::project(selected.pts, agb.mean)
# extract.
agb.mean.pts <- terra::extract(agb.mean, selected.pts)[,2]
agb.std.pts <- terra::extract(agb.sd, selected.pts)[,2]
# zero-truncated normal sampling.
# Set a seed for reproducibility
set.seed(123)

# Sample 1 value per point using your extracted vectors
# lower = 0 enforces the zero-truncation
agb.sampled <- c()
for (i in seq_along(selected.pts)) {
  agb.sampled <- rbind(agb.sampled, msm::rtnorm(n = 100, 
                       mean = agb.mean.pts[i], 
                       sd = agb.std.pts[i], 
                       lower = 0))
}
dir.create("/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data", recursive = TRUE)
save(agb.sampled, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/agb_ic.Rdata")

## AGB observation (from 2001 to 2023).

# find the agb mean and std time series files.
mean.files <- c(file.path("/shared/users-bucket/mhayden/obs_prep/Kirtland/AGB/", paste0("biomassfiaald_", 2001:2023, "_median.tif")))
std.files <- c(file.path("/shared/users-bucket/mhayden/obs_prep/Kirtland/AGB/", paste0("biomassfiaald_", 2001:2023, "_stdv.tif")))

# Skipping random forest for now - if wanted go back to old dongchen code
mean.stack <- terra::rast(mean.files[1:17]) # Add back RF for the stdev file for 2018 onward?
std.stack <- terra::rast(std.files[1:17])
agb.mean <- terra::extract(mean.stack, selected.pts)[, -1] # drop ID column
agb.std <- terra::extract(std.stack, selected.pts)[, -1]
colnames(agb.mean) <- paste0("agb_mean_", 2001:2017)
colnames(agb.std) <- paste0("agb_std_", 2001:2017)

dir.create("/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/", 
           recursive = TRUE, 
           showWarnings = FALSE)
save(list = c("agb.mean", "agb.std"), file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/agb_mean_std.Rdata")


# Prepare LAI initial condition and observation.
## LAI initial condition.

source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Fort_Hunter_Liggett/functions/NASA_DAAC_download.R")
# MODIS CSV prep function.
MODIS.CSV.prep <- function(pts, extent, from, to, download.outdir, csv.outdir) {
  # load previous CSV file.
  if (file.exists(file.path(csv.outdir, "LAI.csv"))) {
    previous.csv <- read.csv(file.path(csv.outdir, "LAI.csv"), 
                             colClasses = c("character", rep("numeric", 5), "character"))
  } else {
    previous.csv <- NULL
  }
  # setup the foreach parallel computation.
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  # reproject site locations to MODIS projection.
  site.ids <- seq_along(pts) %>% as.character()
  site.locs <- terra::crds(pts)
  pts <- data.frame(lon = site.locs[,1], lat = site.locs[,2])
  sp::coordinates(pts) <- ~lon+lat
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  pts.reproj <- sp::spTransform(pts, "+proj=sinu +a=6371007.181 +b=6371007.181 +units=m")
  coords.reproj <- sp::coordinates(pts.reproj) %>% `colnames<-`(c("x", "y"))
  # download data.
  metadata <- NASA_DAAC_download(ul_lat = extent[4], 
                                 ul_lon = extent[1], 
                                 lr_lat = extent[3], 
                                 lr_lon = extent[2], 
                                 from = from,
                                 to = to, 
                                 just_path = F,
                                 outdir = download.outdir,
                                 doi = "10.5067/MODIS/MCD15A3H.061",
                                 ncore = parallel::detectCores()-1,
                                 credential.folder = "~")
  # grab file paths for downloaded hdf files.
  modis.out <- list.files(download.outdir, full.names = T, pattern = "*.hdf")
  # grab id for each file.
  ids <- basename(modis.out)
  # split string.
  splits <- strsplit(x = ids, split = ".", fixed = T)
  # collect attributes table (date, ih, and iv).
  dates <- ivs <- ihs <- c()
  for (i in seq_along(ids)) {
    temp <- splits[[i]]
    # calculate dates based on year and day of year.
    y.doy <- substr(temp[[2]], 2, 8)
    year <- substr(y.doy, 1, 4)
    doy <- substr(y.doy, 5, 7)
    dates <- c(dates, as.character(as.Date(paste(year, doy), format="%Y %j")))
    # extract tile id.
    ihs <- c(ihs, substr(temp[[3]], 2, 3))
    ivs <- c(ivs, substr(temp[[3]], 5, 6))
  }
  # create data frame of extents for MODIS tiles.
  # record progress.
  message("\nCreating information table for downloaded MODIS tiles.")
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(min=1, max=length(modis.out), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  MODIS.tiles.extents <- foreach::foreach(
    tile = modis.out,
    .packages=c("terra", "Kendall"),
    .options.snow=opts
  ) %dopar% {
    return(as.vector(terra::ext(terra::rast(tile))))
  } %>% dplyr::bind_rows()
  MODIS.tiles.info <- cbind(dates, ihs, ivs, MODIS.tiles.extents)
  # filter dates.
  unique.dates <- unique(MODIS.tiles.info$dates)
  unique.dates <- unique.dates[which(as.Date(unique.dates) >= from &
                                     as.Date(unique.dates) <= to)]
  # find inds of file paths that match each point.
  message("\nFinding indexes of file paths that match each point.")
  pb <- utils::txtProgressBar(min=1, max=length(pts), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  tile.ids <- foreach::foreach(
    i = as.numeric(site.ids),
    .packages="Kendall",
    .options.snow=opts
  ) %dopar% {
    # filter tile position ids based on lat/lon range.
    inds <- which(as.Date(MODIS.tiles.info$dates) >= from & 
                    as.Date(MODIS.tiles.info$dates) <= to & 
                    coords.reproj[i, "x"] > MODIS.tiles.info$xmin & 
                    coords.reproj[i, "x"] < MODIS.tiles.info$xmax & 
                    coords.reproj[i, "y"] > MODIS.tiles.info$ymin &
                    coords.reproj[i, "y"] < MODIS.tiles.info$ymax)
    names(inds) <- rep(i,length(inds))
    return(inds)
  } %>% unlist
  # extract MODIS products.
  # loop over tiles.
  message("\nExtracting MODIS datasets by points.")
  unique.tile.ids <- sort(unique(tile.ids)) 
  pb <- utils::txtProgressBar(min=1, max=length(unique.tile.ids), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  outputs <- foreach::foreach(
    i = unique.tile.ids,
    .packages=c("purrr", "Kendall"),
    .options.snow=opts
  ) %dopar% {
    temp.tile <- terra::rast(modis.out[i])
    points.inds <- as.numeric(names(tile.ids)[which(tile.ids == i)])
    temp.res <- terra::extract(temp.tile, data.frame(x = coords.reproj[points.inds, "x"],
                                                     y = coords.reproj[points.inds, "y"]))
    # convert QC flag.
    qc <- temp.res[,4] %>% purrr::map(function(v){
      qc_flag <- intToBits(as.integer(v)) # NB big-endian (ones place first)
      qc_flag <- as.integer(rev(utils::head(qc_flag, 3))) # now ones place last
      paste(qc_flag, collapse = "")
    }) %>% unlist
    # write into table.
    temp.res <- data.frame(date = rep(MODIS.tiles.info$dates[i], length(points.inds)),
                           site_id = points.inds, 
                           lat = site.locs[points.inds, "y"],
                           lon = site.locs[points.inds, "x"],
                           lai = temp.res[,3],
                           sd = temp.res[,7],
                           qc = qc)
    return(temp.res)
  } %>% dplyr::bind_rows()
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # combine with previous CSV file.
  if (!is.null(previous.csv)) {
    outputs <- rbind(previous.csv, outputs)
    outputs <- outputs[!duplicated(outputs),]
  }
  # filter by QC band.
  outputs <- outputs[which(outputs$qc %in% c("000", "001")),]
  # write into CSV file.
  write.csv(outputs, file = file.path(csv.outdir, "LAI.csv"), row.names = F)
  # delete downloaded files.
  unlink(list.files(download.outdir, full.names = T), recursive = T)
  return(outputs)
} # this is dongchen's function
# this is the 'optimized version'
source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Fort_Hunter_Liggett/functions/modis_prep.R")

extent <- terra::ext(terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/selected_polygons/selected_polygons.shp")[,3])
pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/selected_polygons/selected_polygons.shp")

from <- seq(as.Date("2002-06-01"), as.Date("2024-06-01"), by = "1 year")
to <- seq(as.Date("2002-08-31"), as.Date("2024-08-31"), by = "1 year")
download.outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/LAI/download"
csv.outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/LAI"
# setwd(csv.outdir)
for (i in seq_along(from)) {
  print(from[i])
  outputs <- MODIS.CSV.prep(pts = pts, extent = extent, from = from[i], to = to[i], download.outdir = download.outdir, csv.outdir = csv.outdir)
}

# SM.
## SMAP.

# source("/projectnb/dietzelab/dongchen/anchorSites/Obs/SMAP_download/NASA_DAAC_Download.R")
# MODIS CSV prep function.
library(foreach)
library(purrr)
# SMAP.
SMAP.CSV.Prep <- function(site.ids, site.locs, extent, from, to, download.outdir, csv.outdir) {
  # SMAP CRS, EPSG:6933.
  smap.crs <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  # load previous CSV file.
  if (file.exists(file.path(csv.outdir, "SMAP.csv"))) {
    previous.csv <- read.csv(file.path(csv.outdir, "SMAP.csv"), 
                             colClasses = c("character", rep("numeric", 5)))
  } else {
    previous.csv <- NULL
  }
  # reproject site locations to MODIS projection.
  pts <- data.frame(lon = site.locs[,1], lat = site.locs[,2])
  sp::coordinates(pts) <- ~lon+lat
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  pts.reproj <- sp::spTransform(pts, smap.crs)
  coords.reproj <- sp::coordinates(pts.reproj) %>% `colnames<-`(c("x", "y"))
  # download data.
  metadata <- PEcAn.data.remote::NASA_DAAC_download(ul_lat = extent[4], 
                                 ul_lon = extent[1], 
                                 lr_lat = extent[3], 
                                 lr_lon = extent[2], 
                                 from = from,
                                 to = to, 
                                 just_path = F,
                                 outdir = download.outdir,
                                 credential_path = "~/.netrc",
                                 doi = "10.5067/02LGW4DGJYRX",
                                 ncore = parallel::detectCores()-1)
  smap.out <- metadata
  file <- smap.out[1] # select the first file.
  # grab smap extents, it's from the ArcGIS report using the SMAP H5 file.
  smap.ext <- c(-17363027.292480, 17367529.945160, -7319045.227051, 7310037.171387) %>% terra::ext()
  # convert h5 file to raster.
  smap <- terra::rast(file)
  # add extent and crs to the raster.
  terra::ext(smap) <- smap.ext#terra::ext(pr_extent)
  terra::crs(smap) <- smap.crs
  # extract values for smp and std.
  smp <- terra::extract(smap[["sm_rootzone_analysis"]], coords.reproj) * 100
  std <- terra::extract(smap[["sm_rootzone_analysis_ensstd"]], coords.reproj) * 100
  
  # terra::writeRaster(smp, "/projectnb/dietzelab/dongchen/anchorSites/Obs/SMAP_download/download/smap.tif")
  # construct final data frame.
  outputs <- c()
  for (i in seq_along(site.ids)) {
    outputs <- rbind(outputs, data.frame(date = as.character(from),
                                         site_id = i, 
                                         lat = site.locs[i, "x"],
                                         lon = site.locs[i, "y"],
                                         smp = smp[i,],
                                         sd = std[i,]))
  }
  # remove NAs.
  outputs <- outputs[which(!is.na(outputs$smp)),]
  # combine with previous CSV file.
  if (!is.null(previous.csv)) {
    outputs <- rbind(previous.csv, outputs)
    outputs <- outputs[!duplicated(outputs),]
  }
  # write into CSV file.
  write.csv(outputs, file = file.path(csv.outdir, "SMAP.csv"), row.names = F)
  # delete downloaded files.
  unlink(list.files(download.outdir, full.names = T), recursive = T)
  return(outputs)
}

base.map <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/downscale/base_map.tiff")
extent <- terra::ext(base.map)
pts <- terra::vect("/projectnb/dietzelab/dongchen/SDA_disturbance/example_polygon/shp_all/combined_layers.shp")
site.locs <- terra::crds(pts)
from <- seq(as.Date("2015-07-15"), as.Date("2024-07-15"), by = "1 year")
to <- seq(as.Date("2015-07-16"), as.Date("2024-07-16"), by = "1 year")
download.outdir <- "/projectnb/dietzelab/dongchen/SDA_disturbance/example_polygon/observation/SM/SMAP/download"
csv.outdir <- "/projectnb/dietzelab/dongchen/SDA_disturbance/example_polygon/observation/SM/SMAP"
for (i in seq_along(from)) {
  print(paste0("\n", from[i]))
  outputs <- SMAP.CSV.Prep(as.character(seq_along(pts)), site.locs, extent = extent, from = from[i], to = to[i], download.outdir = download.outdir, csv.outdir = csv.outdir)
}

## ERA5 soil moisture and LAI.
library(ecmwfr)
area <- round(c(36.5, -122, 35.5, -121), 2)
years <- 2000:2024
outfolder <- "/projectnb/dietzelab/dongchen/SDA_disturbance/example_polygon/observation/SM/ERA5"
ecmwfr::wf_set_key(user = "zhangdc@bu.edu", key = "a9071675-dd49-443b-8f90-cf8da32d3821")
# loop over years.
nc.paths <- c()
for (y in years) {
  fname <- file.path(outfolder, paste0("ERA5_", y, ".nc"))

  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = list("ensemble_members"),
    data_format = 'netcdf',
    download_format = "unarchived",
    day = list("15"),
    time = list("00:00", "03:00", "06:00",
      "09:00", "12:00", "15:00",
      "18:00", "21:00"),
    month = list("07"),
    year = list(as.character(y)),
    area = area,
    variable = list("volumetric_soil_water_layer_1",
        "volumetric_soil_water_layer_2",
        "volumetric_soil_water_layer_3",
        "volumetric_soil_water_layer_4",
        "leaf_area_index_high_vegetation",
        "leaf_area_index_low_vegetation"),
    target = basename(fname)
  )
    
  # Submit request using ecmwfr
  ecmwfr::wf_request(
      request = request,
      user = "zhangdc@bu.edu",
      path = outfolder,
      time_out = 36000
    )
}

# SOC
library(dplyr)
library(sf)
library(terra)
library(dplyr)

mukey.rast <- terra::rast("/projectnb/dietzelab/dongchen/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/muraster_30m_resample.tif")

# Load the attribute table from the GeoPackage (once extraction is 100%)
gpkg_path <- "/projectnb/dietzelab/dongchen/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/2025_ALL_SSURGO.gpkg"
attributes <- st_read(gpkg_path, layer = "chorizon") # 'chorizon' contains the physical properties

# 1. Load the component table from your GPKG
components <- st_read(gpkg_path, layer = "component")
# If they do, create the column manually:
components$cokey <- rownames(components)
# This forces the internal rowid (cokey) to be readable
components <- st_read(gpkg_path, query = "SELECT rowid AS cokey, * FROM component")
# merge the cockey field to the attribute table.
attributes_with_mukey <- merge(
  attributes, 
  components[, c("mukey", "cokey")], 
  by = "cokey"
)

# STEP 1: Calculate SOC per individual component (cokey) capped at 200cm
soc_by_component <- attributes_with_mukey %>%
  select(
    mukey, cokey, hzname, hzdept_r, hzdepb_r,
    om_l, om_r, om_h, 
    dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, 
    sieveno10_r
  ) %>%
  filter(!is.na(hzdept_r) & !is.na(hzdepb_r)) %>%
  filter(hzdept_r < 200) %>% # Increased to 200cm
  mutate(
    hzdepb_r = ifelse(hzdepb_r > 200, 200, hzdepb_r), # Capped at 200cm
    thick = hzdepb_r - hzdept_r
  ) %>%
  # Handle missing data safely
  mutate(
    om_l = ifelse(is.na(om_l), 0, om_l),
    om_r = ifelse(is.na(om_r), 0, om_r),
    om_h = ifelse(is.na(om_h), 0, om_h),
    dbthirdbar_l = ifelse(is.na(dbthirdbar_l), 1.1, dbthirdbar_l),
    dbthirdbar_r = ifelse(is.na(dbthirdbar_r), 1.3, dbthirdbar_r),
    dbthirdbar_h = ifelse(is.na(dbthirdbar_h), 1.5, dbthirdbar_h),
    frag = ifelse(is.na(sieveno10_r), 0, 100 - sieveno10_r)
  ) %>%
  # Calculate Low, Rep, and High SOC for each horizon layer
  mutate(
    soc_hz_low  = (om_l * 0.58 / 100) * dbthirdbar_l * ((100 - frag) / 100) * thick * 10,
    soc_hz_mean = (om_r * 0.58 / 100) * dbthirdbar_r * ((100 - frag) / 100) * thick * 10,
    soc_hz_high = (om_h * 0.58 / 100) * dbthirdbar_h * ((100 - frag) / 100) * thick * 10
  ) %>%
  # Convert the low/high range into an estimated standard deviation for the layer
  mutate(soc_hz_sd = (soc_hz_high - soc_hz_low) / 4) %>%
  # Aggregate vertically by component profile
  group_by(mukey, cokey) %>%
  summarize(
    comp_soc_mean = sum(soc_hz_mean, na.rm = TRUE),
    comp_soc_sd   = sqrt(sum(soc_hz_sd^2, na.rm = TRUE)), 
    comp_depth    = sum(thick),
    .groups = "drop"
  )

# STEP 2: Join with component table and compute the final mapunit spatial properties
final_mukey_soc <- soc_by_component %>%
  left_join(select(components, cokey, comppct_r), by = "cokey") %>%
  mutate(comppct_r = ifelse(is.na(comppct_r), 0, comppct_r)) %>%
  group_by(mukey) %>%
  summarize(
    total_soc_mean_kg_m2 = sum(comp_soc_mean * (comppct_r / 100), na.rm = TRUE),
    total_soc_sd_kg_m2   = sqrt(sum((comp_soc_sd^2) * (comppct_r / 100), na.rm = TRUE)),
    total_depth_checked  = max(comp_depth, na.rm = TRUE) # Will now max out at 200cm
  )

pts <- terra::vect("/projectnb/dietzelab/dongchen/SDA_disturbance/example_polygon/shp_all/combined_layers.shp")

# 1. Ensure Coordinate Systems Match
if (terra::crs(pts) != terra::crs(mukey.rast)) {
  pts <- terra::project(pts, terra::crs(mukey.rast))
}

# 2. Extract Mapunit raster values at point locations
# Setting ID = FALSE prevents terra from adding an unneeded "ID" column
extracted_mukeys <- terra::extract(mukey.rast, pts)
rownames(final_mukey_soc) <- final_mukey_soc$mukey
soc <- c()
for (i in seq_along(extracted_mukeys$MUKEY)) {
  soc <- rbind(soc, final_mukey_soc[as.character(extracted_mukeys$MUKEY[i]),])
}
save(soc, file = "/projectnb/dietzelab/dongchen/SDA_disturbance/example_polygon/observation/SOC/soc_ssurgo.Rdata")
# # check with soilgrids.
# soc.soilgrids <- terra::rast("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/SOC/SOC.tif")
# soc.pts <- terra::extract(soc.soilgrids, pts)
# 
# plot(soc$total_soc_mean_kg_m2, soc.pts$mean/10)
