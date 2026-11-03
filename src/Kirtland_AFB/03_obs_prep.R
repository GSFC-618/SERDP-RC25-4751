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

# most of this came from data prep, combine it with IC and Observation scripts since some of that is already in here??
# confusing how things are organized so far

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
# this is the 'optimized version', not dongchen's original
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
# clear broken files if needed
# unlink("/shared/users-bucket/mhayden/obs_prep/Kirtland/LAI/download/*")

# SM.
## SMAP.
# This is not working - skip for now...

# code test
i <- 1
from <- from[1]
to <- to[1]

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
  metadata <- PEcAn.data.remote::NASA_DAAC_download(ul_lat = as.numeric(extent[4]), # need to convert to numeric or hardcode; won't recognize otherwise.
                                 ul_lon = as.numeric(extent[1]), 
                                 lr_lat = as.numeric(extent[3]), 
                                 lr_lon = as.numeric(extent[2]), 
                                 from = from,
                                 to = to, 
                                 just_path = F,
                                 outdir = download.outdir,
                                 credential_path = "~/.netrc",
                                 doi = "10.5067/02LGW4DGJYRX",
                                 ncore = 1)
  print("Finished downloading files")                               
  smap.out <- metadata

  if (length(smap.out) == 0 || is.na(smap.out[1])) {
    warning(paste("No files downloaded for date:", from))
    return(NULL) # Skip this date safely and move to the next iteration
  }
  
  print("Opening first file")
  file <- smap.out[1] # select the first file.
  
  # grab smap extents, it's from the ArcGIS report using the SMAP H5 file.
  smap.ext <- c(-17363027.292480, 17367529.945160, -7319045.227051, 7310037.171387) %>% terra::ext()
  
  # 1. Format the file paths as specific GDAL Subdatasets so terra doesn't scan the whole file
  smp_path <- paste0('HDF5:"', file, '"://Analysis_Data/sm_rootzone_analysis')
  std_path <- paste0('HDF5:"', file, '"://Analysis_Data/sm_rootzone_analysis_ensstd')
  
  # 2. Open just those specific variables
  smp_rast <- terra::rast(smp_path)
  std_rast <- terra::rast(std_path)
  
  # 3. Add extent and crs
  terra::ext(smp_rast) <- smap.ext
  terra::crs(smp_rast) <- smap.crs
  
  terra::ext(std_rast) <- smap.ext
  terra::crs(std_rast) <- smap.crs

  # Needs to be flipped to right orientation
  smp_rast <- terra::flip(smp_rast, direction = "vertical")
  std_rast <- terra::flip(std_rast, direction = "vertical")
  
  # -9999 means NA
  terra::NAflag(smp_rast) <- -9999
  terra::NAflag(std_rast) <- -9999
  
  # 4. Extract values 
  print("Extracting values from rasters")
  smp <- terra::extract(smp_rast, coords.reproj) * 100
  std <- terra::extract(std_rast, coords.reproj) * 100
  
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
#source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Fort_Hunter_Liggett/functions/smap_prep.R")

pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
extent <- terra::ext(terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/selected_polygons/selected_polygons.shp")[,3])
site.locs <- terra::crds(pts)

from <- seq(as.Date("2015-07-15"), as.Date("2024-07-15"), by = "1 year")
to <- seq(as.Date("2015-07-16"), as.Date("2024-07-16"), by = "1 year")
csv.outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/SMAP"
download.outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/SMAP/download"
dir.create(download.outdir, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(from)) {
  print(paste0("\n", from[i]))
  outputs <- SMAP.CSV.Prep(as.character(seq_along(pts)), site.locs, extent = extent, from = from[i], to = to[i], download.outdir = download.outdir, csv.outdir = csv.outdir)
}
# clear broken files if needed
#unlink("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/download/*")

## ERA5 soil moisture and LAI.

area <- round(c(35.07, -106.62, 34.94, -106.36), 2)
years <- 2012:2024
outfolder <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/ERA5"
dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
# no password, just press ENTER
ecmwfr::wf_set_key(user = "meghan.t.hayden@nasa.gov", key = "e18bd11d-f60a-4d39-a2be-94a4cf0d5d64")
# loop over years.
nc.paths <- c()
for (y in years) {
  fname <- file.path(outfolder, paste0("ERA5_", y, ".nc"))

  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = list("ensemble_members"),
    data_format = 'netcdf',
    format = "netcdf",
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
      user = "meghan.t.hayden@nasa.gov",
      path = outfolder,
      time_out = 36000
    )
}

# SOC

# First AI optimized version

# SOC SSURGO Processing
mukey.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/muraster_30m_resample.tif")
gpkg_path <- "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/ALL_SSURGO_10_01_2025_gpkg/2025_ALL_SSURGO.gpkg"
attributes <- st_read(gpkg_path, layer = "chorizon")
components <- st_read(gpkg_path, query = "SELECT rowid AS cokey, * FROM component")

attributes_with_mukey <- merge(attributes, components[, c("mukey", "cokey")], by = "cokey")

soc_by_component <- attributes_with_mukey %>%
  dplyr::select(mukey, cokey, hzname, hzdept_r, hzdepb_r, om_l, om_r, om_h, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, sieveno10_r) %>%
  dplyr::filter(!is.na(hzdept_r) & !is.na(hzdepb_r), hzdept_r < 200) %>%
  dplyr::mutate(hzdepb_r = ifelse(hzdepb_r > 200, 200, hzdepb_r), thick = hzdepb_r - hzdept_r) %>%
  dplyr::mutate(om_l = coalesce(om_l, 0), om_r = coalesce(om_r, 0), om_h = coalesce(om_h, 0),
         dbthirdbar_l = coalesce(dbthirdbar_l, 1.1), dbthirdbar_r = coalesce(dbthirdbar_r, 1.3),
         dbthirdbar_h = coalesce(dbthirdbar_h, 1.5), frag = coalesce(100 - sieveno10_r, 100)) %>%
  mutate(soc_hz_low  = (om_l * 0.58 / 100) * dbthirdbar_l * (frag / 100) * thick * 10,
         soc_hz_mean = (om_r * 0.58 / 100) * dbthirdbar_r * (frag / 100) * thick * 10,
         soc_hz_high = (om_h * 0.58 / 100) * dbthirdbar_h * (frag / 100) * thick * 10) %>%
  dplyr::mutate(soc_hz_sd = (soc_hz_high - soc_hz_low) / 4) %>%
  dplyr::group_by(mukey, cokey) %>%
  dplyr::summarize(comp_soc_mean = sum(soc_hz_mean, na.rm = TRUE),
            comp_soc_sd   = sqrt(sum(soc_hz_sd^2, na.rm = TRUE)), 
            comp_depth    = sum(thick), .groups = "drop")

final_mukey_soc <- soc_by_component %>%
  dplyr::left_join(dplyr::select(components, cokey, comppct_r), by = "cokey") %>%
  dplyr::mutate(comppct_r = coalesce(comppct_r, 0)) %>%
  dplyr::group_by(mukey) %>%
  dplyr::summarize(total_soc_mean_kg_m2 = sum(comp_soc_mean * (comppct_r / 100), na.rm = TRUE),
            total_soc_sd_kg_m2   = sqrt(sum((comp_soc_sd^2) * (comppct_r / 100), na.rm = TRUE)),
            total_depth_checked  = max(comp_depth, na.rm = TRUE))

if (terra::crs(pts) != terra::crs(mukey.rast)) pts <- terra::project(pts, terra::crs(mukey.rast))

### OPTIMIZATION 6: Erased the awful rbind loop. Vectorized match takes 0.01 seconds ###
extracted_mukeys <- terra::extract(mukey.rast, pts)

names(extracted_mukeys)[2] <- "mukey"

extracted_mukeys$mukey <- as.numeric(extracted_mukeys$mukey)
final_mukey_soc$mukey <- as.numeric(final_mukey_soc$mukey)

soc <- extracted_mukeys %>%
  dplyr::left_join(final_mukey_soc, by = "mukey")

save(soc, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/soc_ssurgo.Rdata")
gc()

# Adding the observation and IC scripts at the bottom here, to be re-organized since it seems repetative and disorganized

# GEDI AGB - compared with biomass fia above
pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
dod.shp <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/selected_polygons/selected_polygons.shp")
Dod.shapefile <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/DoD_shapefile/Kirtland_AFB.shp")

load("/shared/users-bucket/mhayden/obs_prep/Kirtland/gedi/dat.Rdata")
start.dates <- c("2018-07-16", "2019-07-16", "2020-07-16", "2021-07-16", "2022-07-16", "2023-07-16")
end.dates <- c("2019-07-15", "2020-07-15", "2021-07-15", "2022-07-15", "2023-07-15", "2024-07-15")
ref_dem <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/dem_merged/dem_resample.tif")
r_template <- terra::rast(terra::crop(ref_dem, Dod.shapefile))


# loop over time.
# 1. Create the matrix with 6 columns (to match your 6 dates)
agb.mean.gedi.ts <- agb.sd.gedi.ts <- matrix(NA, length(pts), 6)
# 2. Assign the column names
colnames(agb.mean.gedi.ts) <- colnames(agb.sd.gedi.ts) <- end.dates
for (i in seq_along(start.dates)) {
  # filter by date.
  dat_filtered <- dat %>%
    mutate(shot_time = as.POSIXct(shot_time)) %>%
    filter(shot_time >= as.POSIXct(start.dates[i]) & shot_time <= as.POSIXct(end.dates[i])) %>%
    data.table::setDT()
  # aggregation.
  coords <- as.matrix(dat_filtered[, .(lon_lowestmode, lat_lowestmode)])
  dat_filtered[, cell_id := cellFromXY(r_template, coords)]

  # 2. Count the number of points in each cell (.N is data.table's count function)
  # (If you wanted mean AGBD instead, you'd do: mean_agbd = mean(agbd, na.rm=TRUE))
  cell_summary <- dat_filtered[!is.na(cell_id), .(pt_count = .N, mean_agbd = mean(agbd, na.rm = TRUE), max_std = max(agbd_se, na.rm = TRUE)), by = cell_id]

  # 3. Write those summarized values directly into your raster template
  agb.mean.gedi <- r_template
  values(agb.mean.gedi) <- NA # Ensure it's blank to start
  agb.mean.gedi[cell_summary$cell_id] <- cell_summary$mean_agbd
  
  agb.sd.gedi <- r_template
  values(agb.sd.gedi) <- NA # Ensure it's blank to start
  agb.sd.gedi[cell_summary$cell_id] <- cell_summary$max_std
  # extraction.
  agb.m.pts <- terra::extract(agb.mean.gedi * 1, pts)
  agb.mean.gedi.ts[,i] <- agb.m.pts[,2]
  
  agb.sd.pts <- terra::extract(agb.sd.gedi * 1, pts)
  agb.sd.gedi.ts[,i] <- agb.sd.pts[,2]
}
save(list = c("agb.mean.gedi.ts", "agb.sd.gedi.ts"), file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/agb_mean_std_gedi.Rdata")


# LAI
LAI <- read.csv("/shared/users-bucket/mhayden/obs_prep/Kirtland/LAI/LAI.csv")
LAI <- LAI[which(LAI$sd<24.8),]
LAI$sd[which(LAI$sd == 0)] <- 0.1
# 
dates <- seq(as.Date("2002-07-15"), as.Date("2024-07-15"), "1 year")
data.table::setDT(LAI)
# Ensure the date columns are actual Date objects for matching
LAI[, date_formatted := as.Date(date)] 

LAI.mean.ts <- LAI.std.ts <- matrix(NA, length(pts), 23)
for (i in seq_along(pts)) {
  LAI.temp <- LAI[which(LAI$site_id == as.character(i)),]
  # Create a data.table for your target dates
  target_dates <- data.table::data.table(target_date = dates)
  # Join target dates to LAI to find absolute differences
  closest_records <- target_dates[, {
    # Calculate absolute difference between this target_date and all LAI dates
    differences <- abs(as.numeric(LAI.temp$date_formatted - target_date))
    # Return the row with the minimum difference
    LAI.temp[which.min(differences), ]
  }, by = target_date]
  LAI.mean.ts[i, ] <- closest_records$lai
  LAI.std.ts[i, ] <- closest_records$sd
} # results in many NAs for this site because of QC filter in LAI.csv creation

# Moving this over from IC because one of the variables is required below
var.names.lai <- c("lai_hv", "lai_lv")
ens <- 100
lai.list <- list()
# loop over years.
# Use this code if there are multiple pixels, if not skip to next.
for (t in 2000:2024) {
  ncfile <- file.path("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/ERA5", paste0("ERA5_", t, ".nc"))
  era5.temp <- raster::brick(ncfile, varname = "lai_hv", level = 8)
  lai.temp <- raster::extract(era5.temp, sp::SpatialPoints(cbind(pts$lon, pts$lat)), method = 'simple')
  lai.temp.h <- rowMeans(lai.temp)
  
  era5.temp <- raster::brick(ncfile, varname = "lai_lv", level = 8)
  lai.temp <- raster::extract(era5.temp, sp::SpatialPoints(cbind(pts$lon, pts$lat)), method = 'simple')
  lai.temp.l <- rowMeans(lai.temp)
  
  # loop over sites.
  lai.ens.mat <- matrix(NA, length(pts), ens)
  for (i in seq_along(pts)) {
    lai.ens.mat[i,] <- runif(ens, lai.temp.l[i], lai.temp.h[i])
  }
  lai.list[[t-1999]] <- lai.ens.mat
}

# There's only one pixel over Kirtland so use this code
for (t in 2000:2024) {
  ncfile <- file.path("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/ERA5", paste0("ERA5_", t, ".nc"))
  
  # 1. Open the NetCDF file
  nc <- nc_open(ncfile)
  
  # 2. Extract the raw arrays (these are 8x10 matrices: 8 times x 10 ensemble members)
  lai_hv_array <- ncvar_get(nc, "lai_hv")
  lai_lv_array <- ncvar_get(nc, "lai_lv")
  
  nc_close(nc)
  
  # 3. Calculate the single average value for the entire Kirtland pixel 
  # (Averaging across the 8 time steps and 10 ensemble members)
  lai_h_mean <- mean(lai_hv_array, na.rm = TRUE)
  lai_l_mean <- mean(lai_lv_array, na.rm = TRUE)
  
  # 4. Get your min and max bounds for the runif generator
  min_val <- min(lai_l_mean, lai_h_mean, na.rm = TRUE)
  max_val <- max(lai_l_mean, lai_h_mean, na.rm = TRUE)
  
  # 5. Populate the matrix for all points in your shapefile
  lai.ens.mat <- matrix(NA, length(pts$lon), ens)
  
  for (i in seq_along(pts$lon)) {
    if (is.finite(min_val) && is.finite(max_val)) {
      lai.ens.mat[i,] <- runif(ens, min_val, max_val)
    } else {
      lai.ens.mat[i,] <- NA
    }
  }
  
  # Store in the list
  lai.list[[t-1999]] <- lai.ens.mat
}


save(lai.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/lai_list.Rdata")
LAI_IC <- lai.list[[1]]

# Resuming code from Observation
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/lai_list.Rdata")
lai.mean.2001 <- apply(lai.list[[2]], 1, mean)
LAI.mean.ts <- cbind(lai.mean.2001, LAI.mean.ts)
lai.std.2001 <- apply(lai.list[[2]], 1, sd)
LAI.std.ts <- cbind(lai.mean.2001, LAI.std.ts)
colnames(LAI.mean.ts) <- colnames(LAI.std.ts) <- seq(as.Date("2001-07-15"), as.Date("2024-07-15"), "1 year") %>% as.character()
save(list = c("LAI.mean.ts", "LAI.std.ts"), file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/lai_mean_std.Rdata")

# CDS Soil moisture

# first piece is from IC script
pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
var.names.sm <- c("swvl1", "swvl2", "swvl3")
sm.list <- list()
ens <- 100

# code for multiple pixels from donchen - see below for code that worked for single pixel with chatgsfc
# loop over years.
for (t in 2000:2024) {
  ncfile <- file.path("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/ERA5", paste0("ERA5_", t, ".nc"))
  era5.temp.l1 <- raster::brick(ncfile, varname = "swvl1", level = 8)
  era5.temp.l2 <- raster::brick(ncfile, varname = "swvl2", level = 8)
  era5.temp.l3 <- raster::brick(ncfile, varname = "swvl3", level = 8)
  
  temp.brick <- raster::brick(c(era5.temp.l1, era5.temp.l2, era5.temp.l3))
  # 1. Extract values from the 3 layers for your spatial points
  # This returns a matrix where columns represent the layers (swvl1, swvl2, swvl3)
  sm.extracted <- raster::extract(temp.brick, sp::SpatialPoints(cbind(pts$lon, pts$lat)), method = 'simple')

  # 2. Calculate the depth-weighted mean for 0-1m depth
  # Weights: swvl1 (0.07), swvl2 (0.21), swvl3 (0.72)
  sm.depth.weighted <- (0.07 * sm.extracted[,1:10]) + (0.21 * sm.extracted[,11:20]) + (0.72 * sm.extracted[,21:30])

  # 3. Generate the ensemble matrix (Sites x Ensembles)
  # Assuming you want to add a normal/uniform perturbation around the mean or use standard deviation
  sm.ens.mat <- matrix(NA, length(pts), ens)

  for (i in seq_along(pts)) {
    sm.ens.mat[i, ] <- sample(sm.depth.weighted[i,], ens, T)
  }

  # 4. Save to your list for the current year 't'
  sm.list[[t-1999]] <- sm.ens.mat
}

# code that worked for single pixel

# loop over years.
for (t in 2000:2024) {
  ncfile <- file.path("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/ERA5", paste0("ERA5_", t, ".nc"))
  
  # 1. Open the NetCDF file
  nc <- nc_open(ncfile)
  
  # 2. Extract the raw 8x10 arrays natively
  swvl1 <- ncvar_get(nc, "swvl1")
  swvl2 <- ncvar_get(nc, "swvl2")
  swvl3 <- ncvar_get(nc, "swvl3")
  
  nc_close(nc)
  
  # 3. Calculate the depth-weighted mean for 0-1m depth natively on the arrays
  # This creates a single 8x10 matrix of depth-weighted soil moisture
  sm_weighted_matrix <- (0.07 * swvl1) + (0.21 * swvl2) + (0.72 * swvl3)
  
  # 4. Flatten the 8x10 matrix into a pool of 80 possible values (time x ensemble members)
  sm_pool <- as.vector(sm_weighted_matrix)
  
  # 5. Generate the ensemble matrix (Sites x Ensembles)
  sm.ens.mat <- matrix(NA, length(pts), ens)

  # For each point, draw 'ens' random samples from the 80 available ERA5 values with replacement
  for (i in seq_along(pts)) {
    # We check if the pool is mostly valid numbers to prevent errors
    if (any(!is.na(sm_pool))) {
      # sample(pool, size, replace = TRUE)
      sm.ens.mat[i, ] <- sample(na.omit(sm_pool), ens, replace = TRUE)
    } else {
      sm.ens.mat[i, ] <- NA
    }
  }

  # 6. Save to your list for the current year 't'
  sm.list[[t-1999]] <- sm.ens.mat
}

# Save final outputs
save(sm.list, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/sm_list.Rdata")
SM_IC <- sm.list[[1]]

# move on to observation piece
# CDS SM.
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/sm_list.Rdata")
dates <- seq(as.Date("2001-07-15"), as.Date("2024-07-15"), "1 year")
sm.mean.era5.ts <- sm.std.era5.ts <- matrix(NA, length(pts), 24)
colnames(sm.mean.era5.ts) <- colnames(sm.std.era5.ts) <- as.character(dates)
# loop over time.
for (i in seq_along(dates)) {
  temp.sm <- sm.list[[i+1]]
  sm.mean.era5.ts[,i] <- apply(temp.sm, 1, mean)
  sm.std.era5.ts[,i] <- apply(temp.sm, 1, sd)
}
save(list = c("sm.mean.era5.ts", "sm.std.era5.ts"), file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/sm_mean_std_era5.Rdata")

# SOC
SOC.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/SOC.tif")/10
soc.mean.std.soilgrids <- terra::extract(SOC.rast, pts)
save(soc.mean.std.soilgrids, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/soc_mean_std_soilgrids.Rdata")

# SMAP SM
SM <- read.csv("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/SMAP/SMAP.csv")
dates <- seq(as.Date("2015-07-15"), as.Date("2024-07-15"), "1 year")
data.table::setDT(SM)
# Ensure the date columns are actual Date objects for matching
SM[, date_formatted := as.Date(date)] 
sm.mean.smap.ts <- sm.std.smap.ts <- matrix(NA, length(pts), length(dates))
colnames(sm.mean.smap.ts) <- colnames(sm.std.smap.ts) <- as.character(dates)
for (i in seq_along(pts)) {
  SM.temp <- SM[which(SM$site_id == as.character(i)),]
  # Create a data.table for your target dates
  target_dates <- data.table::data.table(target_date = dates)
  # Join target dates to LAI to find absolute differences
  closest_records <- target_dates[, {
    # Calculate absolute difference between this target_date and all LAI dates
    differences <- abs(as.numeric(SM.temp$date_formatted - target_date))
    # Return the row with the minimum difference
    SM.temp[which.min(differences), ]
  }, by = target_date]
  sm.mean.smap.ts[i, ] <- closest_records$smp
  sm.std.smap.ts[i, ] <- closest_records$sd
}
save(list = c("sm.mean.smap.ts", "sm.std.smap.ts"), file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/sm_mean_std_smap.Rdata")


## Combine observations
years <- 2001:2024
obs.path <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data"
# AGB
#load(file.path(obs.path, "AGB", "agb_mean_std_landtrendr.Rdata")) #is this same as just agb no suffix??
load(file.path(obs.path, "agb_mean_std.Rdata")) #is this same as just agb no suffix??
load(file.path(obs.path, "agb_mean_std_gedi.Rdata"))

# LAI.
load(file.path(obs.path, "lai_mean_std.Rdata"))

# SM.
load(file.path(obs.path, "sm_mean_std_era5.Rdata"))
load(file.path(obs.path, "sm_mean_std_smap.Rdata"))

# SOC
load(file.path(obs.path, "soc_mean_std_soilgrids.Rdata"))

# Left off here - need to get SMAP first

# Combine observations
# loop over time.
time_points <- seq(as.Date("2001-07-15"), as.Date("2024-07-15"), "1 year")
obs.mean <- obs.cov <- vector("list", length = length(time_points))
names(obs.mean) <- time_points
names(obs.cov) <- time_points
# try running this for single time point first and then add as source code to run
source("/shared/users-local/mhayden/SERDP-RC25-4751-1/src/Fort_Hunter_Liggett/functions/run_obs_loop.R")
for (i in seq_along(time_points)) {
  print(time_points[i])
  # initialize annual observation.
  annual.mean <- annual.cov <- vector("list", length = length(pts))
  names(annual.mean) <- seq_along(pts)
  names(annual.cov) <- seq_along(pts)
  
  # Annual AGB.
  if (i <= 17) { # used to be 24 but I didn't do the random forest interpolation at top of script...
    agb_landtrendr_annual <- list(mean = agb.mean[,i], std = agb.std[,i])
  } else {
    agb_landtrendr_annual <- list(mean = rep(NA, length(pts)), std = rep(NA, length(pts)))
  }
  if (i >= 19) {
    agb_gedi_annual <- list(mean = agb.mean.gedi.ts[,as.character(time_points[i])],
                            std = agb.sd.gedi.ts[,as.character(time_points[i])])
  } else {
    agb_gedi_annual <- list(mean = rep(NA, length(pts)), std = rep(NA, length(pts)))
  }
  
  # Annual LAI.
  lai_annual <- list(mean = LAI.mean.ts[,i], std = LAI.std.ts[,i])
  
  # Annual SM.
  sm_era5_annual <- list(mean = sm.mean.era5.ts[,i], std = sm.std.era5.ts[,i])
  if (i >= 15) {
    sm_smap_annual <- list(mean = sm.mean.smap.ts[,as.character(time_points[i])],
                           std = sm.std.smap.ts[,as.character(time_points[i])])
  } else {
    sm_smap_annual <- list(mean = rep(NA, length(pts)), std = rep(NA, length(pts)))
  }
  
  # Annual SOC.
  soc_annual <- list(mean = soc.mean.std.soilgrids$mean, std = soc.mean.std.soilgrids$std)
  
  # loop over sites.
  for (j in seq_along(pts)) {
    # LandTrendr, MODIS, ERA5_SM, SoilGrids.
    if (i >= 1 & i <= 15) {
      var.names <- c("AbvGrndWood", "LAI", "SoilMoistFrac", "TotSoilCarb")
      products <- c("LandTrendr", "MODIS", "ERA5_SM", "SoilGrids")
      vec.mean <- c(agb_landtrendr_annual$mean[j],
                    lai_annual$mean[j],
                    sm_era5_annual$mean[j]*100,
                    soc_annual$mean[j])
      vec.cov <- diag(c(agb_landtrendr_annual$std[j]^2, 
                        lai_annual$std[j]^2, 
                        (sm_era5_annual$std[j]*100)^2, 
                        soc_annual$std[j]^2))
    }
    # LandTrendr, MODIS, ERA5_SM, SMAP, SoilGrids.
    if (i >= 15 & i <= 19) {
      var.names <- c("AbvGrndWood", "LAI", "SoilMoistFrac", "SoilMoistFrac", "TotSoilCarb")
      products <- c("LandTrendr", "MODIS", "ERA5_SM", "SMAP", "SoilGrids")
      vec.mean <- c(agb_landtrendr_annual$mean[j],
                    lai_annual$mean[j],
                    sm_era5_annual$mean[j]*100,
                    sm_smap_annual$mean[j],
                    soc_annual$mean[j])
      vec.cov <- diag(c(agb_landtrendr_annual$std[j]^2, 
                        lai_annual$std[j]^2, 
                        (sm_era5_annual$std[j]*100)^2, 
                        sm_smap_annual$std[j]^2,
                        soc_annual$std[j]^2))
    }
    # LandTrendr, GEDI, MODIS, ERA5_SM, SMAP, SoilGrids.
    if (i >= 19 & i <= 23) {
      var.names <- c("AbvGrndWood", "AbvGrndWood", "LAI", "SoilMoistFrac", "SoilMoistFrac", "TotSoilCarb")
      products <- c("LandTrendr", "GEDI", "MODIS", "ERA5_SM", "SMAP", "SoilGrids")
      vec.mean <- c(agb_landtrendr_annual$mean[j],
                    agb_gedi_annual$mean[j],
                    lai_annual$mean[j],
                    sm_era5_annual$mean[j]*100,
                    sm_smap_annual$mean[j],
                    soc_annual$mean[j])
      vec.cov <- diag(c(agb_landtrendr_annual$std[j]^2, 
                        agb_gedi_annual$std[j]^2,
                        lai_annual$std[j]^2, 
                        (sm_era5_annual$std[j]*100)^2, 
                        sm_smap_annual$std[j]^2,
                        soc_annual$std[j]^2))
    }
    # GEDI, MODIS, ERA5_SM, SMAP, SoilGrids.
    if (i == 24) {
      var.names <- c("AbvGrndWood", "LAI", "SoilMoistFrac", "SoilMoistFrac", "TotSoilCarb")
      products <- c("GEDI", "MODIS", "ERA5_SM", "SMAP", "SoilGrids")
      vec.mean <- c(agb_gedi_annual$mean[j],
                    lai_annual$mean[j],
                    sm_era5_annual$mean[j]*100,
                    sm_smap_annual$mean[j],
                    soc_annual$mean[j])
      vec.cov <- diag(c(agb_gedi_annual$std[j]^2,
                        lai_annual$std[j]^2, 
                        (sm_era5_annual$std[j]*100)^2, 
                        sm_smap_annual$std[j]^2,
                        soc_annual$std[j]^2))
    }
    na_ind <- which(is.na(diag(vec.cov)))
    if (length(na_ind) > 0) {
      if(length(vec.mean) == 1){
        vec.cov <- vec.cov[-na_ind]
      }else{
        vec.cov <- vec.cov[-na_ind, -na_ind]
      }
      products <- products[-na_ind]
      vec.mean <- vec.mean[-na_ind]
      vec.mean <- data.frame(matrix(vec.mean, 1)) %>% `colnames<-`(var.names[-na_ind])
      names(vec.mean) <- var.names[-na_ind]
    } else {
      vec.mean <- data.frame(matrix(vec.mean, 1)) %>% `colnames<-`(var.names)
    }
    # add attributes.
    for (k in seq_along(products)) {
      attr(vec.mean[[k]], "source") <- products[k]
    }
    annual.mean[[j]] <- vec.mean
    annual.cov[[j]] <- vec.cov
  }
  obs.mean[[i]] <- annual.mean
  obs.cov[[i]] <- annual.cov
}
save(obs.mean, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/obs.mean.Rdata")
save(obs.cov, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/obs.cov.Rdata")

# remove ERA5 observation.

load("/shared/users-bucket/mhayden/obs_prep/Kirtland/obs.mean.Rdata")
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/obs.cov.Rdata")

# 2. Loop through years (i) and polygons/sites (j)
for (i in seq_along(obs.mean)) {
  for (j in seq_along(obs.mean[[i]])) {
    
    # Get the current list of variables for this site
    current_mean <- obs.mean[[i]][[j]]
    current_cov  <- obs.cov[[i]][[j]]
    
    # Identify which columns/elements do NOT have the "ERA5_SM" attribute
    keep_indices <- sapply(seq_along(current_mean), function(k) {
      src_attr <- attr(current_mean[[k]], "source")
      is.null(src_attr) || src_attr != "ERA5_SM"
    })
    
    # If any ERA5_SM variables were found, filter them out
    if (!all(keep_indices)) {
      # Filter the data frame columns/list elements
      obs.mean[[i]][[j]] <- current_mean[keep_indices]
      
      # Subset the covariance matrix symmetrically (both rows and columns)
      if (is.matrix(current_cov) || is.data.frame(current_cov)) {
        obs.cov[[i]][[j]] <- current_cov[keep_indices, keep_indices, drop = FALSE]
      } else {
        obs.cov[[i]][[j]] <- current_cov[keep_indices]
      }
    }
  }
}

# 3. Save the cleaned versions back to disk
save(obs.mean, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/obs.mean_noERA5_SM.Rdata")
save(obs.cov, file = "/shared/users-bucket/mhayden/obs_prep/Kirtland/obs.cov_noERA5_SM.Rdata")