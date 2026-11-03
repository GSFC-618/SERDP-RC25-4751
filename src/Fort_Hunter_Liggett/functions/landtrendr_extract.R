# reference this file: https://github.com/PecanProject/pecan/blob/5e9535fe51721cbc0caf3a5bb30421e6e6d1772f/modules/data.remote/R/LandTrendr.AGB.R#L77
# ftp connection doesn't seem to work on pcluster...
# need to transfer files from bu cluster

# First run this on the BU cluster to crop the files:

library(terra)

# 1. Define where to save the cropped files on the BU cluster
#out_dir <- "~/fhl_agb_cropped"
out_dir <- "~/kirtland_agb_cropped"
dir.create(out_dir, showWarnings = FALSE)

# 2. Gather all the file paths from both colleagues' folders
hamzed_dir <- "/projectnb/dietzelab/hamzed/LandTrendr/LandTrendr_AGB_data"
dongchen_dir <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/AGB"

files_to_crop <- c(
  list.files(hamzed_dir, pattern = "biomassfiaald_.*\\.tif$", full.names = TRUE),
  list.files(dongchen_dir, pattern = "biomassfiaald_.*\\.tif$", full.names = TRUE)
)

# Filter to make sure we only grab median and stdv files (ignoring any other random files)
files_to_crop <- files_to_crop[grepl("median|stdv", files_to_crop)]

if(length(files_to_crop) == 0) {
  stop("Could not find any files. Check your path or read access.")
}

# 3. Create the Fort Hunter Liggett Bounding Box (Lat/Lon)
# Based on your ERA5 coordinates: c(xmin, xmax, ymin, ymax)
fhl_ext <- terra::ext(-122.0, -121.0, 35.5, 36.5)
kirtland_ext <- terra::ext(-106.599287863887, -106.362138351901, 34.9476409469021, 35.0657277409363) # Example coordinates for Kirtland AFB
fhl_poly <- terra::as.polygons(fhl_ext, crs = "EPSG:4326")
kirtland_poly <- terra::as.polygons(kirtland_ext, crs = "EPSG:4326")

# 4. Loop through the massive files and crop them
cat("Found", length(files_to_crop), "files to process. Starting crop...\n")

for (f in files_to_crop) {
  
  # Load the massive raster (terra only loads metadata, uses almost no RAM)
  r <- terra::rast(f)
  
  # Project the FHL polygon to match the raster's native coordinate system (likely Albers)
  #fhl_poly_proj <- terra::project(fhl_poly, terra::crs(r))
  kirtland_poly_proj <- terra::project(kirtland_poly, terra::crs(r))
  
  # Add a 1-mile buffer (1 mile = 1609.34 meters)
  #fhl_poly_buffered <- terra::buffer(fhl_poly_proj, width = 1610)
  kirtland_poly_buffered <- terra::buffer(kirtland_poly_proj, width = 1610)
  
  # Crop the massive raster using our buffered polygon
  cat("Cropping:", basename(f), "...\n")
  
  # catch errors just in case one of the source files is corrupted
  tryCatch({
    r_cropped <- terra::crop(r, terra::ext(kirtland_poly_buffered))
    
    # Save it to your output folder
    out_name <- file.path(out_dir, basename(f))
    terra::writeRaster(r_cropped, out_name, overwrite = TRUE)
  }, error = function(e) {
    cat("  -> Skipped: Could not read or crop this file.\n")
  })
}

cat("\nDone! All tiny cropped AGB files are saved in:", out_dir, "\n")


#!/usr/bin/env Rscript
# extract_landtrendr.R

# 1. Load required libraries
library(PEcAn.all)
library(PEcAn.data.remote)
library(sp)
library(raster)
library(foreach)
library(doParallel)

# ---------------------------------------------------------
# Paste the two functions you provided right here:
# download.LandTrendr.AGB <- function(...) { ... }
# extract.LandTrendr.AGB <- function(...) { ... }
# ---------------------------------------------------------

# 2. Setup output directory
agb_dir <- "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/example_polygon/AGB"
dir.create(agb_dir, recursive = TRUE, showWarnings = FALSE)

# 3. DOWNLOAD THE DATA
# LandTrendr AGB data typically covers 2000 to 2017
cat("Starting LandTrendr AGB Download...\n")
download_results <- PEcAn.data.remote::download.LandTrendr.AGB(
  outdir = agb_dir,
  target_dataset = "biomass",
  product_dates = 2000:2023, 
  run_parallel = TRUE,
  ncores = 12,           # Keep cores throttled to protect your RAM!
  overwrite = FALSE
)
