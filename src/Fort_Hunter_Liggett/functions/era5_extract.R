# Code to extract ERA5 weather data for observations and initial conditions

# 1. Load Required Libraries
library(PEcAn.all)
library(PEcAn.data.atmosphere)
library(ecmwfr)
library(lubridate)
library(purrr)
library(terra)

# Download ERA5
raw_ERA5_dir <- "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/example_polygon/ERA5"
dir.create(raw_ERA5_dir, showWarnings = FALSE)

# Set keyring backend for headless cluster node
Sys.setenv(R_KEYRING_BACKEND = "env")

# Download the raw ERA5 weather drivers (Temp, Precip, Wind, Radiation, etc.)
# If the function is in your script, remove 'PEcAn.data.atmosphere::'
PEcAn.data.atmosphere::download.ERA5_cds(
  outfolder = raw_ERA5_dir,
  extent = c(-122.0, -121.0, 35.5, 36.5), 
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

gc()

# Since there are both inst and accum variables, we end up with a .zip per year
# these need to be combined into a single .nc per year

# Define the directory where your downloaded ERA5 .zip files are located
raw_ERA5_dir <-  "/shared/users-bucket/mhayden/obs_prep/Kirtland/ERA5"

# Get a list of all .zip files
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