# 1. Load Required Libraries
library(PEcAn.all)
library(PEcAn.data.atmosphere)
library(ecmwfr)
library(lubridate)
library(purrr)

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