# some of this is already in obs_prep --> need to reorganize so that these files make sense in order

knitr::opts_chunk$set(echo = TRUE)
# Set the root directory for all chunks
knitr::opts_knit$set(root.dir = "/shared/users-bucket/mhayden/obs_prep/Kirtland/")


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

#read settings.
settings <- PEcAn.settings::read.settings("/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/pecan.xml")
# can eventually try with .rds file instead
#settings <- readRDS("/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/pecan.rds")

#grab settings info.
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
#setup configurations.
time_poimt <- as.Date("2011-07-15")
ens <- 100
source <- "RS_veg"
Write_into_settings <- TRUE
outfolder <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/IC/"
dir.create(outfolder, showWarnings = FALSE)

#read parameters and pfts.
load("/shared/users-bucket/mhayden/example_polygon/samples.Rdata")
pft <- read.csv(settings[[1]]$run$inputs$pft.site$path)

# LAI.
# already created this in the obs_prep stage
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/lai_list.Rdata")
LAI_IC <- lai.list[[1]]


# SM.
# already created this in the obs_prep stage
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/initial_condition_data/sm_list.Rdata")
SM_IC <- sm.list[[1]]

# AGB.
load("/shared/users-bucket/mhayden/obs_prep/Kirtland/observation_data/agb_mean_std.Rdata")
pts <- terra::vect("/shared/users-bucket/mhayden/obs_prep/Kirtland/shp_all/combined_layers.shp")
agb.mean.vec <- as.numeric(unlist(agb.mean))
agb.std.vec <- as.numeric(unlist(agb.std))

AGB_IC <- matrix(NA, length(pts), ens)
for (i in seq_along(pts)) {
  # Use the new numeric vectors here
  AGB_IC[i,] <- truncnorm::rtruncnorm(ens, a = 0, b = Inf, mean = agb.mean.vec[i], sd = agb.std.vec[i])
}

# SOC.
soc.rast <- terra::rast("/shared/users-bucket/mhayden/obs_prep/Kirtland/SM/SOC.tif")/10
soc.pts <- terra::extract(soc.rast, pts)
SOC_IC <- matrix(NA, length(pts), ens)
for (i in seq_along(pts)) {
  SOC_IC[i,] <- truncnorm::rtruncnorm(ens, a = 0, b = Inf, mean = soc.pts$mean[i], sd = soc.pts$std[i])
}

# Convert and write IC files into local directory and into settings.
## Here we will loop over each site and each ensemble member (we will be creating folders for each `site_id` and store the NC files associated with each ensemble member within the folder). Within each loop, we will first do some unit conversions (e.g., AGB, LAI), and then assemble the `poolinfo` object, which is required for writing the NC files through the `PEcAn.SIPNET::veg2model.SIPNET` function. Finally, if we set the `Write_into_settings` as TRUE at the beginning, we will then write those paths into the settings and rewrite the XML file to the `outdir`.

# write with ensemble number
pb <- utils::txtProgressBar(min = 0, max = length(site_info$site_id), style = 3)
for (i in seq_along(site_info$site_id)) {
  #create new folder for each site.
  if(!file.exists(file.path(outfolder, site_info$site_id[i]))) {
    dir.create(file.path(outfolder, site_info$site_id[i]))
  } else {
    #if folder already exists, go to the next.
    next
  }
  #grab SLA from parameter file.
  site_pft <- pft$pft[which(pft$site == site_info$site_id[i])]
  sla <- ensemble.samples[[site_pft]]$SLA
  # --- NEW SAFETY CHECK ---
  if (length(site_pft) == 0) {
    message("\nWarning: No PFT found for site ", site_info$site_id[i], ". Skipping.")
    next
  }
  sla <- ensemble.samples[[site_pft]]$SLA
  if (is.null(sla) || length(sla) == 0) {
    message("\nWarning: No SLA found for site ", site_info$site_id[i], " (PFT: ", site_pft, "). Skipping.")
    next
  }
  # ------------------------
  paths <- c()
  for (j in 1:ens) {
    #unit conversion for AGB.
    AGB <- ifelse(is.na(AGB_IC[i, j]), NULL, PEcAn.utils::ud_convert(AGB_IC[i, j], "Mg ha-1", "kg m-2"))
    leaf_carbon_content <- ifelse(is.na(LAI_IC[i, j]), NULL, LAI_IC[i, j]/sample(sla, 1))
    #woody biomass calculation based on leaf and aboveground biomass densities.
    if (!is.null(AGB)) {
      if (!is.null(leaf_carbon_content)) {
        wood_carbon_content <- AGB - leaf_carbon_content
      } else {
        wood_carbon_content <- AGB
      }
    } else {
      wood_carbon_content <- NULL
    }
    #assemble poolinfo and write into NC files.
    dims <- list(time =1) #Time dimension may be irrelevant
    variables <-list(AbvGrndWood = AGB, 
                     wood_carbon_content = wood_carbon_content, 
                     leaf_carbon_content = leaf_carbon_content, 
                     soil_organic_carbon_content = ifelse(is.na(SOC_IC[i, j]), NULL, SOC_IC[i, j]),
                     SoilMoistFrac = ifelse(is.na(SM_IC[i, j]), NULL, SM_IC[i, j]))
    variables[sapply(variables, is.null)] <- NULL
    poolinfo <- list(dims = dims, vals = variables)
    out <- PEcAn.SIPNET::veg2model.SIPNET(file.path(outfolder, site_info$site_id[i]), 
                                          poolinfo, as.numeric(site_info$site_id[i]), 
                                          ens = j)
    #record paths.    
    paths <- c(out$file, paths)
  }
  #write paths of initial conditions into settings.
  if(Write_into_settings){
    #populated IC file paths into settings
    Create_mult_list <- function(list.names, paths){
      out <- as.list(paths)
      names(out) <- list.names
      out
    }
    settings[[i]]$run$inputs$poolinitcond$source <- source
    settings[[i]]$run$inputs$poolinitcond$output <- "poolinitcond"
    settings[[i]]$run$inputs$poolinitcond$ensemble <- ens
    settings[[i]]$run$inputs$poolinitcond$path <- Create_mult_list(rep("path", ens), paths)
  }
  utils::setTxtProgressBar(pb, i)
}
#write settings.
if(Write_into_settings){
  PEcAn.settings::write.settings(settings, outputdir = file.path(settings$outdir), outputfile = "pecanIC.xml")
}