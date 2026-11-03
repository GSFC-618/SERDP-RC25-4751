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