##' soilgrids_texture_extraction function
##' A function to extract and save three types of soil texture data in parallel for a single or group of 
##' lat/long locations based on user-defined site location from SoilGrids250m 
##' version 2.0 : https://soilgrids.org
##' @title soilgrids_texture_extraction
##' @name soilgrids_texture_extraction
##' 
##' @param site_info A data frame of site info containing the BETYdb site ID, 
##' site name, latitude, and longitude, e.g. 
##' (site_id, site_name, lat, lon)
##' @param outdir  Provide the path to store the texture data file
##' @param verbose Provide progress feedback to the terminal? TRUE/FALSE
##' @return a data frame containing the soil texture data with columns "Depth", "Quantile", "Siteid", and "Value"
##' 
##' @export
##' @author Qianyu Li
##' @importFrom magrittr %>%

soilgrids_texture_extraction <- function(site_info, outdir = NULL, prefix = NULL, verbose=TRUE){
  
  # A function to extract and save one type of soil texture data 
  download_and_extraction <- function(base_data_url, site_info, outdir, prefix) {
    if (is.null(site_info)) {
      PEcAn.logger::logger.error(
        "No site information found. Please provide a BETY DB site list containing at least the site id and PostGIS geometry\
    as lon and lat"
      )
    }
    # Prepare site info for extraction
    internal_site_info <- data.frame(site_info$site_id, site_info$lat,site_info$lon)
    #Create a variable to store mean and quantile of soil texture data for each soil depth
    soiltquant <- matrix(NA, nrow = 6, ncol = length(internal_site_info$site_info.lon) * 4)
    lonlat <-cbind(internal_site_info$site_info.lon, internal_site_info$site_info.lat)
    depths <-c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
    
    # Reproject locations to soilgrids projection
    # Soilgrids data is using Homolosine projection https://www.isric.org/explore/soilgrids/faq-soilgrids
    p <-terra::vect(lonlat, crs = "+proj=longlat +datum=WGS84") # Users need to provide lon/lat
    if (is.null(prefix)) {
      newcrs <- "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0"
      p_reproj <- terra::project(p, newcrs) # Transform the point vector to data with Homolosine projection
      data_tag <- c("_mean.vrt", "_Q0.05.vrt", "_Q0.5.vrt", "_Q0.95.vrt")
    } else {
      p_reproj <- p
      base_data_url <- paste0(prefix, base_data_url)
      data_tag <- c("_mean.tif", "_Q0.05.tif", "_Q0.5.tif", "_Q0.95.tif")
    }
    
    name_tag <- expand.grid(depths, data_tag, stringsAsFactors = F) #find the combinations between data and depth tags.
    L <- split(as.data.frame(name_tag), seq(nrow(as.data.frame(name_tag))))#convert tags into lists.
    
    soilt_real <- vector("list", length = length(L))
    pb <- utils::txtProgressBar(min = 0, max = length(L), style = 3)
    for (i in seq_along(L)) {
      l <- L[[i]]
      soilt_url <- paste0(base_data_url, l[[1]], l[[2]])
      soilt_map <- terra::extract(terra::rast(soilt_url), p_reproj)
      soilt_real[[i]] <- unlist(soilt_map[, -1])/10
      utils::setTxtProgressBar(pb, i)
    }
    
    for (dep in seq_along(depths)) {
      dep.ind <- which(grepl(depths[dep], name_tag[, 1]))
      soiltquant[dep, ] <- soilt_real[dep.ind] %>% unlist
    }
    
    # Parse extracted data and prepare for output
    quantile_name <-c(paste("Mean_", site_info$site_id, sep = ""),paste("0.05_", site_info$site_id, sep = ""),paste("0.5_", site_info$site_id, sep = ""),paste("0.95_", site_info$site_id, sep = ""))
    colnames(soiltquant) <- quantile_name
    soilt_dep <- cbind(soiltquant, depths)
    soilt_df <- tidyr::pivot_longer(as.data.frame(soilt_dep),cols = tidyselect::all_of(quantile_name),names_to = c("Quantile", "Siteid"),names_sep = "_")
    # Remove NA
    soilt_df <- stats::na.omit(soilt_df)
    colnames(soilt_df) <- c("Depth", "Quantile", "Siteid", "Value")
    soilt_df$Value<-as.numeric(soilt_df$Value)
    
    if (!is.null(outdir)) {
      PEcAn.logger::logger.info(paste0("Storing results in: ",outdir))
      saveRDS(soilt_df,file=outdir)
    }
    else {
      PEcAn.logger::logger.error("No output directory found.")
    }
    # Return the results to the terminal as well
    return(soilt_df)
  }
  
  if (is.null(prefix)) {
    sand_data_url <-
      "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/sand/sand_"
    clay_data_url <-
      "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_"
    silt_data_url <-
      "/vsicurl?max_retry=30&retry_delay=60&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/silt/silt_"
  } else {
    sand_data_url <- "/sand/sand_"
    clay_data_url <- "/clay/clay_"
    silt_data_url <- "/silt/silt_"
  }
  
  URLs <- c(sand_data_url, clay_data_url, silt_data_url)
  save_paths <- file.path(outdir, c("sand_percent.rds", "clay_percent.rds", "silt_percent.rds"))
  results <- vector("list", 3)
  for (i in seq_along(URLs)) {
    results[[i]] <- download_and_extraction(URLs[i], site_info, save_paths[i], prefix)
  }
  return(results)
}