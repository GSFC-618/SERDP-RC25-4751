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
                                 ncore = 1,
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
}