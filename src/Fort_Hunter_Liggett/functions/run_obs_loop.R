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