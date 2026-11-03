estimate_dirichlet_parameters <- function(means, quantiles) {
  estimate_alpha0 <- function(means, quantiles) {
    objective_function <- function(alpha0) {
      if (alpha0 <= 0) return(Inf)
      alpha <- means * alpha0
      samples <- MCMCpack::rdirichlet(10000, alpha)
      estimated_quantiles <- apply(samples, 2, stats::quantile, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
      quantile_diff <- sum((estimated_quantiles - do.call(rbind, quantiles))^2)
      return(quantile_diff)
    }
    result <- stats::optim(par = 1, fn = objective_function, method = "L-BFGS-B", lower = 0.01)
    return(result$par)
  }
  alpha0 <- estimate_alpha0(means, quantiles)
  if (alpha0 <= 0) stop("Estimated alpha0 is non-positive, which is invalid.")
  alphas <- means * alpha0
  return(alphas)
}

reformat_soil_list <- function(samples_all_depth) {
  fractions <- c("fraction_of_sand_in_soil", "fraction_of_clay_in_soil", "fraction_of_silt_in_soil")
  reformatted <- stats::setNames(vector("list", length(fractions)), fractions)
  for (fraction in fractions) {
    reformatted[[fraction]] <- unlist(lapply(samples_all_depth, function(depth_list) depth_list[[fraction]])) %>% purrr::set_names(NULL)
  }
  reformatted$soil_depth <- as.numeric(names(samples_all_depth))
  return(reformatted)
}

soil_params_ensemble_soilgrids <- function(settings, sand, clay, silt, outdir, write_into_settings=TRUE){
  rescale_sum_to_one <- function(sand, clay, silt) {
    total <- sand + clay + silt
    return(list(sand = sand / total, clay = clay / total, silt = silt / total))
  }
  create_mult_list <- function(list.names, paths) {
    out <- as.list(paths)
    names(out) <- list.names
    out
  }
  if (any(c(sand$Value, clay$Value, silt$Value) > 2)) {
    sand$Value <- if (is.null(sand$Value)) { NULL } else { sand$Value / 100 }
    clay$Value <- if (is.null(clay$Value)) { NULL } else { clay$Value / 100 }
    silt$Value <- if (is.null(silt$Value)) { NULL } else { silt$Value / 100 }
  }
  ens_n <- as.numeric(settings$ensemble$size)
  texture_all <- merge(sand, clay, by=c("Depth", "Quantile", "Siteid")) %>% 
    merge(silt, by=c("Depth", "Quantile", "Siteid")) %>%
    `colnames<-`(c("soil_depth", "quantile", "siteid", "fraction_of_sand_in_soil", "fraction_of_clay_in_soil", "fraction_of_silt_in_soil"))
  
  texture_all$soil_depth <- as.numeric(gsub("100-200cm", 2, gsub("60-100cm", 1, gsub("30-60cm", 0.6, gsub("15-30cm", 0.3, gsub("5-15cm", 0.15, gsub("0-5cm", 0.05, texture_all$soil_depth)))))))
  
  f1 <- factor(texture_all$siteid, levels = unique(texture_all$siteid))
  dat <- split(texture_all, f1)
  dat <- dat[order(as.numeric(names(dat)))] 
  settings_id <- lapply(settings, function(x) as.numeric(x$run$site$id)) %>% unlist()
  
  cl <- parallel::makeCluster(4)
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(min=1, max=length(dat), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  PATH <- foreach::foreach(i = seq_along(dat), 
                           .packages = c("Kendall", "purrr", "PEcAn.data.land", "dplyr"), 
                           .export = c("estimate_dirichlet_parameters", "reformat_soil_list"), 
                           .options.snow=opts) %dopar% {
                             
    samples_ens <- list()
    paths <- c()
    siteid <- unique(dat[[i]]$siteid)
    soil_depth <- unique(dat[[i]]$soil_depth)
    str_ns <- if (is.numeric(siteid) && siteid > 1e9) paste0(siteid %/% 1e+09, "-", siteid %% 1e+09) else as.character(siteid)
    
    temp_outdir <- file.path(outdir, siteid)
    dir.create(temp_outdir, showWarnings = FALSE, recursive = TRUE)
    
    for (depths in sort(unique(texture_all$soil_depth))) {
      quantiles <- list(
        q5 = dplyr::filter(dat[[i]], .data$quantile == "0.05", soil_depth == depths) %>% dplyr::select("fraction_of_sand_in_soil", "fraction_of_clay_in_soil", "fraction_of_silt_in_soil"),
        q50 = dplyr::filter(dat[[i]], .data$quantile == "0.5", soil_depth == depths) %>% dplyr::select("fraction_of_sand_in_soil", "fraction_of_clay_in_soil", "fraction_of_silt_in_soil"),
        q95 = dplyr::filter(dat[[i]], .data$quantile == "0.95", soil_depth == depths) %>% dplyr::select("fraction_of_sand_in_soil", "fraction_of_clay_in_soil", "fraction_of_silt_in_soil")
      )
      means <- dplyr::filter(dat[[i]], .data$quantile == "Mean", soil_depth == depths) %>% dplyr::select("fraction_of_sand_in_soil", "fraction_of_clay_in_soil", "fraction_of_silt_in_soil")
      soil_rescaled <- rescale_sum_to_one(means$fraction_of_sand_in_soil, means$fraction_of_clay_in_soil, means$fraction_of_silt_in_soil)
      means$fraction_of_sand_in_soil <- soil_rescaled$sand
      means$fraction_of_clay_in_soil <- soil_rescaled$clay
      means$fraction_of_silt_in_soil <- soil_rescaled$silt
      
      alpha_est <- estimate_dirichlet_parameters(as.matrix(means), quantiles)
      samples <- MCMCpack::rdirichlet(ens_n, alpha_est)
      colnames(samples) <- c("fraction_of_sand_in_soil","fraction_of_clay_in_soil","fraction_of_silt_in_soil")
      samples <- list(samples) %>% stats::setNames(depths)
      samples_ens <- append(samples_ens, samples)
    }
    
    for (ens in 1:ens_n) {
      samples_all_depth <- lapply(samples_ens, function(x) x[ens, ])
      reformatted_soil_list <- reformat_soil_list(samples_all_depth)
      prefix <- paste0("Soil_params_", str_ns, "_", ens)
      new.file <- file.path(outdir, siteid, paste0(prefix, ".nc"))
      out.ense <- PEcAn.data.land:::soil2netcdf(reformatted_soil_list, new.file)
      paths <- c(new.file, paths)
    }
    return(paths)
  } %>% purrr::set_names(names(dat))
  
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  
  if (write_into_settings) {
    for (i in seq_along(PATH)) {
      ind <- which(settings_id == names(PATH)[i])
      settings[[ind]]$run$inputs$soil_physics$source <- "SoilGrids"
      settings[[ind]]$run$inputs$soil_physics$output <- "soil_physics"
      settings[[ind]]$run$inputs$soil_physics$ensemble <- ens_n
      settings[[ind]]$run$inputs$soil_physics$path <- create_mult_list(rep("path", ens_n), PATH[[i]])
    }
    PEcAn.settings::write.settings(settings, outputdir = settings$outdir, outputfile = "pecan.xml")
  }
}