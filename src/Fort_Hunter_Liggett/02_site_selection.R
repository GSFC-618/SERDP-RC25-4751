# Site selection
# Run with srun --partition=demand-36cpu-c5n --mem=80G --cpus-per-task=4 --pty bash

# Set the root directory for all chunks
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/")

# initialize parallel.
future::plan(future::multisession, workers = 4)

# loading libraries.
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
library(factoextra)

# loading files
data.dir <- "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/site_selection_data"
load(file.path(data.dir, "lc.Rdata"))
load(file.path(data.dir, "agb.Rdata"))
load(file.path(data.dir, "dem.Rdata"))
load(file.path(data.dir, "climate.Rdata"))
load(file.path(data.dir, "soil.Rdata"))
load(file.path(data.dir, "stand_age.Rdata"))
load(file.path(data.dir, "gedi_density.Rdata"))

# handling LC.
nlcd_dict <- c(
  "11" = "Open Water",
  "12" = "Perennial Ice/Snow",
  "21" = "Developed, Open Space",
  "22" = "Developed, Low Intensity",
  "23" = "Developed, Medium Intensity",
  "24" = "Developed, High Intensity",
  "31" = "Barren Land",
  "41" = "Deciduous Forest",
  "42" = "Evergreen Forest",
  "43" = "Mixed Forest",
  "52" = "Shrub/Scrub",
  "71" = "Grassland/Herbaceous",
  "81" = "Pasture/Hay",
  "82" = "Cultivated Crops",
  "90" = "Woody Wetlands",
  "95" = "Emergent Herbaceous Wetlands"
)
non.veg.lc <- c("11", "12", "21", "22", "23", "24", "31")
veg.len <- c()
for (i in seq_along(lc.list)) {
  veg.len <- c(veg.len, length(which(!lc.list[[i]] %in% non.veg.lc & agb.list[[i]] > 0)))
}
# sum(veg.len)
# function for smooth division.
smooth.div <- function(tot, v) {
  vec <- c()
  ceil <- TRUE
  remain <- tot
  
  for (i in seq_along(v)) {
    if (i == length(v)) {
      vec <- c(vec, remain)
      break
    }
    if (ceil) {
      temp <- ceiling(v[i])
    } else {
      temp <- floor(v[i])
    }
    ceil <- !ceil
    remain <- remain - temp
    vec <- c(vec, temp)
  }
  vec
}
min.sample.size.per.lc <- 50
pca.plot <- function(dat, inds, ids) {
  # grab ecoClim for the current land cover class.
  clusters.df <- dat[inds, c(2:16)]
  # PCA operation.
  pca_res <- prcomp(as.matrix(clusters.df), center = TRUE, scale. = TRUE)
  summ <- summary(pca_res)
  variance.explained <- summ$importance[2,]
  pca.loading <- t(pca_res$rotation[,1:2])
  plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), cluster = as.character(ids))
  # calculate center points.
  cent.loc <- data.frame()
  for (j in seq_along(unique(ids))) {
    cent.loc <- rbind(cent.loc, list(PC1 = median(plot_data[which(plot_data$cluster == j),1]), 
                                     PC2 = median(plot_data[which(plot_data$cluster == j),2])))
  }
  cent.loc$cluster <- as.character(unique(k$cluster))
  # cent.locs[[i]] <- cent.loc
  p <- ggplot() +
    geom_point(data = plot_data, mapping = aes(x = PC1, y = PC2, colour = cluster)) +
    geom_point(data = cent.loc, mapping = aes(x = PC1, y = PC2, fill = cluster), 
               shape = 23, size = 5, colour = "black") +
    labs(x = paste0("PC1 (", variance.explained[1]*100, "%)"),
         y = paste0("PC2 (", variance.explained[2]*100, "%)"))
  return(list(plot = p, variance.explained = variance.explained, pca.loading = pca.loading, cent.loc = cent.loc))
}

# cluster visualizations.

# loop over polygons.
poly.ind <- 3
LC <- lc.list[[poly.ind]]
AGB <- agb.list[[poly.ind]]
DEM <- dem.list[[poly.ind]]$dem.df$dem
CLIMATE <- climate.list[[poly.ind]]
SOIL <- soil.list[[poly.ind]]
STAND <- stand.age.list[[poly.ind]]
var.names <- c("lc", "agb", "dem", colnames(CLIMATE), colnames(SOIL), "stand_age", "gedi_density")
dat <- cbind(LC, AGB, DEM, CLIMATE, SOIL, STAND, gedi_density$gedi_density) %>% as.data.frame()
colnames(dat) <- var.names
dat$lc <- as.factor(dat$lc)
# Create a copy to keep original data safe
dat_norm <- dat
# Apply Min-Max normalization to columns 2 through 16
dat_norm[, 2:16] <- apply(dat[, 2:16], 2, function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})
# handling LC.
veg.inds <- which(!dat$lc %in% non.veg.lc & dat$agb > 0 & complete.cases(dat))
# determine the total number of samples.
if (length(veg.inds)/500 < 500) {
  tot.sample.size <- 500
} else {
  tot.sample.size <- ceiling(length(veg.inds)/500)
}
dat.dim <- dim(dat)
dat <- dat_norm[veg.inds,]
weights <- dat$agb + 1 - dat$stand_age + dat$gedi_density
# loop over LC.
lc <- unique(dat$lc)
# remove LC that only few pixels cover.
lc.pix.num <- lc %>% purrr::map(function(l){
  length(which(dat$lc == l))
}) %>% unlist
if (length(which(lc.pix.num < min.sample.size.per.lc))) {
  lc <- lc[-which(lc.pix.num < min.sample.size.per.lc)]
}
# calculate sample sizes per land cover class.
LC.inds <- vector("list", length = length(lc))
for (i in seq_along(LC.inds)) {
  LC.inds[[i]] <- which(dat$lc == lc[i])
}
proportion <- LC.inds %>% purrr::map(length) %>% unlist
proportion <- proportion/sum(proportion)
lc.sizes <- smooth.div(tot.sample.size, (tot.sample.size - min.sample.size.per.lc*length(lc)) * proportion + min.sample.size.per.lc)
# start clustering.
clusters <- points <- tot.clust <- k.pca <- k.means.tot <- vector("list", length = length(lc))
for (i in seq_along(lc)) {
  print(paste("Processing", i, "land cover."))
  # total pixels.
  tot.inds <- which(dat$lc == lc[i])
  # handling sample per land cover class.
  sample.max <- 6e4
  # choose sample size (only consider vegetated pixels).
  initial.sample <- FALSE
  if (length(tot.inds) > sample.max) {
    sample.size <- sample.max
    initial.sample <- TRUE
  } else {
    sample.size <- length(tot.inds)
  }
  # sample based on the weights of AGB.
  if (initial.sample) {
    # normalize weights.
    # weights <- dat$agb[tot.inds]
    sample.inds <- sample(x = tot.inds, size = sample.size, replace = F, prob = weights[tot.inds])
    sample.inds <- na.omit(sample.inds)
  } else {
    sample.inds <- tot.inds
  }
  # k-means clustering. (updated via claude because k must be > 2)
  # find the correct size for the current land cover.
  tot.within <- c()
  kmeans.lc <- vector("list", length = 20)
  
  # START LOOP AT 2 instead of 1
  for (j in 2:20) { 
    temp <- factoextra::hkmeans(dat[sample.inds, c(2:16)], j, hc.metric = "euclidean", iter.max = 50)
    kmeans.lc[[j]] <- temp
    tot.within <- c(tot.within, temp$tot.withinss)
    print(paste("Testing cluster size:", j))
  }
  k.means.tot[[i]] <- kmeans.lc
  
  # Adjust x axis to 2:20 to match the loop
  df <- data.frame(x = 2:20, y = tot.within) 
  
  # find the correct cluster size through the elbow location.
  # find_curve_elbow returns the row index, so we map it back to the actual 'j' value using df$x
  elbow_idx <- pathviewr::find_curve_elbow(df)
  size <- df$x[elbow_idx] 
  
  # k-means clustering.
  k <- kmeans.lc[[size]]
  pca.res.all <- pca.plot(dat, sample.inds, k$cluster)
  # loop over clusters.
  cluster <- list()
  tot.cluster.inds <- tot.cluster.ids <- c()
  cluster.points.num <- smooth.div(lc.sizes[i], rep(lc.sizes[i]/size, size))
  for (j in 1:size) {
    # total cluster points.
    cluster.sample.inds <- sample.inds[which(k$cluster == j)]
    # sampling.
    # weights <- dat$agb[cluster.sample.inds]
    cluster.sample.inds <- sample(x = cluster.sample.inds, size = cluster.points.num[j], replace = F, prob = weights[cluster.sample.inds])
    cluster.sample.inds <- na.omit(cluster.sample.inds)
    # summarize.
    cluster[[j]] <- list(inds = cluster.sample.inds)
    tot.cluster.inds <- c(tot.cluster.inds, cluster.sample.inds)
    tot.cluster.ids <- c(tot.cluster.ids, rep(j, length(cluster.sample.inds)))
  }
  # load to lists.
  clusters[[i]] <- list(cluster.inds = cluster, tot.cluster.inds = tot.cluster.inds, tot.cluster.ids = tot.cluster.ids)
  tot.clust[[i]] <- list(k_means = k, inds = sample.inds)
  # pca plotting.
  pca.res.clusters <- pca.plot(dat, tot.cluster.inds, tot.cluster.ids)
  k.pca[[i]] <- list(pca.all = pca.res.all, pca.clusters = pca.res.clusters)
  # print progress.
  print(i)
}
save(list = c("k.means.tot", "clusters", "tot.clust", "k.pca"), file = "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/cluster.Rdata")

# generate shapefile.
load("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/cluster.Rdata")
load("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/site_selection_data/coordinates.Rdata")
gedi_density <- terra::rast("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/gedi/point_count_raster.tif")
poly.id <- 3
LC <- lc.list[[poly.id]]
AGB <- agb.list[[poly.id]]
DEM <- dem.list[[poly.id]]$dem.df$dem
CLIMATE <- climate.list[[poly.id]]
SOIL <- soil.list[[poly.id]]
STAND <- stand.age.list[[poly.id]]
var.names <- c("lc", "agb", "dem", colnames(CLIMATE), colnames(SOIL), "stand_age", "lat", "lon")
dat <- cbind(LC, AGB, DEM, CLIMATE, SOIL, STAND, lon.lat.list[[poly.id]]$lat, lon.lat.list[[poly.id]]$lon) %>% as.data.frame()
colnames(dat) <- var.names
dat$lc <- as.factor(dat$lc)
# handling LC.
veg.inds <- which(!dat$lc %in% non.veg.lc & dat$agb > 0 & complete.cases(dat))
dat <- dat[veg.inds,]
lc.type <- c("Shrub", "Mixed_Forest", "Evergreen_Forest", "Grassland", "Emergent_Herbaceous_Wetlands", "Woody_Wetlands", "Deciduous_Forest")
for (i in seq_along(clusters)) {
  df <- data.frame(lat = dat$lat[clusters[[i]][["tot.cluster.inds"]]],
                   lon = dat$lon[clusters[[i]][["tot.cluster.inds"]]],
                   cluster_id = clusters[[i]][["tot.cluster.ids"]],
                   lc = dat$lc[clusters[[i]][["tot.cluster.inds"]]],
                   agb = dat$agb[clusters[[i]][["tot.cluster.inds"]]])
  points_vector <- terra::vect(df, geom = c("lon", "lat"), crs = "EPSG:4326")
  # calculate frequency of fires per point.
  # setup agent ID for wildfire.
  agent.num <- 6
  # loop over years.
  agent.ext <- c()
  for (j in 1988:2022) {
    # load the corresponding agent raster map.
    temp.agent <- terra::rast(file.path(paste0("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/resample_agent_maps/AGENT_", j, ".tif")))
    temp.agent.ext <- terra::extract(temp.agent, points_vector) %>% data.frame
    temp.agent.ext[,2][which(temp.agent.ext[,2] != agent.num)] <- 0
    temp.agent.ext[,2][which(temp.agent.ext[,2] == agent.num)] <- 1
    agent.ext <- rbind(agent.ext, temp.agent.ext[,2])
  }
  points_vector$fire_freq <- colSums(agent.ext)
  gedi_dens <- terra::extract(gedi_density, points_vector)
  points_vector$gedi_dens <- gedi_dens[,2]
  terra::writeVector(points_vector, file.path("/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/shp_lc", paste0(lc.type[i], ".shp")))
}
