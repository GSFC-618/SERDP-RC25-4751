# SDA Runner - UPDATE TO MATCH WITH SINGLE JOB RUNS

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
setwd("/shared/users-bucket/mhayden/example_polygon/")

# read settings xml file.
settings_dir <- "pecan_MTH.xml"
settings <- PEcAn.settings::read.settings(settings_dir)

# update settings with the actual PFTs.
settings <- PEcAn.settings::prepare.settings(settings)

# setup the batch job settings.
general.job <- list(cores = 8, 
 folder.num = 30)

batch.settings = structure(list(
  general.job = general.job,
  qsub.cmd = "qsub -q demand-32cpu-c7a -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
))

settings$state.data.assimilation$batch.settings <- batch.settings

# CRITICAL: Also override the host qsub settings to ensure demand-32cpu-c7a is used
settings$host$qsub <- "qsub -q demand-32cpu-c7a -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
settings$host$qsub.extra <- "-q demand-32cpu-c7a"

# alter the ensemble size.
settings$ensemble$size <- 10
settings$state.data.assimilation$aqq.Init <- 1

# load observations.
load("obs.mean.Rdata")
load("obs.cov.Rdata")

# load("obs.mean_noERA5_SM.Rdata")
# load("obs.cov_noERA5_SM.Rdata")

#load("obs.mean_noERA5_noGEDI.Rdata")
#load("obs.cov_noERA5_noGEDI.Rdata")

# replace zero observations and variances with small numbers.
for (i in 1:length(obs.mean)) {
  if(is.null(obs.mean[[i]][[1]])){
    next
  }
  for (j in 1:length(obs.mean[[i]])) {
    if (length(obs.mean[[i]][[j]])==0) {
      next
    }
    inds <- which(obs.mean[[i]][[j]]==0)
    for (ind in inds) {
      att <- attributes(obs.mean[[i]][[j]][[ind]])[[1]]
      obs.mean[[i]][[j]][[ind]] <- 0.01
      attr(obs.mean[[i]][[j]][[ind]], "source") <- att
    }
    if(length(obs.cov[[i]][[j]]) > 1){
      diag(obs.cov[[i]][[j]])[which(diag(obs.cov[[i]][[j]]<=0.1))] <- 0.1
    }else{
      if(obs.cov[[i]][[j]] <= 0.1){
        obs.cov[[i]][[j]] <- 0.1
      }
    }
  }
}

# --- NEW CLEANUP STEP ---
# Actively remove the empty sites from both lists
# this may be causing the toggle errors in MCMC_block_function
# Try removing this and re-running to see which error prompted me to add this
for (t in names(obs.mean)) {
  
  # Identify sites that are NULL or have a length of 0
  bad_sites <- sapply(obs.mean[[t]], function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x))
  })
  
  # Only subset if there are actually bad sites to remove
  if (any(bad_sites)) {
    obs.mean[[t]] <- obs.mean[[t]][!bad_sites]
    obs.cov[[t]]  <- obs.cov[[t]][!bad_sites]
  }
}

# load PFT parameter file.
load("samples.Rdata")
outdir <- "/shared/users-bucket/mhayden/example_polygon/SDA"

settings$host$prerun <- "export PATH=\"/home/mhayden/pecan/.pixi/envs/default/bin:$PATH\""

# Verify queue settings before running
cat("\n=== QUEUE SETTINGS VERIFICATION ===\n")
cat("Batch qsub command:", settings$state.data.assimilation$batch.settings$qsub.cmd, "\n")
cat("Host qsub command:", settings$host$qsub, "\n")
cat("Host qsub extra:", settings$host$qsub.extra, "\n\n")

# execute the SDA.
qsub_sda(settings = settings, 
         obs.mean = obs.mean, 
         obs.cov = obs.cov, 
         Q = NULL, 
         pre_enkf_params = NULL, 
         ensemble.samples = ensemble.samples, 
         outdir = outdir, 
        # dist.dir = "disturbance_history/dist_history_new.Rdata", # Dongchen hasn't PRed the disturbance workflow yet
         control = list(TimeseriesPlot = FALSE,
                        OutlierDetection=FALSE,
                        send_email = NULL,
                        keepNC = TRUE, # Try this since timestep 2 wasn't converting to nc
                        forceRun = TRUE,
                        MCMC.args = NULL,
                        merge_nc = F),
         block.index = NULL,
         debias = list(cov.dir = "/covariates_lc_ts/covariates_with_LAI/",
                       t.start = NULL, residual.lag = TRUE, start.year = 2003, fun = PEcAnAssimSequential:::.get_debias_mod, mode = 0), 
         prefix = "batch")

batch.folder <- "/shared/users-bucket/mhayden/example_polygon/SDA/batch"

# FIX THE NETCDF CONVERSION BUG (The Pixi Fix)
# The default PEcAn code writes " | R --no-save" at the bottom of the job scripts.
# We need to overwrite that with the Pixi R executable so the model2netcdf function works!
job_files <- list.files(batch.folder, pattern = "job.sh", recursive = TRUE, full.names = TRUE)
for (jf in job_files) {
  script_text <- readLines(jf)
  # Replace standard R with your Pixi R path
  script_text <- gsub("| R --no-save", "| /home/mhayden/pecan/.pixi/envs/default/bin/R --no-save", script_text, fixed = TRUE)
  writeLines(script_text, jf)
}

# Inject the Pixi path directly into EVERY dynamically generated model script!
settings$host$prerun <- "export PATH=\"/home/mhayden/pecan/.pixi/envs/default/bin:$PATH\""

out <- sda.qsub.job.submission(batch.folder = batch.folder, 
                               username = "mhayden", 
                               outdir = "/shared/users-bucket/mhayden/example_polygon/SDA_outputs", 
                               max.job = 31)

while (!all(out == -1)) {
  Sys.sleep(120)
  out <- sda.qsub.job.submission(batch.folder = batch.folder, 
                                 username = "mhayden", 
                                 outdir = "/shared/users-bucket/mhayden/example_polygon/SDA_outputs", 
                                 past.job.ids = out, max.job = 31)
}