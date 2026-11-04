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
library(parallel)
setwd("/shared/users-bucket/mhayden/obs_prep/Kirtland")

# read settings xml file.
settings_dir <- "SDA/pecan.xml"
settings <- PEcAn.settings::read.settings(settings_dir)

# update settings with the actual PFTs.
settings <- PEcAn.settings::prepare.settings(settings)

# set up to run on srun interactive session.
general.job <- list(cores = 8, 
 folder.num = 30)

# Keep the chunking settings, but remove the qsub command
batch.settings = structure(list(
  general.job = general.job
))
settings$state.data.assimilation$batch.settings <- batch.settings

# Tell PEcAn to run locally on the current interactive node!
settings$host$name <- "localhost"
settings$host$qsub <- NULL
settings$host$qsub.extra <- NULL

# alter the ensemble size.
settings$ensemble$size <- 10
settings$state.data.assimilation$aqq.Init <- 1
settings$ensemble$samplingspace$parameters$method <- "lhc"

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

if (!is.null(settings$pfts)) {
  # Find which PFTs are valid (not NA, not NULL, not empty string)
  valid_pfts <- sapply(settings$pfts, function(p) {
    !is.null(p$name) && !is.na(p$name) && p$name != "NA" && p$name != ""
  })
  
  # Subset the settings to only keep valid PFTs
  if (any(!valid_pfts)) {
    cat("\nWarning: Removing", sum(!valid_pfts), "invalid/NA PFTs from settings.\n")
    settings$pfts <- settings$pfts[valid_pfts]
  }
}

# --- NEW FIX: Force met paths to point to the file, not the directory ---
for (i in seq_along(settings)) {
  # (If your settings are structured as settings[[i]]$run...)
  if (!is.null(settings[[i]]$run$inputs$met$path)) {
    current_path <- settings[[i]]$run$inputs$met$path
    
    # If the path is a directory, append the actual filename to it
    if (dir.exists(current_path)) {
      # Assuming all your files follow a similar naming convention, 
      # or you can just hardcode the filename if it's the exact same file for all runs.
      # Example: appending the specific file name you found:
      fixed_path <- file.path(current_path, "ERA5_1_Mean/ERA5.1.2001-01-01.2024-12-31.clim")
      
      settings[[i]]$run$inputs$met$path <- fixed_path
    }
  }
}
# ------------------------------------------------------------------------

# load PFT parameter file.
load("/shared/users-bucket/mhayden/example_polygon/samples.Rdata")
outdir <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA"

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

batch.folder <- "/shared/users-bucket/mhayden/obs_prep/Kirtland/SDA/batch"

# FIX THE NETCDF CONVERSION BUG (The Pixi Fix)
# The default PEcAn code writes " | R --no-save" at the bottom of the job scripts.
# We need to overwrite that with the Pixi R executable so the model2netcdf function works!
job_files <- list.files(batch.folder, pattern = "job.sh", recursive = TRUE, full.names = TRUE)

for (jf in job_files) {
  script_text <- readLines(jf)
  
  # Replace standard R with your Pixi R path
  script_text <- gsub("| R --no-save", "| /home/mhayden/pecan/.pixi/envs/default/bin/R --no-save", script_text, fixed = TRUE)
  
  # Mute the HPC 'module' commands so they don't throw bash errors
  script_text <- gsub("module load", "# module load", script_text)
  script_text <- gsub("module purge", "# module purge", script_text)
  
  writeLines(script_text, jf)
}

# --- EXECUTE JOBS LOCALLY ON YOUR SRUN NODE ---
# Instead of submitting to a queue, we will run the generated job.sh scripts 
# directly in parallel on the CPUs you already have reserved.

# You can increase mc.cores if you want to use more of your node's CPUs, 
# but start with 8 to ensure you don't run out of memory.

cat("\n=== STARTING LOCAL PARALLEL EXECUTION ===\n")
results <- mclapply(job_files, function(jf) {
  cat(paste("Starting job:", jf, "\n"))
  
  # Run the bash script natively
  system(paste("bash", jf))
  
}, mc.cores = 8)

cat("\n=== ALL SDA BATCHES COMPLETED ===\n")