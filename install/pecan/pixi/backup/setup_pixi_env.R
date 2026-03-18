#!/usr/bin/env Rscript

# NOTE: This needs to be executed WITHIN a fresh pull of the PEcAn repo 
# (git pull https://github.com/PecanProject/pecan) in order to find the modules
# and model packages to install

# Example usage:
# Rscript setup_pixi_env.R

# Define repo locations
options(repos = c(
  pecanproject = 'https://pecanproject.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install any missing tools needed to create the pixi.toml file
list.of.packages <- c("desc","here","here","tidyverse","pak")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos = "http://cran.rstudio.com/")
}

use("desc")
use("here")
use("tidyverse")
use("pak")

# Create output paths before changing WD
outdir <- file.path(here(),"install/pecan/pixi/scratch/")
if (! file.exists(outdir)) dir.create(outdir,recursive=TRUE)

#NOTE: For now, make sure you set the PEcAn direction as WD
#setwd("/Users/sserbin/Data/Github/pecan")
setwd("/efs/shared/users/serdp-project/scratch/pecan")

base <- list.dirs("base", recursive = FALSE)
modules <- list.dirs("modules", recursive = FALSE)
models <- list.dirs("models", recursive = FALSE)

pkgs <- c(base, modules, models)
names(pkgs) <- pkgs

alldeps <- pkgs |>
  map(desc::desc_get_deps) |>
  bind_rows(.id = "source")

external_deps <- alldeps |>
  filter(!startsWith(package, "PEcAn."))

# Packages included with R
base_r <- c(
  "R",
  "boot",
  "class",
  "cluster",
  "codetools",
  "lattice",
  "grDevices",
  "graphics",
  "grid",
  "KernSmooth",
  "MASS",
  "Matrix",
  "methods",
  "nlme",
  "parallel",
  "stats",
  "survival",
  "tools",
  "utils"
)

# Packages not available on conda-forge
no_conda <- c(
  # Primary dependencies
  "BayesianTools",
  "BioCro",
  "MODISTools",
  "Maeswrap",
  "REddyProc",
  "RPostgreSQL",
  "Rpreles",
  "SimilarityMeasures",
  "SticsRFiles",
  "TruncatedNormal",
  "amerifluxr",
  "dataone",
  "datapack",
  "ecmwfr",
  "geonames",
  "keras3",
  "linkages",
  "lqmm",
  "mlegp",
  "mvbutils",
  "neonstore",
  "nneo",
  "redland",
  "sensitivity",
  "sirt",
  "suntools",
  # nested dependencies:
  "CDM",
  "SparseGrid",
  "TAM",
  "bigleaf",
  "duckdbfs",
  "pbv",
  "solartime",
  "thor"
  # Not available on macos-arm64, but work on linux-64
  # "dtw",
  # "dtwclust",
  # "duckdb"
  # "nimble",
  # "traits",   # Older versions are available
)

# Get the dependencies of the non-conda packages
get_deps <- function(pkg) {
  return(tryCatch(
    pak::pkg_deps(pkg),
    error = function(e) {
      message("Error for ", pkg, " : ", e)
      return(NULL)
    }
  ))
}

names(no_conda) <- no_conda
no_conda_deps <- no_conda |>
  map(get_deps) |>
  bind_rows(.id = "package")

no_conda_uniq <- no_conda_deps |>
  dplyr::filter(
    !(ref %in% no_conda),
    !(ref %in% base_r),
    !(startsWith(ref, "installed::")),
    ref != "roxygen2"
  ) |>
  distinct(ref) |>
  arrange(ref) |>
  pull()

uniq_deps <- external_deps |>
  filter(
    !(package %in% base_r),
    !(package %in% no_conda)
  ) |>
  distinct(package) |>
  pull()

# Additional packages
extras <- c(
  "RCurl"  # Needed by amerifluxr
)

all_uniq_deps <- sort(unique(c(uniq_deps, no_conda_uniq, extras)))
pixi_string <- sprintf("\"r-%s\" = \"*\"", all_uniq_deps)

pixi_lines <- c(
  "[workspace]",
  'authors = [ "Chris Black" ]',
  'channels = ["conda-forge"]',
  'name = "serdp"',
  # NOTE: Would be nice to get osx-64, osx-arm64, and win-64
  # However, some packages don't have builds for osx-arm64 and win-64.
  #'platforms = ["osx-arm64"]', # For MacOSX
  'platforms = ["linux-64"]', # For Linux
  'version = "0.1.0"',
  "",
  "[dependencies]",
  'r = ">=4"',
  'r-roxygen2 = "==7.3.2"',
  pixi_string
)

# Write out pixi.toml - still requires some post creation edits
writeLines(pixi_lines, file.path(outdir, "pixi.toml"))
