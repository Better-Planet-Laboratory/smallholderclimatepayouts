#### Script 1: Combine monthly SPEI data for historical, ssp245 and ssp585 for all GCMs, get ensemble averages

## Load packages
library(terra)
library(tidyverse)

### Extract historical
## Step 1: Load monthly tif files 
spei_val <- "spei_6"
model <- "gfdlesm4"
modeltype <- "historical"
startyear <- 1995
endyear <- 2015

in_dir <- paste0("inputdata/", spei_val, "/", model, "_", modeltype)
files  <- list.files(in_dir, pattern = "^spei_6_.*_[0-9]{6}\\.tif$", full.names = TRUE)
stopifnot(length(files) > 0)

## Step 2: Parse YYYYMM from filenames, order chronologically
ym <- sub(".*_([0-9]{6})\\.tif$", "\\1", basename(files))
ord <- order(ym)
files <- files[ord]
ym    <- ym[ord]

## Step 3: Read time-ordered SpatRaster (one layer per month)
r <- rast(files)  # terra reads on demand; no immediate full load in memory

## Step 4: Assign layer names and a time axis (mid-month)
layer_names <- paste0("spei_", substr(ym, 1, 4), "_", substr(ym, 5, 6))
names(r) <- layer_names
dates <- as.Date(paste(substr(ym, 1, 4), substr(ym, 5, 6), "15", sep = "-"))  # 15th of month
time(r) <- dates

## Step 5: Check that all tiles align
template <- rast(files[1])
same_geom <- all(sapply(files, function(f) compareGeom(template, rast(f), stopOnError = FALSE)))

if (!same_geom) {
  message("Geometries differ; resampling all to the first raster’s grid...")
  rs <- lapply(files, \(f) resample(rast(f), template, method = "bilinear"))
  r  <- rast(rs)
  names(r) <- layer_names
  time(r)  <- dates
} else {
  message("All rasters share same geometry.")
}

## Step 6: Write to file (Geotiff)
out_tif <- file.path(paste0("intermediate/", spei_val, "_", model, "_", modeltype, "_", startyear, "_", endyear, "_stack.tif"))
writeRaster(
  r, out_tif, overwrite = TRUE, 
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"),
  datatype = "FLT4S"  # 32-bit float
)
cat("Wrote:", out_tif, "\n")






### Extract SSPs
## Step 1: Load monthly tif files 
spei_val <- "spei_6"
model <- "mpiesmhr"
modeltype <- "ssp585"
startyear <- 2040
endyear <- 2060

in_dir <- paste0("inputdata/", spei_val, "/", model, "_", modeltype, "_", startyear, endyear)
files  <- list.files(in_dir, pattern = "^spei_6_.*_[0-9]{6}\\.tif$", full.names = TRUE)
stopifnot(length(files) > 0)

## Step 2: Parse YYYYMM from filenames, order chronologically
ym <- sub(".*_([0-9]{6})\\.tif$", "\\1", basename(files))
ord <- order(ym)
files <- files[ord]
ym    <- ym[ord]

## Step 3: Read time-ordered SpatRaster (one layer per month)
r <- rast(files)  # terra reads on demand; no immediate full load in memory

## Step 4: Assign layer names and a time axis (mid-month)
layer_names <- paste0("spei_", substr(ym, 1, 4), "_", substr(ym, 5, 6))
names(r) <- layer_names
dates <- as.Date(paste(substr(ym, 1, 4), substr(ym, 5, 6), "15", sep = "-"))  # 15th of month
time(r) <- dates

## Step 5: Check that all tiles align
template <- rast(files[1])
same_geom <- all(sapply(files, function(f) compareGeom(template, rast(f), stopOnError = FALSE)))

if (!same_geom) {
  message("Geometries differ; resampling all to the first raster’s grid...")
  rs <- lapply(files, \(f) resample(rast(f), template, method = "bilinear"))
  r  <- rast(rs)
  names(r) <- layer_names
  time(r)  <- dates
} else {
  message("All rasters share same geometry.")
}

## Step 6: Write to file (Geotiff)
out_tif <- file.path(paste0("intermediate/", spei_val, "_", model, "_", modeltype, "_", startyear, "_", endyear, "_stack.tif"))
writeRaster(
  r, out_tif, overwrite = TRUE, 
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"),
  datatype = "FLT4S"  # 32-bit float
)
cat("Wrote:", out_tif, "\n")

plot(r[[1]])






#---------------------------------
#if there are blanks in the file

library(terra)
library(sf)          # for sf::gdal_utils
library(dplyr)
library(stringr)

in_dir <- "inputdata/gfdlesm4_historical/"   # change me
files  <- list.files(in_dir, pattern = "^spei_6_.*_[0-9]{6}\\.tif$", full.names = TRUE)
stopifnot(length(files) > 0)

# order by YYYYMM at end
ym <- sub(".*_([0-9]{6})\\.tif$", "\\1", basename(files))
ord <- order(ym)
files <- files[ord]; ym <- ym[ord]

# helper: “does this file fully read?”
is_good_tif <- function(f, template = NULL) {
  tryCatch({
    r <- rast(f)
    
    # geometry check (cheap)
    if (!is.null(template) && !compareGeom(template, r, stopOnError = FALSE)) return(FALSE)
    
    # force a real read; convert specific GDAL warnings to errors
    withCallingHandlers(
      {
        invisible(global(r, "mean", na.rm = TRUE))
      },
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("TIFFReadEncodedStrip|Read error at scanline|IReadBlock failed", msg)) {
          stop(w)  # treat these warnings as errors → caught by outer tryCatch
        } else {
          invokeRestart("muffleWarning")  # ignore unrelated warnings
        }
      }
    )
    
    TRUE
  }, error = function(e) FALSE)
}

# Pass 1: find a template (first file that reads OK)
template <- NULL
for (f in files) {
  if (is_good_tif(f)) { template <- rast(f); break }
}
if (is.null(template)) stop("No readable TIFFs found; all look corrupted.")

# Pass 2: test all files
status <- vapply(files, is_good_tif, logical(1), template = template)

bad <- files[!status]
good <- files[status]
cat("Good files:", length(good), " | Bad files:", length(bad), "\n")

# Try to repair bad files by re-encoding with GDAL (DEFLATE+tiled)
repair_one <- function(f) {
  repaired <- sub("\\.tif$", "_repaired.tif", f)
  # translate with CreateCopy; sf::gdal_utils leverages your GDAL install
  try({
    sf::gdal_utils(
      util = "translate",
      source = f,
      destination = repaired,
      options = c(
        "-co", "COMPRESS=DEFLATE",
        "-co", "PREDICTOR=2",
        "-co", "TILED=YES"
      ),
      quiet = TRUE
    )
  }, silent = TRUE)
  if (file.exists(repaired) && is_good_tif(repaired, template)) return(repaired)
  return(NA_character_)
}

if (length(bad)) {
  cat("Attempting to repair", length(bad), "files...\n")
  repaired <- vapply(bad, repair_one, character(1))
  # replace any successful repairs into the list
  ok_rep <- which(!is.na(repaired))
  if (length(ok_rep)) {
    good <- c(good, repaired[ok_rep])
    # keep original ordering by YYYYMM
    all_files <- c(good, bad[setdiff(seq_along(bad), ok_rep)])
    ym_all    <- sub(".*_([0-9]{6})\\.tif$", "\\1", basename(all_files))
    ord2      <- order(ym_all)
    good      <- all_files[ord2][file.exists(all_files[ord2])]
  }
}

# Insert NA placeholders for any months still missing so the time axis stays continuous:
# (build a blank raster on the template grid)
mk_blank <- function(f_like) {
  r0 <- rast(f_like) # ensures same geom
  values(r0) <- NA_real_
  r0
}

# Rebuild chronological sequence with good+blanks
all_by_time <- split(good, sub(".*_([0-9]{6})\\.tif$", "\\1", basename(good)))
uniq_ym <- unique(ym)
layers <- vector("list", length(uniq_ym))
for (i in seq_along(uniq_ym)) {
  ymi <- uniq_ym[i]
  if (ymi %in% names(all_by_time)) {
    # there should be exactly one file per month; if multiple, take the first
    layers[[i]] <- rast(all_by_time[[ymi]][1])
  } else {
    layers[[i]] <- mk_blank(template)
  }
}
r <- rast(layers)

# Assign names & time
layer_names <- paste0("spei_", substr(uniq_ym,1,4), "_", substr(uniq_ym,5,6))
names(r) <- layer_names
time(r)  <- as.Date(paste(substr(uniq_ym,1,4), substr(uniq_ym,5,6), "15", sep = "-"))

# Final geometry harmonisation to the template if needed
if (!compareGeom(template, r, stopOnError = FALSE)) {
  r <- resample(r, template, method = "bilinear")
  names(r) <- layer_names
}

# Write multi-band GeoTIFF (compressed, tiled)
out_tif <- file.path("intermediate/spei_6_gfdlesm4_historical_1995_2015_stack.tif")
writeRaster(
  r, out_tif, overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"),
  datatype = "FLT4S", NAflag = -9999
)
cat("Wrote:", out_tif, "\n")

plot(r[[1]])




#---------------------------------
#Get ensemble averages
#load rasters
 # r1 <- rast("intermediate/spei_6_gfdlesm4_historical_1995_2015_stack.tif")
 # r2 <- rast("intermediate/spei_6_ecearth3_historical_1995_2015_stack.tif")
 # r3 <- rast("intermediate/spei_6_mpiesmhr_historical_1995_2015_stack.tif")
 # r4 <- rast("intermediate/spei_6_mriesm2_historical_1995_2015_stack.tif")
 # r5 <- rast("intermediate/spei_6_noresm2lm_historical_1995_2015_stack.tif")

# r1 <- rast("intermediate/spei_6_gfdlesm4_ssp245_2040_2060_stack.tif")
# r2 <- rast("intermediate/spei_6_ecearth3_ssp245_2040_2060_stack.tif")
# r3 <- rast("intermediate/spei_6_mpiesmhr_ssp245_2040_2060_stack.tif")
# r4 <- rast("intermediate/spei_6_mriesm2_ssp245_2040_2060_stack.tif")
# r5 <- rast("intermediate/spei_6_noresm2lm_ssp245_2040_2060_stack.tif")

r1 <- rast("intermediate/spei_6_gfdlesm4_ssp585_2040_2060_stack.tif")
r2 <- rast("intermediate/spei_6_ecearth3_ssp585_2040_2060_stack.tif")
r3 <- rast("intermediate/spei_6_mpiesmhr_ssp585_2040_2060_stack.tif")
r4 <- rast("intermediate/spei_6_mriesm2_ssp585_2040_2060_stack.tif")
r5 <- rast("intermediate/spei_6_noresm2lm_ssp585_2040_2060_stack.tif")

#create list
rs <- list(r1, r2, r3, r4, r5)

# 0) Sanity checks
stopifnot(all(vapply(rs, inherits, logical(1), what = "SpatRaster")))
nL <- vapply(rs, nlyr, numeric(1))   
stopifnot(length(unique(nL)) == 1)# all have same #layers (e.g., 240)

# 1) Align to a template if needed (extent/res/CRS)
template <- rs[[1]]
align <- function(r) {
  if (!compareGeom(template, r, stopOnError = FALSE)) {
    r <- resample(r, template, method = "bilinear")
  }
  if (as.character(crs(r)) != as.character(crs(template))) {
    r <- project(r, crs(template), method = "bilinear")
  }
  r
}
rs <- lapply(rs, align)

# 2) Stack all layers: order will be r1_1..r1_240, r2_1..r2_240, ..., r5_1..r5_240
stk <- do.call(c, rs)

# 3) Build a grouping index: 1..240 repeated for each model (5 times)
k <- nlyr(rs[[1]])           # e.g., 240
idx <- rep(seq_len(k), times = length(rs))

# 4) Per-month ensemble mean (keeps 240 layers)
ensemble_mean <- tapp(stk, index = idx, fun = function(x) mean(x, na.rm = TRUE))

# (Optional) carry over time/names from a template
if (!is.null(time(template))) time(ensemble_mean) <- time(template)
if (!is.null(names(template))) names(ensemble_mean) <- names(template)

# 5) Write to disk
# writeRaster(
#   ensemble_mean,
#   "spei_analysis/spei_inputs/spei_6_ensemble_historical_1995_2015_stack.tif",
#   overwrite = TRUE,
#   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"),
#   datatype = "FLT4S"
# )
# 
# writeRaster(
#   ensemble_mean,
#   "spei_analysis/spei_inputs/spei_6_ensemble_ssp245_2040_2060_stack.tif",
#   overwrite = TRUE,
#   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"),
#   datatype = "FLT4S"
# )


writeRaster(
  ensemble_mean,
  "intermediate/spei_6_ensemble_ssp585_2040_2060_stack.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"),
  datatype = "FLT4S"
)
