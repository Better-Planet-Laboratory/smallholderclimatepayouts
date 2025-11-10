### Script 2: Extract Growing Season Min/Max SPEI from GCMs

## Load packages
library(terra)
library(dplyr)
library(tidyr)
library(data.table)


## Step 1: Define paths and settings
spei_val <- "spei_12"
model <- "noresm2lm"
modeltype <- "ssp585"
year_start  <- 2040
year_end    <- 2060

spei_path   <- paste0("intermediate/", spei_val, "_", model, "_", modeltype, "_", year_start, "_", year_end, "_stack.tif")  
cropland_fp <- "inputdata/CroplandPastureArea2000_Geotiff/Cropland2000_5m.tif"  # fraction 0..1
calendar_dir<- "inputdata/ALL_CROPS_netCDF_0.5deg_filled/"  # each file has 'plant' & 'harvest' DOY
units_path  <- "inputdata/tests_shps.shp" #admin 0 & 1 shapefiles

# Optional area of interest (AOI) crop (xmin, xmax, ymin, ymax) for speed
aoi_extent  <- NULL
# aoi_extent  <- ext(-25.35, 57.8, -46.95, 40)

# Output
out_csv     <- paste0("intermediate/", spei_val, "_", model, "_", modeltype, "_", year_start, "_", year_end,"_admin1_gsminmax.csv")




## Step 2: Helper functions
# ---- Helpers ----------------------------------------------------------

afn.printinfo <- function(r) {
  plot(r)
  cat("Dimensions:", nrow(r), ncol(r), nlyr(r), "\n")
  cat("Extent:", as.character(ext(r)), "\n")
  cat("Resolution:", res(r)[1], res(r)[2], "\n")
  cat("CRS:", crs(r), "\n")
}

# Load planting/harvest DOY calendars and (optionally) align to a template grid
afn.loadcalendar <- function(dir, matchraster, var_start = "plant", var_end = "harvest",
                             align_to_match = TRUE) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) stop("No calendar files found in: ", dir)
  lapply(files, function(f) {
    r <- c(
      rast(f, subds = var_start),
      rast(f, subds = var_end)
    )
    names(r) <- c(var_start, var_end)
    if (align_to_match) r <- resample(r, matchraster, method = "near")  # DOY is categorical-ish
    r
  })
}

# Stack calendars across crops into plant-stack and harvest-stack (aligned)
stack_calendars <- function(cals) {
  plant   <- do.call(c, lapply(cals, \(r) r[[1]]))
  harvest <- do.call(c, lapply(cals, \(r) r[[2]]))
  names(plant)   <- paste0("plant_",   seq_len(nlyr(plant)))
  names(harvest) <- paste0("harvest_", seq_len(nlyr(harvest)))
  list(plant = plant, harvest = harvest)
}

# Build a GS mask (0/1) for a single DOY (union over crops), with pure raster algebra
gs_mask_for_doy <- function(plant_stack, harvest_stack, doy) {
  stopifnot(nlyr(plant_stack) == nlyr(harvest_stack))
  n <- nlyr(plant_stack)
  
  # Start with all FALSE
  gs <- plant_stack[[1]] == -99999  # always FALSE logical raster (since DOY won't be -99999)
  
  for (i in seq_len(n)) {
    p <- plant_stack[[i]]
    h <- harvest_stack[[i]]
    
    # Conditions (all vectorised on rasters):
    # - normal season: p<=h & p<=doy<=h
    # - wrap season:   p>h  & (doy>=p | doy<=h)
    normal <- (p <= h) & (p <= doy) & (doy <= h)
    wrap   <- (p >  h) & ( (doy >= p) | (doy <= h) )
    
    m <- (normal | wrap)
    
    # Union across crops (logical OR); NA-safe
    gs <- ifel(is.na(gs), m, ifel(is.na(m), gs, (gs | m)))
  }
  
  # Convert logical to {0,1}
  classify(gs, rbind(c(0, 0, 0), c(1, 1, 1)))
}

# Core: compute GS-restricted, cropland-weighted monthly means; then per-year min/max
fast_spei_yearly_minmax_gs <- function(spei, cropland, cals, units) {
  
  # 1) Align grids to SPEI
  cropland_a <- resample(cropland, spei, method = "bilinear")
  cal_stacks <- stack_calendars(cals)
  cal_stacks$plant   <- resample(cal_stacks$plant,   spei, method = "near")
  cal_stacks$harvest <- resample(cal_stacks$harvest, spei, method = "near")
  units_spei <- project(units, crs(spei))
  
  # 2) Cropland area per cell (km^2 × fraction)
  cropland_area <- cellSize(cropland_a, unit = "km") * clamp(cropland_a, 0, 1)
  names(cropland_area) <- "croparea"
  
  # 3) Dates & DOY
  dts  <- as.Date(time(spei))
  doys <- as.integer(strftime(dts, "%j"))
  nL   <- nlyr(spei)
  
  # 4) GS mask stack (0/1 → convert 0 to NA for masking)
  gs_layers <- vector("list", nL)
  for (i in seq_len(nL)) {
    gs_layers[[i]] <- gs_mask_for_doy(cal_stacks$plant, cal_stacks$harvest, doys[i])
  }
  gs_mask <- rast(gs_layers); names(gs_mask) <- names(spei)
  gs_mask_na <- classify(gs_mask, rbind(c(0, 0, NA), c(1, 1, 1)))
  
  # 5) Mask SPEI to GS months; build numerator/denominator for weighted mean
  spei_gs <- mask(spei, gs_mask_na)                 # GS-only SPEI
  num <- spei_gs * cropland_area                    # ∑ SPEI × cropland_area
  den <- gs_mask_na * cropland_area                 # ∑ cropland_area (only GS pixels)
  
  # 6) Zonal sums (fast)
  num_df <- terra::extract(num, units_spei, fun = sum, na.rm = TRUE, exact = TRUE, ID = TRUE)
  den_df <- terra::extract(den, units_spei, fun = sum, na.rm = TRUE, exact = TRUE, ID = TRUE)
  
  # 7) Monthly cropland-weighted mean per polygon
  mdf <- as_tibble(num_df) |>
    rename(ID = ID) |>
    pivot_longer(-ID, names_to = "layer", values_to = "num") |>
    left_join(
      as_tibble(den_df) |>
        rename(ID = ID) |>
        pivot_longer(-ID, names_to = "layer", values_to = "den"),
      by = c("ID","layer")
    ) |>
    mutate(
      date       = dts[match(layer, names(spei))],
      year       = as.integer(format(date, "%Y")),
      spei_wmean = ifelse(den > 0, num/den, NA_real_)
    )
  
  # 8) Annual min/max across GS months
  yearly <- mdf |>
    group_by(ID, year) |>
    summarise(
      minspei_gs  = suppressWarnings(min(spei_wmean, na.rm = TRUE)),
      maxspei_gs  = suppressWarnings(max(spei_wmean, na.rm = TRUE)),
      n_months_gs = sum(!is.na(spei_wmean)),
      .groups = "drop"
    )
  
  yearly$minspei_gs[is.infinite(yearly$minspei_gs)] <- NA
  yearly$maxspei_gs[is.infinite(yearly$maxspei_gs)] <- NA
  yearly
}

# ---- LOAD DATA ---------------------------------------------------------

# SPEI
spei_all <- rast(spei_path)                 # use subds="spei" if your file has multiple variables
stopifnot(!is.null(time(spei_all)))
tt  <- time(spei_all)
yrs <- as.integer(format(tt, "%Y"))
sel <- which(yrs >= year_start & yrs <= year_end)
if (length(sel) == 0) stop("No SPEI layers between ", year_start, " and ", year_end)

spei <- spei_all[[sel]]
if (!is.null(aoi_extent)) spei <- crop(spei, aoi_extent)

# Cropland (fraction 0..1). Optional pre-aggregation to speed up:
cropland <- rast(cropland_fp)
if (!is.null(aoi_extent)) cropland <- crop(cropland, aoi_extent)
# Optional speed knob (uncomment to coarsen cropland by 2× in each axis)
# cropland <- aggregate(cropland, fact = 2, fun = mean)

# Polygons (admin units / countries)
library(sf)
library(lwgeom)

units_sf <- st_read(units_path, quiet = TRUE) %>%
  #filter(nm_slc0 != "Sub-Saharan Africa") %>%
  select(-farm_sz, -frm2020, -nm_slc0) %>% unique()

# 1) Basic fixes in lon/lat are fine
units_sf <- st_make_valid(units_sf)
units_sf <- units_sf[!st_is_empty(units_sf), , drop = FALSE]
units_sf <- st_collection_extract(units_sf, "POLYGON", warn = FALSE)

# 2) Switch to a projected CRS for robust ops (meters)
units_sf <- units_sf %>%
  st_transform(3857) %>%
  st_buffer(0) %>%           # classic self-intersection fix in planar coords
  st_make_valid()

# 3) SNAP TO GRID (now projected, so allowed). Size in meters.
units_sf <- units_sf %>%
  st_set_precision(1) %>%                    # optional; sets intent, not required
  lwgeom::st_snap_to_grid(size = 1) %>%      # 1 m grid; use 5–10 m if still noisy
  st_make_valid()

# 4) Optional: drop tiny slivers (< ~3 km^2); use an equal-area CRS for areas
areas_km2 <- units_sf %>%
  st_transform(6933) %>%   # World Cylindrical Equal Area
  st_area() %>% as.numeric() / 1e6
units_sf <- units_sf[areas_km2 > 3, , drop = FALSE]

# 5) Back to WGS84 for raster ops (terra will reproject to SPEI anyway)
units_sf <- st_transform(units_sf, 4326)

# 6) Antimeridian-safe wrap (only if you truly span ±180°)
# units_sf <- lwgeom::st_wrap_dateline(units_sf,
#                options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
#             st_make_valid()

# → convert to terra using the CLEAN object (don’t re-read the path)
units <- terra::vect(units_sf)
units$ID <- seq_len(nrow(units))
units <- terra::project(units, terra::crs(spei))

# Calendars (align to cropland grid at load time → avoids costly regridding later)
cals <- afn.loadcalendar(calendar_dir, matchraster = cropland, align_to_match = TRUE)
if (!is.null(aoi_extent)) {
  cals <- lapply(cals, crop, y = aoi_extent)
}

# ---- RUN ---------------------------------------------------------------

yearly_minmax <- fast_spei_yearly_minmax_gs(spei, cropland, cals, units)

# Save
write.csv(yearly_minmax, out_csv, row.names = FALSE)



