# ============================================================
# SCRIPT NAME:
# 00_oceanMask.R
#
# PURPOSE:
# Create an ocean mask from the GEBCO bathymetry NetCDF,
# cropped to the study area defined in bbox_env.txt
#
# INPUT:
# - 00inputOutput/00input/00rawData/00enviro/00StaticLayers/GEBCO_2014_2D.nc
# - 00inputOutput/00input/00rawData/01tracking/00auxiliaryFiles/bbox_env.txt
#
# OUTPUT:
# - 00inputOutput/00input/00rawData/00enviro/oceanmask.tif
#
# MASK VALUES:
# - 1 = ocean
# - 0 = land
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(here)
})

message("Starting ocean mask creation...")

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

bathy_nc <- here(
  "00inputOutput", "00input", "00rawData", "00enviro",
  "00StaticLayers", "GEBCO_2014_2D.nc"
)

bbox_file <- here(
  "00inputOutput", "00input", "00rawData", "01tracking",
  "00auxiliaryFiles", "bbox_env.txt"
)

out_dir <- here(
  "00inputOutput", "00input", "00rawData", "00enviro"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

oceanmask_out <- file.path(out_dir, "oceanmask.tif")

if (!file.exists(bathy_nc)) {
  stop("Bathymetry NetCDF not found: ", bathy_nc)
}

if (!file.exists(bbox_file)) {
  stop("bbox_env.txt not found: ", bbox_file)
}

# ------------------------------------------------------------
# 2. READ BBOX
# Expected format:
# MIN_LON=...
# MAX_LON=...
# MIN_LAT=...
# MAX_LAT=...
# ------------------------------------------------------------

bbox_lines <- readLines(bbox_file)

get_bbox_value <- function(prefix, x) {
  line <- x[grepl(paste0("^", prefix, "="), x)]
  if (length(line) != 1) {
    stop("Could not find unique entry for ", prefix, " in ", bbox_file)
  }
  as.numeric(sub(paste0("^", prefix, "="), "", line))
}

min_lon <- get_bbox_value("MIN_LON", bbox_lines)
max_lon <- get_bbox_value("MAX_LON", bbox_lines)
min_lat <- get_bbox_value("MIN_LAT", bbox_lines)
max_lat <- get_bbox_value("MAX_LAT", bbox_lines)

message("Using bbox:")
message("  LON: ", min_lon, " to ", max_lon)
message("  LAT: ", min_lat, " to ", max_lat)

# ------------------------------------------------------------
# 3. READ GEBCO
# ------------------------------------------------------------

bathy <- rast(bathy_nc)

if (nlyr(bathy) > 1) {
  bathy <- bathy[[1]]
}

names(bathy) <- "bathymetry"

# ------------------------------------------------------------
# 4. CROP TO STUDY AREA
# ------------------------------------------------------------

target_ext <- ext(min_lon, max_lon, min_lat, max_lat)

bathy_crop <- crop(bathy, target_ext)

if (is.null(bathy_crop)) {
  stop("Crop returned NULL")
}

vals_crop <- values(bathy_crop)

if (length(vals_crop) == 0 || all(is.na(vals_crop))) {
  stop("Cropped bathymetry has no valid values")
}

# ------------------------------------------------------------
# 5. CREATE OCEAN MASK
# Ocean = bathymetry < 0
# Land  = bathymetry >= 0 or NA
# ------------------------------------------------------------

oceanmask <- ifel(!is.na(bathy_crop) & bathy_crop < 0, 1, 0)
names(oceanmask) <- "oceanmask"

# ------------------------------------------------------------
# 6. SAVE
# ------------------------------------------------------------

writeRaster(
  oceanmask,
  filename = oceanmask_out,
  overwrite = TRUE
)

message("Ocean mask saved at:")
message("  ", oceanmask_out)

# ------------------------------------------------------------
# 7. FINAL MESSAGE
# ------------------------------------------------------------

message("====================================")
message("Ocean mask created successfully")
message("1 = ocean")
message("0 = land")
message("====================================")