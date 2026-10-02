# bake_data.R ----------------------------------------------------------
# Pre-process the raw source data into a single compact .rds file that
# ships with the app, so the deployed version works out of the box
# with no manual downloads.
#
# Inputs (place in data/ before running):
#   data/hcl_2024.csv            — HCL "Detailed results by constituency"
#   data/constituencies.geojson  — ONS Westminster July 2024 BGC
#
# Outputs:
#   data/baked.rds — list(results, boundaries) ready for the app
#
# Run once locally after dropping the raw files in:
#   Rscript data-raw/bake_data.R
# ----------------------------------------------------------------------

library(sf)
# Source the loader so our baked `results` matches load_hcl_2024() exactly.
source("R/data_loader.R")

raw_csv <- "data/hcl_2024.csv"
raw_geo <- "data/constituencies.geojson"
stopifnot(file.exists(raw_csv), file.exists(raw_geo))

cat("Loading HCL CSV...\n")
results <- load_hcl_2024(raw_csv)
cat("  ", nrow(results), "rows across",
    length(unique(results$constituency_id)), "constituencies\n")

cat("Loading boundaries...\n")
shp <- st_read(raw_geo, quiet = TRUE)
cat("  Original object size: ",
    format(object.size(shp), units = "MB"), "\n")

cat("Simplifying geometry (keep ~500 m tolerance in projected metres)...\n")
# Project to British National Grid for metre-scale simplification,
# simplify, then project back to WGS84 for leaflet.
shp_bng <- st_transform(shp, 27700)
shp_bng <- st_simplify(shp_bng, dTolerance = 500, preserveTopology = TRUE)
shp <- st_transform(shp_bng, 4326)

# Keep only the columns the app actually uses.
keep_cols <- intersect(c("PCON24CD", "PCON24NM", "geometry"), names(shp))
shp <- shp[, keep_cols]

cat("  Simplified object size: ",
    format(object.size(shp), units = "MB"), "\n")

baked <- list(
  results    = results,
  boundaries = shp,
  built_at   = Sys.time()
)

out_path <- "data/baked.rds"
saveRDS(baked, out_path, compress = "xz")
cat("\nWrote ", out_path, " (",
    format(file.info(out_path)$size / 1024 / 1024, digits = 3), " MB)\n",
    sep = "")
