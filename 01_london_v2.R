#Green areas analysis
#Date: October, 2025
#Objective: Calculate green areas in London using ONS and OMS adding Green Belt

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(osmdata)
library(sf)
library(dplyr)
library(units)
library(ggplot2)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# 1. Paths
# ---------------------------------------------------------------------
ons_lad_geojson <- "./00_raw_data/geo/uk/Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-6307115499537197728.geojson"
greenbelt_local <- "./00_raw_data/geo/uk/green-belt.geojson"
green_areas_gpkg <- "./01_clean_data/green/gbr/green_areas_london.gpkg"
outdir <- "./02_intermediate_data/green/gbr/"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 2. Load Greater London boundary
# ---------------------------------------------------------------------
ons_lads <- st_read(ons_lad_geojson, quiet = TRUE)

greater_london <- ons_lads |>
  filter(LAD24CD %in% paste0("E090000", sprintf("%02d", 1:33))) |>
  st_make_valid() |>
  st_union() |>
  st_as_sf() |>
  mutate(name = "Greater London")

#Save the polygon for Greater London
st_write(
  st_as_sf(greater_london),
  "./02_intermediate_data/green/gbr/greater_london_v1.gpkg",
  delete_dsn = TRUE
)

# ---------------------------------------------------------------------
# 3. Load datasets
# ---------------------------------------------------------------------
green_areas <- st_read(green_areas_gpkg, quiet = TRUE)
greenbelt_all <- st_read(greenbelt_local, quiet = TRUE)

# ---------------------------------------------------------------------
# 4. Reproject all to British National Grid (EPSG 27700)
# ---------------------------------------------------------------------
proj_crs <- 27700
greater_london <- st_transform(greater_london, proj_crs)
green_areas <- st_transform(green_areas, proj_crs)
greenbelt_all <- st_transform(greenbelt_all, proj_crs)

# ---------------------------------------------------------------------
# 5. Clip Green Belt to Greater London boundary
# ---------------------------------------------------------------------
greenbelt_london <- st_make_valid(greenbelt_all) |>
  st_intersection(greater_london)

# ---------------------------------------------------------------------
# 6. Compute individual areas (for diagnostics)
# ---------------------------------------------------------------------
london_area_m2 <- as.numeric(st_area(greater_london))
green_osm_m2    <- sum(as.numeric(st_area(green_areas)))
greenbelt_m2    <- sum(as.numeric(st_area(greenbelt_london)))

cat("Greater London area (km²):", round(set_units(london_area_m2 / 1e6, km^2), 1), "\n")
cat("OSM green area (km²):", round(set_units(green_osm_m2 / 1e6, km^2), 1), "\n")
cat("Green Belt (km²):", round(set_units(greenbelt_m2 / 1e6, km^2), 1), "\n")

# ---------------------------------------------------------------------
# 7. Combine OSM green + Green Belt (safe, memory-efficient)
# ---------------------------------------------------------------------
# Combine and union once more
combined_london <- st_union(st_sfc(c(st_geometry(green_areas),
                                     st_geometry(greenbelt_london)),
                                   crs = proj_crs))

combined_london <- st_intersection(st_make_valid(combined_london), greater_london)

# check
print("✅ Combined geometry successfully created.")
print(st_area(combined_london) / 1e6)

# ---------------------------------------------------------------------
# 8. Final area results
# ---------------------------------------------------------------------
combined_area_m2 <- as.numeric(st_area(combined_london))
share_total <- 100 * combined_area_m2 / london_area_m2
cat("\nTotal green area share (OSM + Green Belt) of London: ",
    round(share_total, 2), "%\n")

#Total green area share London (OSM + Green Belt) of London:  33.65%

# ---------------------------------------------------------------------
# 9. Save combined layer
# ---------------------------------------------------------------------
st_write(
  st_as_sf(combined_london),
  paste0(outdir, "london_osm_greenbelt_v1.gpkg"),
  delete_dsn = TRUE
  )

cat("✅ Combined green layer saved to:", paste0(outdir, "london_osm_greenbelt_v1.gpkg"), "\n")

# Ensure valid
combined_london <- st_make_valid(combined_london)

# Convert GEOMETRYCOLLECTION to MULTIPOLYGON
combined_london <- st_collection_extract(combined_london, "POLYGON")

# Check
st_geometry_type(combined_london)

st_write(
  st_as_sf(combined_london),
  "./02_intermediate_data/green/gbr/london_osm_greenbelt_v2.gpkg",
  delete_dsn = TRUE
)

#======================================================================
#Save the polygon for Greater London
st_write(
  st_as_sf(greater_london),
  "./02_intermediate_data/green/gbr/greater_london_v2.gpkg",
  delete_dsn = TRUE
)

#===============================================================================
#Map green areas
pab <- ggplot() +
  geom_sf(data = greater_london, fill = "white", color = "black", size = 0.6) +
  geom_sf(data = combined_london, fill = "darkgreen", color = NA, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Green Areas within Greater London",
    subtitle = "White = ONS Greater London boundary | Green = OSM green features and Green Belt",
    caption = "Source: ONS LAD 2024 + OSM"
  ) +
  theme(
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_line(color = "grey30"),
    #Explicit dark background (no transparency)
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA)
  )

# Save map
ggsave(
  filename = "./02_intermediate_data/green/map/static/gbr/glondon_v2.png",
  plot = pab,
  width = 8, height = 6, dpi = 300
)
