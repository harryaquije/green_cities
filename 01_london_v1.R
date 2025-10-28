#Green areas analysis
#Date: October, 2025
#Objective: Calculate green areas in London using ONS and OMS

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(osmdata)
library(sf)
library(dplyr)
library(units)
library(ggplot2)
library(ggspatial)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------
# 1. Load Greater London boundary from ONS GeoJSON
# -----------------------------
london_lad <- st_read("./00_raw_data/geo/uk/Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-6307115499537197728.geojson")

# Filter Greater London
greater_london <- london_lad |>
  filter(LAD24CD %in% paste0("E090000", sprintf("%02d", 1:33))) |>
  st_union() |>
  st_make_valid() |>
  st_as_sf(crs = 4326)

#===============================================================================
#Step 2. Define function to fetch + clip green features
fetch_green <- function(key, values, place = "Greater London, UK", boundary = greater_london) {
  # Use smaller bounding box to avoid timeout
  q <- opq(place) %>%
    add_osm_feature(key = key, value = values)
  
  dat <- osmdata_sf(q)
  
  polys <- dat$osm_polygons
  if (!is.null(polys) && nrow(polys) > 0) {
    polys <- st_transform(polys, st_crs(boundary))
    polys$key <- key
    polys$value <- polys[[key]]
    
    # Clip to official ONS boundary
    polys_clipped <- st_intersection(polys, boundary)
    return(polys_clipped)
  } else {
    return(NULL)
  }
}

#===============================================================================
#Step 3. Fetch different green categories
green_list <- list(
  fetch_green("leisure", c("park", "garden", "recreation_ground", "pitch", "nature_reserve")),
  fetch_green("landuse", c("forest", "grass", "meadow")),
  fetch_green("natural", c("wood", "heath", "scrub"))
)

# Clean & merge
green_list_clean <- lapply(green_list, function(x) {
  if (!is.null(x)) select(x, osm_id, name, key, value, geometry)
})

green_areas <- do.call(rbind, green_list_clean)

#===============================================================================
#Step 4. Calculate coverage
# Total areas
london_area <- st_area(greater_london)
green_area <- st_area(green_areas)

green_share <- sum(green_area) / london_area * 100
cat("Total green area share of London (%):", round(green_share, 2), "\n")

#Total green area share of London (%): 27.03

#===============================================================================
#Step 5. Map it
paa <- ggplot() +
  geom_sf(data = greater_london, fill = "white", color = "black", size = 0.6) +
  geom_sf(data = green_areas, fill = "darkgreen", color = NA, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Green Areas within Greater London",
    subtitle = "White = ONS Greater London boundary | Green = OSM green features",
    caption = "Source: ONS LAD 2024 + OSM"
  ) +
  theme(
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_line(color = "grey30")
  )

# Save map
ggsave(
  filename = "./02_intermediate_data/green/map/static/gbr/glondon_v1.png",
  plot = paa,
  width = 8, height = 6, dpi = 300
)

#===============================================================================
# Save the vector (polygon)
st_write(green_areas, "./01_clean_data/green/gbr/green_areas_london.gpkg", delete_dsn = TRUE)
