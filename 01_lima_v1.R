#Green areas analysis
#Date: October, 2025
#Objective: Calculate green areas in Lima using INEI and OMS

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(osmdata)
library(sf)
library(dplyr)
library(tidyverse)
library(units)
library(ggplot2)
library(ggspatial)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------
# 1. Load Greater London boundary from ONS GeoJSON
# -----------------------------
lima_lad <- st_read("./00_raw_data/geo/per/DISTRITO.gpkg")

lima_lad |> map_dbl(~length(unique(.))) #1891 districts
lima_lad |> map_dbl(~sum(is.na(.)))
#Lima province has 43 districts and Callao has 7 districts

# Filter Lima province and Callao
lima_prov <- lima_lad |>
  filter(NOMBPROV %in% c("LIMA", "CALLAO")) |>
  st_union() |>
  st_make_valid() |>
  st_as_sf(crs = 4326)

#Save the polygon for Greater London
st_write(
  st_as_sf(lima_prov),
  "./02_intermediate_data/green/per/lima_v1.gpkg",
  delete_dsn = TRUE
)

#Check the geography
#opq(bbox = "Lima, Peru") #This is given a smaller area than Provincia de Lima
#opq(bbox = "Provincia de Lima, Peru")
#opq(bbox = "Provincia del Callao, Peru")

#===============================================================================
#Step 2. Define function to fetch + clip green features
fetch_green <- function(key, values, places = c("Provincia de Lima, Peru", "Provincia del Callao, Peru"), boundary = lima_prov) {
  
  all_polys <- list()
  
  # Loop through each place
  for (place in places) {
    q <- opq(place) %>%
      add_osm_feature(key = key, value = values)
    
    dat <- osmdata_sf(q)
    
    polys <- dat$osm_polygons
    if (!is.null(polys) && nrow(polys) > 0) {
      polys <- st_transform(polys, st_crs(boundary))
      polys$key <- key
      polys$value <- polys[[key]]
      
      all_polys[[length(all_polys) + 1]] <- polys
    }
  }
  
  # Combine all polygons if any were found
  if (length(all_polys) > 0) {
    combined_polys <- bind_rows(all_polys)  # Changed from do.call(rbind, ...)
    
    # Remove duplicates by osm_id
    combined_polys <- combined_polys[!duplicated(combined_polys$osm_id), ]
    
    # Clip to official INEI boundary
    polys_clipped <- st_intersection(combined_polys, boundary)
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
class(green_areas)
table(green_areas$key)
table(green_areas$value)

#===============================================================================
#Step 4. Calculate coverage
# Total areas
lima_area <- st_area(lima_prov)
green_area <- st_area(green_areas)

green_share <- sum(green_area) / lima_area * 100
cat("Total green area share of Lima (%):", round(green_share, 2), "\n")

#Total green area share of Lima (%): 11.3

#===============================================================================
#Step 5. Map it
paa <- ggplot() +
  geom_sf(data = lima_prov, fill = "white", color = "black", size = 0.6) +
  geom_sf(data = green_areas, fill = "darkgreen", color = NA, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Green Areas within Metropolitan Lima",
    subtitle = "White = INEI Lima Province boundary | Green = OSM green features",
    caption = "Source: INEI 2023 + OSM"
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
  filename = "./02_intermediate_data/green/map/static/per/glima_v1.png",
  plot = paa,
  width = 8, height = 6, dpi = 300
)

#===============================================================================
# Save the vector (polygon)
st_write(green_areas, "./01_clean_data/green/per/green_areas_lima.gpkg", delete_dsn = TRUE)
