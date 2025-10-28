#Green areas analysis
#Date: October, 2025
#Objective: Create statics combined maps

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(magick)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Read both images
london <- image_read("./02_intermediate_data/green/map/static/gbr/glondon_v2.png")
lima   <- image_read("./02_intermediate_data/green/map/static/per/glima_v1.png")

# --- 2. Resize both to same height (important for alignment) ---
target_height <- min(image_info(london)$height, image_info(lima)$height)
london <- image_scale(london, paste0("x", target_height))
lima   <- image_scale(lima,   paste0("x", target_height))

# Option 1 — Side-by-side (landscape)
combined_side <- image_append(c(london, lima))

# Option 2 — One on top of the other (portrait)
combined_stack <- image_append(c(london, lima), stack = TRUE)

# Save both versions to compare
image_write(combined_side, path = "./02_intermediate_data/green/map/static/glondon_lima_v1.png", format = "png")
image_write(combined_stack, path = "./02_intermediate_data/green/map/static/glondon_lima_v2.png", format = "png")

# Optional: resize for LinkedIn post (width ~1600px)
linkedin_img <- image_scale(combined_side, "1600")
image_write(linkedin_img, path = "./02_intermediate_data/green/map/static/glondon_lima_v1_in.png", format = "png")
