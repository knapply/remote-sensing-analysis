landsat8_bands <- read_rds("data/landsat8_bands.rds")
sentinel2_bands <- read_rds("data/sentinel2_bands.rds")

cust_pal <- RColorBrewer::brewer.pal(11, "Spectral") %>% 
  rev() %>% 
  {colorRampPalette(.)(255)}


cust_theme <- rasterTheme(region = rev(brewer.pal(11, "Spectral")))

