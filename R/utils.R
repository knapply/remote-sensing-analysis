sf_crop <- function(raster, sf){
  crs_raster <- as.character(crs(raster))
  sf <- st_transform(sf, crs_raster)
  crop(raster, as(sf, "Spatial"))
}

match_bands <- function(rasters){
  target_res <- rasters %>% 
    map(res) %>% 
    unlist() %>% 
    unique() %>% 
    min()
  
  for(raster in rasters){
    if(res(raster)[[1]] == target_res) target_raster <- raster
  }
  
  rasters %>% 
    map_if(~ !identical(.x, target_raster), resample, target_raster)
}

cust_pal <- RColorBrewer::brewer.pal(11, "Spectral") %>% 
  rev() %>% 
  {colorRampPalette(.)(255)}


cust_plot <- function(raster_layer){
    image(raster_layer,
          col = cust_pal, 
          ann = FALSE,
          axes = FALSE)
    # title(main = names(raster_layer))
    # plot(vector_sf$geometry, col = vector_color, lwd = lwd,
    #      axes = FALSE,
    #      add = TRUE)
  }


