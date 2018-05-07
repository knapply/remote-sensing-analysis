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

bulk_fortify <- function(raster, indices = FALSE, date_name){
  if(!indices){
    date_name <- str_subset(names(raster), "\\d{8}")[[1]] %>% 
      str_extract("\\d{8}") %>% 
      str_replace("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3")
    raster_df <- raster %>% 
      fortify() %>% 
      as_tibble() %>% 
      gather(band_number, val, -x, -y) %>% 
      mutate(band_number = str_extract(band_number, "\\d+A?$")) %>% 
      left_join(sentinel2_bands) %>% 
      mutate(date_name = date_name)
  }
  if(indices){
    raster_df <- raster %>%
      fortify() %>%
      as_tibble() %>%
      select(-MNDWI) %>%
      gather(index, val, -x, -y) %>%
      mutate(date_name = date_name) %>% 
      drop_na()
  }
  return(raster_df)
}


