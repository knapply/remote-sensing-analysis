Sentinel 2 Band Info
================
Brendan Knapp
May 6, 2018

``` r
library(tidyverse)
library(rvest)
```

``` r
target_url <- "https://en.wikipedia.org/wiki/Sentinel-2"
table_css <- "#mw-content-text > div > table.wikitable"

sentinel2_bands <- target_url %>%
  read_html() %>% 
  html_nodes(table_css) %>% 
  html_table() %>%
  as.data.frame() %>% 
  as_tibble() %>% 
  set_names(c("band", "central_wavelength",
              "resolution", "bandwidth")) %>% 
  mutate(band_number = band %>% 
           str_extract("\\d+A?") %>% 
           str_pad(2, pad = "0")
         ) %>% 
  mutate(band = band %>% 
           str_remove("^.*?(-|–)\\s+") %>% 
           str_trim()
         ) %>% 
  mutate(band_number = if_else(band == "Narrow NIR", "08A", band_number)) %>% 
  mutate(platform = "sentinel2") %>% 
  select(band_number, everything())
```

``` r
sentinel2_bands %>% 
  knitr::kable(format = "markdown")
```

| band\_number | band                |  central\_wavelength|  resolution|  bandwidth| platform  |
|:-------------|:--------------------|--------------------:|-----------:|----------:|:----------|
| 01           | Coastal aerosol     |                0.443|          60|         20| sentinel2 |
| 02           | Blue                |                0.490|          10|         65| sentinel2 |
| 03           | Green               |                0.560|          10|         35| sentinel2 |
| 04           | Red                 |                0.665|          10|         30| sentinel2 |
| 05           | Vegetation Red Edge |                0.705|          20|         15| sentinel2 |
| 06           | Vegetation Red Edge |                0.740|          20|         15| sentinel2 |
| 07           | Vegetation Red Edge |                0.783|          20|         20| sentinel2 |
| 08           | NIR                 |                0.842|          10|        115| sentinel2 |
| 08A          | Narrow NIR          |                0.865|          20|         20| sentinel2 |
| 09           | Water vapour        |                0.945|          60|         20| sentinel2 |
| 10           | SWIR – Cirrus       |                1.375|          60|         20| sentinel2 |
| 11           | SWIR                |                1.610|          20|         90| sentinel2 |
| 12           | SWIR                |                2.190|          20|        180| sentinel2 |

``` r
write_rds(sentinel2_bands, "data/sentinel2_bands.rds")
```
