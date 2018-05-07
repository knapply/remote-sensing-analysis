Landsat 8 Band Info
================
Brendan Knapp
May 6, 2018

``` r
library(tidyverse)
library(rvest)
```

``` r
target_url <- "https://landsat.usgs.gov/what-are-best-spectral-bands-use-my-study"

table_css <- "#node-3373 > div > div.field.field-name-body.field-type-text-with-summary.field-label-hidden > div > div > table:nth-child(5)"

bands_15m_res <- c(8)
bands_30m_res <- c(1:6, 7, 9)
bands_100m_res <- c(10, 11)


landsat8_bands <- target_url %>%
  read_html() %>% 
  html_nodes(table_css) %>% 
  html_table() %>% 
  flatten() %>% 
  as_tibble() %>% 
  rename_all(funs(str_to_lower(str_replace_all(., " ", "_")))) %>% 
  separate(wavelength, c("min_wavelength", "max_wavelength"), 
           sep = "-|–") %>% 
  mutate_at(vars(min_wavelength, max_wavelength), # in nm
            funs(as.numeric(str_trim(.)))) %>% 
  rowwise() %>% 
  mutate(central_wavelength = mean(min_wavelength, max_wavelength)) %>% 
  ungroup() %>% 
  mutate(band_number = band %>% 
           str_extract("\\d+")
         ) %>% 
  mutate(band = band %>% 
           str_remove("^.*?(-|–)\\s+") %>% 
           str_trim()
         ) %>% 
  mutate(resolution = case_when(
    band_number %in% bands_15m_res ~ 15,
    band_number %in% bands_30m_res ~ 30,
    band_number %in% bands_100m_res ~ 100
    )) %>% 
  mutate(platform = "landsat8") %>% 
  select(band_number, everything())
```

``` r
landsat8_bands %>% 
  knitr::kable(format = "markdown")
```

<table style="width:100%;">
<colgroup>
<col width="5%" />
<col width="13%" />
<col width="7%" />
<col width="7%" />
<col width="46%" />
<col width="9%" />
<col width="5%" />
<col width="4%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">band_number</th>
<th align="left">band</th>
<th align="right">min_wavelength</th>
<th align="right">max_wavelength</th>
<th align="left">useful_for_mapping</th>
<th align="right">central_wavelength</th>
<th align="right">resolution</th>
<th align="left">platform</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1</td>
<td align="left">Coastal Aerosol</td>
<td align="right">0.435</td>
<td align="right">0.451</td>
<td align="left">Coastal and aerosol studies</td>
<td align="right">0.435</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="even">
<td align="left">2</td>
<td align="left">Blue</td>
<td align="right">0.452</td>
<td align="right">0.512</td>
<td align="left">Bathymetric mapping, distinguishing soil from vegetation, and deciduous from coniferous vegetation</td>
<td align="right">0.452</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="odd">
<td align="left">3</td>
<td align="left">Green</td>
<td align="right">0.533</td>
<td align="right">0.590</td>
<td align="left">Emphasizes peak vegetation, which is useful for assessing plant vigor</td>
<td align="right">0.533</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="even">
<td align="left">4</td>
<td align="left">Red</td>
<td align="right">0.636</td>
<td align="right">0.673</td>
<td align="left">Discriminates vegetation slopes</td>
<td align="right">0.636</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="odd">
<td align="left">5</td>
<td align="left">Near Infrared (NIR)</td>
<td align="right">0.851</td>
<td align="right">0.879</td>
<td align="left">Emphasizes biomass content and shorelines</td>
<td align="right">0.851</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="even">
<td align="left">6</td>
<td align="left">Short-wave Infrared (SWIR) 1</td>
<td align="right">1.566</td>
<td align="right">1.651</td>
<td align="left">Discriminates moisture content of soil and vegetation; penetrates thin clouds</td>
<td align="right">1.566</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="odd">
<td align="left">7</td>
<td align="left">Short-wave Infrared (SWIR) 2</td>
<td align="right">2.107</td>
<td align="right">2.294</td>
<td align="left">Improved moisture content of soil and vegetation and thin cloud penetration</td>
<td align="right">2.107</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="even">
<td align="left">8</td>
<td align="left">Panchromatic</td>
<td align="right">0.503</td>
<td align="right">0.676</td>
<td align="left">15 meter resolution, sharper image definition</td>
<td align="right">0.503</td>
<td align="right">15</td>
<td align="left">landsat8</td>
</tr>
<tr class="odd">
<td align="left">9</td>
<td align="left">Cirrus</td>
<td align="right">1.363</td>
<td align="right">1.384</td>
<td align="left">Improved detection of cirrus cloud contamination</td>
<td align="right">1.363</td>
<td align="right">30</td>
<td align="left">landsat8</td>
</tr>
<tr class="even">
<td align="left">10</td>
<td align="left">TIRS 1</td>
<td align="right">10.600</td>
<td align="right">11.190</td>
<td align="left">100 meter resolution, thermal mapping and estimated soil moisture</td>
<td align="right">10.600</td>
<td align="right">100</td>
<td align="left">landsat8</td>
</tr>
<tr class="odd">
<td align="left">11</td>
<td align="left">TIRS 2</td>
<td align="right">11.500</td>
<td align="right">12.510</td>
<td align="left">100 meter resolution, Improved thermal mapping and estimated soil moisture</td>
<td align="right">11.500</td>
<td align="right">100</td>
<td align="left">landsat8</td>
</tr>
</tbody>
</table>

``` r
write_rds(landsat8_bands, "data/landsat8_bands.rds")
```
