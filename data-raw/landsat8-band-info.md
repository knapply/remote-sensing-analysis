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

landsat8_bands <- target_url %>%
  read_html() %>% 
  html_nodes(table_css) %>% 
  html_table() %>% 
  flatten() %>% 
  as_tibble() %>% 
  rename_all(funs(str_to_lower(str_replace_all(., " ", "_")))) %>% 
  separate(wavelength, c("min_wavelength", "max_wavelength"), 
           sep = "-|–") %>% 
  mutate_at(vars(min_wavelength, max_wavelength), 
            funs(as.numeric(str_trim(.)))) %>% 
  mutate(band_number = band %>% 
           str_extract("\\d+") %>% 
           as.numeric()
         ) %>% 
  mutate(band = band %>% 
           str_remove("^.*?(-|–)\\s+") %>% 
           str_trim()
         ) %>% 
  select(band_number, everything())
```

``` r
landsat8_bands %>% 
  knitr::kable(format = "markdown")
```

<table>
<colgroup>
<col width="7%" />
<col width="17%" />
<col width="9%" />
<col width="9%" />
<col width="57%" />
</colgroup>
<thead>
<tr class="header">
<th align="right">band_number</th>
<th align="left">band</th>
<th align="right">min_wavelength</th>
<th align="right">max_wavelength</th>
<th align="left">useful_for_mapping</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">Coastal Aerosol</td>
<td align="right">0.435</td>
<td align="right">0.451</td>
<td align="left">Coastal and aerosol studies</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">Blue</td>
<td align="right">0.452</td>
<td align="right">0.512</td>
<td align="left">Bathymetric mapping, distinguishing soil from vegetation, and deciduous from coniferous vegetation</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">Green</td>
<td align="right">0.533</td>
<td align="right">0.590</td>
<td align="left">Emphasizes peak vegetation, which is useful for assessing plant vigor</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">Red</td>
<td align="right">0.636</td>
<td align="right">0.673</td>
<td align="left">Discriminates vegetation slopes</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">Near Infrared (NIR)</td>
<td align="right">0.851</td>
<td align="right">0.879</td>
<td align="left">Emphasizes biomass content and shorelines</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">Short-wave Infrared (SWIR) 1</td>
<td align="right">1.566</td>
<td align="right">1.651</td>
<td align="left">Discriminates moisture content of soil and vegetation; penetrates thin clouds</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">Short-wave Infrared (SWIR) 2</td>
<td align="right">2.107</td>
<td align="right">2.294</td>
<td align="left">Improved moisture content of soil and vegetation and thin cloud penetration</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">Panchromatic</td>
<td align="right">0.503</td>
<td align="right">0.676</td>
<td align="left">15 meter resolution, sharper image definition</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">Cirrus</td>
<td align="right">1.363</td>
<td align="right">1.384</td>
<td align="left">Improved detection of cirrus cloud contamination</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">TIRS 1</td>
<td align="right">10.600</td>
<td align="right">11.190</td>
<td align="left">100 meter resolution, thermal mapping and estimated soil moisture</td>
</tr>
<tr class="odd">
<td align="right">11</td>
<td align="left">TIRS 2</td>
<td align="right">11.500</td>
<td align="right">12.510</td>
<td align="left">100 meter resolution, Improved thermal mapping and estimated soil moisture</td>
</tr>
</tbody>
</table>

``` r
write_rds(landsat8_bands, "data/landsat8_bands.rds")
```
