---
title: "Untitled"
author: "Anne-Marie Parkinson"
date: "February 22, 2020"
output: html_document
---




```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.2.1     v purrr   0.3.3
## v tibble  2.1.3     v dplyr   0.8.3
## v tidyr   1.0.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.4.0
```

```
## Warning: package 'forcats' was built under R version 3.6.2
```

```
## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(sf) 
```

```
## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3
```

```r
library(gganimate)
library(transformr) #need?
```

```
## 
## Attaching package: 'transformr'
```

```
## The following object is masked from 'package:sf':
## 
##     st_normalize
```

```r
library(magick) #need?
```

```
## Linking to ImageMagick 6.9.9.14
## Enabled features: cairo, freetype, fftw, ghostscript, lcms, pango, rsvg, webp
## Disabled features: fontconfig, x11
```

```r
library(here)
```

```
## here() starts at C:/Users/amlpa/OneDrive/Bren 2019_2020/ESM 244 Advanced Data Science/Shiny-App-Assignment
```

```r
library(raster) #need?
```

```
## Warning: package 'raster' was built under R version 3.6.2
```

```
## Loading required package: sp
```

```
## Warning: package 'sp' was built under R version 3.6.2
```

```
## 
## Attaching package: 'raster'
```

```
## The following object is masked from 'package:gganimate':
## 
##     animate
```

```
## The following object is masked from 'package:janitor':
## 
##     crosstab
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
library(tmap)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:here':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
library(lwgeom)
```

```
## Warning: package 'lwgeom' was built under R version 3.6.2
```

```
## Linking to liblwgeom 2.5.0dev r16016, GEOS 3.6.1, PROJ 4.9.3
```

```r
library(ggthemes)

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016")
fire <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) 
```

```
## Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, :
## GDAL Message 1: organizePolygons() received an unexpected geometry. Either
## a polygon with interior rings, or a polygon with less than 4 points, or a
## non-Polygon geometry. Return arguments as a collection.
```

```
## Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, :
## GDAL Message 1: Geometry of polygon of fid 17578 cannot be translated to
## Simple Geometry. All polygons will be contained in a multipolygon.
```

```r
fire <- fire %>% 
  clean_names() %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() %>% 
  mutate (fire_name = str_to_title(fire_name))
fire <- fire %>% 
  clean_names() %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() %>% 
  mutate (fire_name = str_to_title(fire_name)) %>% 
  dplyr::filter (acres > 200) %>% 
  st_simplify(dTolerance = 100) %>% 
  mutate(year = as.numeric(year)) 
```

```
## Warning in st_collection_extract.sf(.): x is already of type POLYGON.
```

```r
ggplot() +
    #geom_sf(data = ca_border, color = "grey80") +
    geom_sf(data = fire, fill = "red", alpha = 0.8, color = "red") +
    theme_classic() +
    theme_map() 
```

<img src="gganimate_map_draft_files/figure-html/unnamed-chunk-1-1.png" width="672" />

```r
    #labs(title = "Year: {frame_time}") +
    #transition_time(year)
```

