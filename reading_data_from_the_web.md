reading_data_from_the_web
================
Sharon Kulali

loading visualization stuff

``` r
library(tidyverse)
library(patchwork)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

load the necessary libraries

``` r
library(rvest)
library(httr)
```

import NSDUH data

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = 
  read_html(nsduh_url)
```

``` r
marj_use_df =
  nsduh_html |> 
  html_table() |> 
  first() |> 
  slice(-1)
```

import star wars data

``` r
swm_url = "https://www.imdb.com/list/ls070150896/"

swm_html = 
  read_html(swm_url)
```

``` r
swm_title_vec =
  swm_html |> 
  html_elements(".lister-item-header a") |> 
  html_text()

swm_gross_rev_vec =
  swm_html |> 
  html_elements(".text-small:nth-child(7) span:nth-child(5)") |> 
  html_text()

swm_df = 
  tibble(
    title = swm_title_vec,
    gross_rev = swm_gross_rev_vec
  )
```
