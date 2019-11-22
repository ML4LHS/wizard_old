WIndowed Summarization for Auto Regressive Data (WISARD)
================

The purpose for this package is to provide an all in one data
transformation tool intended for use with machine learning applications.

Features:

  - Windowed summaries - Downsampling method for

# Installation

``` r
# install.packages("devtools")
library("devtools")
devtools::install_github("seameyer-umich/wisard")
```

# Quick Start

## Temporal Data Format

``` r
read_csv("./inst/extdata/temporal_data.csv") %>% 
  head()
```

    ## Parsed with column specification:
    ## cols(
    ##   patient_id = col_character(),
    ##   encounter_id = col_character(),
    ##   time = col_datetime(format = ""),
    ##   category = col_character(),
    ##   variable = col_character(),
    ##   value = col_double()
    ## )

    ## # A tibble: 6 x 6
    ##   patient_id      encounter_id      time                category variable  value
    ##   <chr>           <chr>             <dttm>              <chr>    <chr>     <dbl>
    ## 1 38B3EFF8BAF566~ C81E728D9D4C2F63~ 2019-04-04 06:06:00 vitals   NIBP Pul~   124
    ## 2 38B3EFF8BAF566~ C4CA4238A0B92382~ 2019-04-08 08:49:00 vitals   SpO2         92
    ## 3 38B3EFF8BAF566~ ECCBC87E4B5CE2FE~ 2019-04-02 18:33:00 vitals   NIBP Sys    108
    ## 4 38B3EFF8BAF566~ ECCBC87E4B5CE2FE~ 2019-04-05 17:02:00 vitals   SpO2         90
    ## 5 EC8956637A9978~ ECCBC87E4B5CE2FE~ 2019-04-03 16:24:00 vitals   NIBP Dias   115
    ## 6 38B3EFF8BAF566~ ECCBC87E4B5CE2FE~ 2019-04-05 01:44:00 vitals   NIBP Pul~    88

## Fixed Data Format

``` r
read_csv("./inst/extdata/fixed_data.csv") %>% 
  head()
```

    ## Parsed with column specification:
    ## cols(
    ##   patient_id = col_character(),
    ##   encounter_id = col_character(),
    ##   admit_time = col_datetime(format = ""),
    ##   outcome = col_datetime(format = "")
    ## )

    ## # A tibble: 6 x 4
    ##   patient_id         encounter_id        admit_time          outcome            
    ##   <chr>              <chr>               <dttm>              <dttm>             
    ## 1 38B3EFF8BAF566274~ A87FF679A2F3E71D91~ 2019-04-01 00:39:00 2019-04-09 19:57:00
    ## 2 38B3EFF8BAF566274~ C4CA4238A0B923820D~ 2019-04-01 01:36:00 2019-04-09 22:41:00
    ## 3 38B3EFF8BAF566274~ C81E728D9D4C2F636F~ 2019-04-01 02:56:00 2019-04-09 23:34:00
    ## 4 38B3EFF8BAF566274~ ECCBC87E4B5CE2FE28~ 2019-04-01 03:03:00 2019-04-09 21:59:00
    ## 5 EC8956637A99787BD~ A87FF679A2F3E71D91~ 2019-04-01 02:47:00 2019-04-09 23:17:00
    ## 6 EC8956637A99787BD~ C4CA4238A0B923820D~ 2019-04-01 00:21:00 2019-04-09 23:32:00
