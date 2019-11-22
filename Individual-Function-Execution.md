Introduction
================
Adharsh Murali
2019-11-22

# Installation

``` r
# install.packages("devtools")
library("devtools")
devtools::install_github("smeyer-umich/wisard", auth_token = "725cd269dcfd0b64fdeb5aa51a3e0a52193fdd3c")
```

# 1\) Load in the fixed data and temporal data

``` r
library(tidyverse, quietly = TRUE)
#> -- Attaching packages ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.2.1     v purrr   0.3.3
#> v tibble  2.1.3     v dplyr   0.8.3
#> v tidyr   1.0.0     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.4.0
#> -- Conflicts -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

temporal_file = file.path("./inst/extdata/temporal_data.csv")
fixed_file = file.path("./inst/extdata/fixed_data.csv")

temporal_data =  data.table::fread(temporal_file) %>% 
  dplyr::mutate(time = time %>% lubridate::as_datetime())

temporal_data %>% head()
#>                         patient_id                     encounter_id
#> 1 38B3EFF8BAF56627478EC76A704E9B52 C81E728D9D4C2F636F067F89CC14862C
#> 2 38B3EFF8BAF56627478EC76A704E9B52 C4CA4238A0B923820DCC509A6F75849B
#> 3 38B3EFF8BAF56627478EC76A704E9B52 ECCBC87E4B5CE2FE28308FD9F2A7BAF3
#> 4 38B3EFF8BAF56627478EC76A704E9B52 ECCBC87E4B5CE2FE28308FD9F2A7BAF3
#> 5 EC8956637A99787BD197EACD77ACCE5E ECCBC87E4B5CE2FE28308FD9F2A7BAF3
#> 6 38B3EFF8BAF56627478EC76A704E9B52 ECCBC87E4B5CE2FE28308FD9F2A7BAF3
#>                  time category        variable value
#> 1 2019-04-04 06:06:00   vitals NIBP Pulse Rate   124
#> 2 2019-04-08 08:49:00   vitals            SpO2    92
#> 3 2019-04-02 18:33:00   vitals        NIBP Sys   108
#> 4 2019-04-05 17:02:00   vitals            SpO2    90
#> 5 2019-04-03 16:24:00   vitals       NIBP Dias   115
#> 6 2019-04-05 01:44:00   vitals   NIBP Pulse Ox    88

# The fixed data should of type tibble, data.frame or disk frame.
fixed_data = data.table::fread(fixed_file) %>% 
  dplyr::mutate(time = admit_time %>% lubridate::as_datetime(), outcome = outcome %>% lubridate::as_datetime()) %>% 
  as.data.frame()

fixed_data %>% head()
#>                         patient_id                     encounter_id
#> 1 38B3EFF8BAF56627478EC76A704E9B52 A87FF679A2F3E71D9181A67B7542122C
#> 2 38B3EFF8BAF56627478EC76A704E9B52 C4CA4238A0B923820DCC509A6F75849B
#> 3 38B3EFF8BAF56627478EC76A704E9B52 C81E728D9D4C2F636F067F89CC14862C
#> 4 38B3EFF8BAF56627478EC76A704E9B52 ECCBC87E4B5CE2FE28308FD9F2A7BAF3
#> 5 EC8956637A99787BD197EACD77ACCE5E A87FF679A2F3E71D9181A67B7542122C
#> 6 EC8956637A99787BD197EACD77ACCE5E C4CA4238A0B923820DCC509A6F75849B
#>             admit_time             outcome                time
#> 1 2019-04-01T00:39:00Z 2019-04-09 19:57:00 2019-04-01 00:39:00
#> 2 2019-04-01T01:36:00Z 2019-04-09 22:41:00 2019-04-01 01:36:00
#> 3 2019-04-01T02:56:00Z 2019-04-09 23:34:00 2019-04-01 02:56:00
#> 4 2019-04-01T03:03:00Z 2019-04-09 21:59:00 2019-04-01 03:03:00
#> 5 2019-04-01T02:47:00Z 2019-04-09 23:17:00 2019-04-01 02:47:00
#> 6 2019-04-01T00:21:00Z 2019-04-09 23:32:00 2019-04-01 00:21:00
```

# 2\) Generating the categorical column names.

``` r
# input temporal_dataframe and the return is also a temporal_data frame.
# The function columns the categorical columns as a variable with a binary value.
temporal_data_2 = wisard::categorical_col_names_generator(temporal_data = temporal_data)

temporal_data_2
#> # A tibble: 5 x 2
#> # Groups:   variable [5]
#>   variable        var_type
#>   <chr>           <chr>   
#> 1 NIBP Dias       integer 
#> 2 NIBP Pulse Ox   integer 
#> 3 NIBP Pulse Rate integer 
#> 4 NIBP Sys        integer 
#> 5 SpO2            integer
```

# 3\) Identifying the unique variables in the temporal dataset.

``` r
# The input parameter is the temporal dataframe and the output is unique variables and their categories
unique_columns = wisard::unique_variables(data = temporal_data)
#> [1] "here"
unique_columns
#> # A tibble: 5 x 2
#>   variable        category
#>   <chr>           <chr>   
#> 1 NIBP Dias       vitals  
#> 2 NIBP Pulse Ox   vitals  
#> 3 NIBP Pulse Rate vitals  
#> 4 NIBP Sys        vitals  
#> 5 SpO2            vitals
```

# 4\) Calling in the date\_to\_time function to convert the timestamps in the temporal data to Number relative to the admit time or initial time.

``` r
# To convert the user argument which is a period object to a numeric value. It returns a list of values.
period_measure = wisard::hour_to_number(window_size = lubridate::hours(6),lookback = lubridate::hours(48),lookahead = lubridate::hours(48))
# The input parameters are:
#1)temporal_data
#2) Fixed data frame
#3) units : unit of the period object for eg.in hours(6) the unit is hours and seconds(6) unit is seconds.
temporal_data = wisard::date_to_time(temporal_data = temporal_data, fixed_data = fixed_data, units = period_measure$units)
#> [1] "just in"
#> [1] "did i"
#> [1] "ran"
```

# 4\) Generating the column names based on user specified summary stats.

``` r
# The input parameters are:
#1) temporal dataframe.
#2) feature stat based on category
column_names = wisard::colnames_generator(temporal_data = temporal_data, feature_stat = list(vitals = c("min","mean", "max"),meds = c("n")) )
# returns a list of column names
all_variable_to_create = column_names[[1]]
```

# 5\) Finally calling in the lagged feature generation function.

``` r
result_frame = wisard::dummy_wisard(temporal_data = temporal_data,all_variable_to_create = all_variable_to_create,window_size = period_measure$window_size,outcome_var = NULL,outcome_stat = list("min"),lookahead = period_measure$lookahead,lookback = period_measure$lookback)
#> [1] 1
#> [1] 2
#> [1] 3
#> [1] 4
#> [1] 5
#> [1] 6
#> [1] 7
#> [1] "done imputing the data"
#> [1] "done gathering all the data"
#> [1] "tbl_df"     "tbl"        "data.frame"
#> [1] "Completed spreading the data frame."
result_frame %>% head()
#> # A tibble: 6 x 127
#>   encounter_id  time `NIBP Pulse Rat~ `NIBP Pulse Rat~ `NIBP Pulse Rat~
#>   <chr>        <dbl>            <dbl>            <dbl>            <dbl>
#> 1 A87FF679A2F~     0               80               NA               NA
#> 2 A87FF679A2F~     6               69               80               NA
#> 3 A87FF679A2F~    12               75               69               80
#> 4 A87FF679A2F~    18               44               75               69
#> 5 A87FF679A2F~    24               64               44               75
#> 6 A87FF679A2F~    30               55               64               44
#> # ... with 122 more variables: `NIBP Pulse Rate_min_18` <dbl>, `NIBP Pulse
#> #   Rate_min_24` <dbl>, `NIBP Pulse Rate_min_30` <dbl>, `NIBP Pulse
#> #   Rate_min_36` <dbl>, `NIBP Pulse Rate_min_42` <dbl>, `NIBP Pulse
#> #   Rate_mean_0` <dbl>, `NIBP Pulse Rate_mean_6` <dbl>, `NIBP Pulse
#> #   Rate_mean_12` <dbl>, `NIBP Pulse Rate_mean_18` <dbl>, `NIBP Pulse
#> #   Rate_mean_24` <dbl>, `NIBP Pulse Rate_mean_30` <dbl>, `NIBP Pulse
#> #   Rate_mean_36` <dbl>, `NIBP Pulse Rate_mean_42` <dbl>, `NIBP Pulse
#> #   Rate_max_0` <dbl>, `NIBP Pulse Rate_max_6` <dbl>, `NIBP Pulse
#> #   Rate_max_12` <dbl>, `NIBP Pulse Rate_max_18` <dbl>, `NIBP Pulse
#> #   Rate_max_24` <dbl>, `NIBP Pulse Rate_max_30` <dbl>, `NIBP Pulse
#> #   Rate_max_36` <dbl>, `NIBP Pulse Rate_max_42` <dbl>, SpO2_min_0 <dbl>,
#> #   SpO2_min_6 <dbl>, SpO2_min_12 <dbl>, SpO2_min_18 <dbl>, SpO2_min_24 <dbl>,
#> #   SpO2_min_30 <dbl>, SpO2_min_36 <dbl>, SpO2_min_42 <dbl>, SpO2_mean_0 <dbl>,
#> #   SpO2_mean_6 <dbl>, SpO2_mean_12 <dbl>, SpO2_mean_18 <dbl>,
#> #   SpO2_mean_24 <dbl>, SpO2_mean_30 <dbl>, SpO2_mean_36 <dbl>,
#> #   SpO2_mean_42 <dbl>, SpO2_max_0 <dbl>, SpO2_max_6 <dbl>, SpO2_max_12 <dbl>,
#> #   SpO2_max_18 <dbl>, SpO2_max_24 <dbl>, SpO2_max_30 <dbl>, SpO2_max_36 <dbl>,
#> #   SpO2_max_42 <dbl>, `NIBP Sys_min_0` <dbl>, `NIBP Sys_min_6` <dbl>, `NIBP
#> #   Sys_min_12` <dbl>, `NIBP Sys_min_18` <dbl>, `NIBP Sys_min_24` <dbl>, `NIBP
#> #   Sys_min_30` <dbl>, `NIBP Sys_min_36` <dbl>, `NIBP Sys_min_42` <dbl>, `NIBP
#> #   Sys_mean_0` <dbl>, `NIBP Sys_mean_6` <dbl>, `NIBP Sys_mean_12` <dbl>, `NIBP
#> #   Sys_mean_18` <dbl>, `NIBP Sys_mean_24` <dbl>, `NIBP Sys_mean_30` <dbl>,
#> #   `NIBP Sys_mean_36` <dbl>, `NIBP Sys_mean_42` <dbl>, `NIBP Sys_max_0` <dbl>,
#> #   `NIBP Sys_max_6` <dbl>, `NIBP Sys_max_12` <dbl>, `NIBP Sys_max_18` <dbl>,
#> #   `NIBP Sys_max_24` <dbl>, `NIBP Sys_max_30` <dbl>, `NIBP Sys_max_36` <dbl>,
#> #   `NIBP Sys_max_42` <dbl>, `NIBP Dias_min_0` <dbl>, `NIBP Dias_min_6` <dbl>,
#> #   `NIBP Dias_min_12` <dbl>, `NIBP Dias_min_18` <dbl>, `NIBP
#> #   Dias_min_24` <dbl>, `NIBP Dias_min_30` <dbl>, `NIBP Dias_min_36` <dbl>,
#> #   `NIBP Dias_min_42` <dbl>, `NIBP Dias_mean_0` <dbl>, `NIBP
#> #   Dias_mean_6` <dbl>, `NIBP Dias_mean_12` <dbl>, `NIBP Dias_mean_18` <dbl>,
#> #   `NIBP Dias_mean_24` <dbl>, `NIBP Dias_mean_30` <dbl>, `NIBP
#> #   Dias_mean_36` <dbl>, `NIBP Dias_mean_42` <dbl>, `NIBP Dias_max_0` <dbl>,
#> #   `NIBP Dias_max_6` <dbl>, `NIBP Dias_max_12` <dbl>, `NIBP
#> #   Dias_max_18` <dbl>, `NIBP Dias_max_24` <dbl>, `NIBP Dias_max_30` <dbl>,
#> #   `NIBP Dias_max_36` <dbl>, `NIBP Dias_max_42` <dbl>, `NIBP Pulse
#> #   Ox_min_0` <dbl>, `NIBP Pulse Ox_min_6` <dbl>, `NIBP Pulse Ox_min_12` <dbl>,
#> #   `NIBP Pulse Ox_min_18` <dbl>, `NIBP Pulse Ox_min_24` <dbl>, `NIBP Pulse
#> #   Ox_min_30` <dbl>, `NIBP Pulse Ox_min_36` <dbl>, ...
```
