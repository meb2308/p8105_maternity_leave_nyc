Data Cleaning and Plot Drafts
================
November 12, 2020

# Settings

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

# Data Cleaning

Read in dataset and select variables for analysis

``` r
wfls_df = 
  read_csv("./data/WFLS_2014.csv") %>% 
  select("resp", "mp1", "ph1", "bc2", "pph1":"el3", "el9", "el11":"cp1", "mh1", "mh4", "cc1", "fixd2":"d7", "st2":"es4", "SAMP_WEIGHT", "POP_WEIGHT", "stratum")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   clientid = col_number(),
    ##   resp = col_number(),
    ##   qkey = col_number(),
    ##   sc4a_1 = col_logical(),
    ##   sc4a_2 = col_logical(),
    ##   sc4c = col_logical(),
    ##   sc5 = col_logical(),
    ##   el12_2 = col_logical(),
    ##   el12_3 = col_logical(),
    ##   cp10_2 = col_logical(),
    ##   cp10_3 = col_logical(),
    ##   ih4_4 = col_logical(),
    ##   ih4_5 = col_logical(),
    ##   ih5a_3 = col_logical(),
    ##   ih5a_4 = col_logical(),
    ##   bf1_2 = col_logical(),
    ##   bf1_3 = col_logical(),
    ##   bf1_4 = col_logical(),
    ##   bf1_5 = col_logical(),
    ##   bf1a_2 = col_logical()
    ##   # ... with 14 more columns
    ## )

    ## See spec(...) for full column specifications.
