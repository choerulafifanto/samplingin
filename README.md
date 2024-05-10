# samplingin <a href="https://cran.r-project.org/package=samplingin"><img src="man/figures/samplingin.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/samplingin)](https://cran.r-project.org/package=samplingin)
[![Total
download](https://cranlogs.r-pkg.org/badges/grand-total/samplingin)](https://cran.r-project.org/package=samplingin)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)
<!-- badges: end -->

## Overview
samplingin is a robust solution employing SRS (Simple Random Sampling), systematic and PPS (Probability Proportional to Size) sampling methods, ensuring a methodical and representative selection of data. 
Seamlessly allocate predetermined allocations to smaller levels. 

- `get_allocation()` allocate predetermined allocations to smaller levels using proportional allocation method
- `doSampling()` samples selection using srs, systematic or PPS (Probability Proportional to Size) sampling method based on certain allocation.

## Installation

``` r
install.packages("samplingin")
```

## Usage

``` r
library(samplingin)
library(magrittr)
library(dplyr)

contoh_alokasi = alokasi_dt %>%
    select(-n_primary) %>%
    mutate(nasional = 1)

alokasi_dt = get_allocation(
    data = contoh_alokasi
    , alokasi = 100
    , group = c("nasional")
    , pop_var = "jml_kabkota"
 )
 
# Simple Random Sampling (SRS)
dtSampling_srs = doSampling(
  pop         = pop_dt
  , alloc     = alokasi_dt
  , nsample   = "n_primary"
  , type      = "U"
  , ident     = c("kdprov")
  , method    = "srs"
  , auxVar    = "Total"
  , seed      = 7892
)

# Population data with flag sample
pop_dt = dtSampling_srs$pop

# Selected Samples
dsampel = dtSampling_srs$sampledf

# Details of sampling process
rincian = dtSampling_srs$details

# PPS Sampling 
dtSampling_pps = doSampling(
    pop       = pop_dt
    , alloc   = alokasi_dt
    , nsample = "n_primary"
    , type    = "U"
    , ident   = c("kdprov")
    , method  = "pps"
    , auxVar  = "Total"
    , seed    = 1234
)

# Population data with flag sample
pop_dt = dtSampling_pps$pop

# Selected Samples
sampledf = dtSampling_pps$sampledf

# Details of sampling process
details = dtSampling_pps$details

# Systemtic Sampling 
dtSampling_sys = doSampling(
    pop       = pop_dt
    , alloc   = alokasi_dt
    , nsample = "n_primary"
    , type    = "U"
    , ident   = c("kdprov")
    , method  = "systematic"
    , seed    = 4321
)

# Population data with flag sample
pop_dt = dtSampling_sys$pop

# Selected Samples
sampledf = dtSampling_sys$sampledf

# Details of sampling process
details = dtSampling_sys$details

# Systematic Sampling (Secondary Samples)

alokasi_dt_p = alokasi_dt %>%
  mutate(n_secondary = 2 * n_primary)

dtSampling_sys_p = doSampling(
  pop           = dtSampling_sys$pop
  , alloc       = alokasi_dt_p
  , nsample     = "n_secondary"
  , type        = "P"
  , ident       = c("kdprov")
  , method      = "systematic"
  , seed        = 6789
  , is_secondary = TRUE
)

# Population data with flag sample
pop_dt = dtSampling_sys_p$pop

# Selected Samples
dsampel = dtSampling_sys_p$sampledf

# Details of sampling process
rincian = dtSampling_sys_p$details

# Systematic Sampling with predetermined random number (predetermined_rn parameter)
alokasi_dt_rn = alokasi_dt %>% rowwise() %>% mutate(ar = runif(n(),0,1)) %>% ungroup

dtSampling_sys = doSampling(
    pop = pop_dt
    , alloc    = alokasi_dt_rn
    , nsample  = "n_primary"
    , type     = "U"
    , ident    = c("kdprov")
    , method   = "systematic"
    , predetermined_rn = "ar"
    , seed     = 4321
)

# Population data with flag sample
pop_dt = dtSampling_sys$pop

# Selected Samples
sampledf = dtSampling_sys$sampledf

# Details of sampling process
details = dtSampling_sys$details
```
