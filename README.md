# samplingin
Repository for testing samplingin package

## Overview
samplingin is a robust solution employing both systematic and PPS (Probability Proportional to Size) sampling methods, ensuring a methodical and representative selection of data. 
Seamlessly allocate predetermined allocations to smaller levels. Kish, L. (1965) <https://books.google.co.id/books?id=xiZmAAAAIAAJ>.

- `get_allocation()` allocate predetermined allocations to smaller levels using proportional allocation method
- `doSampling()` samples selection using systematic or PPS (Probability Proportional to Size) sampling method based on certain allocation.

## Installation

``` r
install.packages("samplingin")
```

## Usage

``` r
library(samplingin)
library(magrittr)

contoh_alokasi = alokasi_dt %>%
    dplyr::select(-n_primary) %>%
    dplyr::mutate(nasional = 1)

alokasi_dt = get_allocation(
    data = contoh_alokasi
    , alokasi = 100
    , group = c("nasional")
    , pop_var = "jml_kabkota"
 )

library(samplingin)

# PPS Sampling 
dtSampling_pps = doSampling(
    pop = pop_dt
    , alloc = alokasi_dt
    , nsampel = "n_primary"
    , type = "U"
    , ident = c("kdprov")
    , method = "pps"
    , auxVar = "Total"
    , seed = 1234
)

# Population data with flag sample
pop_dt = dtSampling_pps$pop

# Selected Samples
dsampel = dtSampling_pps$dsampel

# Details of sampling process
rincian = dtSampling_pps$rincian

# Systemtic Sampling 
dtSampling_sys = doSampling(
    pop = pop_dt
    , alloc = alokasi_dt
    , nsampel = "n_primary"
    , type = "U"
    , ident = c("kdprov")
    , method = "systematic"
    , seed = 4321
)

# Population data with flag sample
pop_dt = dtSampling_sys$pop

# Selected Samples
dsampel = dtSampling_sys$dsampel

# Details of sampling process
rincian = dtSampling_sys$rincian
```