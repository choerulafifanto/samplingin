# samplingin 1.1.0

Changes in version 1.1.0

Added

* Adding parameter `is_secondary` in `doSampling` function. It maintains existing selected samples in `pop` data.frame

* PPS using `sampling::inclusionprobabilities()`

Changed

* Return value variable on `doSampling`, `dsampel` to `sampledf` and `rincian` to `details`, `pop` remains the same

* Change `nsampel` to `nsample` parameter in `doSampling` function

* `sisa` changed to `n_deficit` on details data.frame

* `jml` changed to `n_selected` on details data.frame

* Change in deprecated `.dots = var` to `across(all_of(var))` 

# samplingin 1.0.7

Changes in version 1.0.7

Bug fixes

* Resolving the issue of creating interval in PPS Sampling.

* Resolving the issue of getting allocation variable

# samplingin 1.0.6

Changes in version 1.0.6

Bug fixes

* Resolving the issue of having the same random number in each row.

* Adding unselected sample allocation messages.

# samplingin 1.0.5

Changes in version 1.0.5

Bug fixes

* Resolving the error in which population is smaller than allocated size in PPS sampling.

* Incorporating messages for both negative and zero allocations and implementing an error message specifically for negative allocations.

* Optimizing processing time by excluding zero allocations from the allocation data frame.

* Adding predetermined random number parameter on doSampling function

# samplingin 1.0.4

Changes in version 1.0.4

Bug fixes

* Commented if(verbose) cat(lis[["ar"]],"\n")

# samplingin 1.0.3

Changes in version 1.0.3

Bug fixes

* Changed `Imports` on DESCRIPTION

# samplingin 1.0.2

Changes in version 1.0.2

Bug fixes

* Improved implicit stratification sorting

* Changed Description in `flags` parameter

# samplingin 1.0.1

Changes in version 1.0.1

Bug fixes

* Removed "Package for" in title

* Added full sentences in description text

* Added references describing the methods

* Unwrapped examples and replaced \dontrun with \donttest because elapsed time > 5 secs

* Added `verbose` parameter in doSampling

# samplingin 1.0.0

* This is a new release.
