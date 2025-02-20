# groveR 0.1.0

* Initial release of groveR.

# groveR 0.2.3

* Major rework of groveR has been undertaken to update most functions to now 
use `terra` and `sf` packages for the majority of spatial data work and has now 
also been tested on Sentinel imagery. 
* Other work includes:
    + `veg_dens()` parameters simplified. Now the user provides an `areaname` that 
    is pre-pended to the outputs and carried forward resulting in simplified name 
    handling. New default values align with a standard workflow.
    + `mask_product()` has been removed.
    + `general_mask()` has replaced `mask_product()`. Code has been simplified and 
    there are now fewer parameters.
    + The new function `make_mask()` which turns vectorised cloud boundaries into 
    raster masks.
    + `cloud_mask_select()` and `cloud_mask_bulk()` have been removed.
    + `cloud_mask()` now replaces the "select" and "bulk" prior versions. The 
    interface has been streamlined and has fewer parameters.
    + `veg_class()` now incorporates any cloud masking to outputs.
    + `change_extent()` renamed to `extent_change()` to better reflect its function.
    
    
