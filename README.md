# Open-Tropical-Soil-Index-OTSI-

The Open Tropical Soil Index (OTSI) is an R-based soil assessment framework developed to evaluate chemical and physical soil conditions as part of soil health assessment in tropical environments.

OTSI converts available soil properties into standardized indicator scores ranging from 0 to 1, aggregates them into chemical and physical domain scores, and calculates a final index showing the soil health score when both domains are represented. 

The current implementation supports calculation for:

a single land unit using otsi_single()
multiple land units using otsi_multiple()

A land unit is defined by the user and may represent, for example, a field, parcel, soil profile, land-use type, restoration plot, or soil mapping unit.

# Current development status

OTSI version 1.0 is currently a development prototype.

The repository contains three main R scripts:

otsi_1_0_core.R
otsi_1_0_single.R
otsi_1_0_multiple.R

The core calculation engine has been tested against the existing FAO HWSD v2.0 implementation and reproduced the same indicator scores, chemical and physical scores, and final OTSI score.

The current scripts are intended to provide a transparent and reproducible basis for further development into an R package. 

