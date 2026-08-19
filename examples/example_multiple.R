# ============================================================
# OTSI 1.0 - Example: Multiple land units
# ============================================================

# Run this example from the root directory of the OTSI repository.

# Load OTSI
source("otsi_1_0_core.R")
source("otsi_1_0_single.R")
source("otsi_1_0_multiple.R")

# ============================================================
# Example 1: One observation per land unit
# ============================================================

soil_data <- data.frame(
  Parcel_ID = c(
    "Field_A",
    "Field_B",
    "Field_C"
  ),

  Landuse = c(
    "Agroforestry",
    "Forest plantation",
    "Dryland farming"
  ),

  SOC_percent = c(
    1.8,
    2.5,
    1.2
  ),

  pH_H2O = c(
    6.4,
    6.8,
    5.3
  ),

  Root_depth_cm = c(
    75,
    110,
    40
  ),

  Drainage_FAO = c(
    "MW",
    "W",
    "P"
  )
)

# Calculate OTSI
multiple_result <- otsi_multiple(
  data = soil_data,

  unit_id = "Parcel_ID",

  group_by = "Landuse",

  indicator_map = c(
    soc = "SOC_percent",
    ph = "pH_H2O",
    root_depth = "Root_depth_cm",
    drainage = "Drainage_FAO"
  ),

  aggregate_within_unit = "none"
)

# View the main results
multiple_result$scores

# Indicator-level results
multiple_result$indicator_table

# Indicator availability
multiple_result$indicator_coverage

# Summary by land-use group
multiple_result$group_summary


# ============================================================
# Example 2: Repeated observations within each land unit
# ============================================================

repeated_soil_data <- data.frame(
  Parcel_ID = c(
    "Field_A",
    "Field_A",
    "Field_B",
    "Field_B"
  ),

  Sample_ID = c(
    "A1",
    "A2",
    "B1",
    "B2"
  ),

  Landuse = c(
    "Agroforestry",
    "Agroforestry",
    "Forest plantation",
    "Forest plantation"
  ),

  SOC_percent = c(
    1.0,
    3.0,
    2.0,
    4.0
  ),

  pH_H2O = c(
    6.2,
    6.6,
    6.5,
    6.7
  ),

  Root_depth_cm = c(
    60,
    80,
    100,
    120
  ),

  Drainage_FAO = c(
    "MW",
    "MW",
    "W",
    "W"
  )
)

# Use the median of repeated numeric observations
# within each land unit before OTSI scoring.

repeated_result <- otsi_multiple(
  data = repeated_soil_data,

  unit_id = "Parcel_ID",

  group_by = "Landuse",

  indicator_map = c(
    soc = "SOC_percent",
    ph = "pH_H2O",
    root_depth = "Root_depth_cm",
    drainage = "Drainage_FAO"
  ),

  aggregate_within_unit = "median"
)

# View results
repeated_result$scores

# Detailed indicator-level results
repeated_result$indicator_table
