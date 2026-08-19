# ============================================================
# OTSI 1.0 - Example: Single land unit
# ============================================================

# Run this example from the root directory of the OTSI repository.

# Load OTSI
source("otsi_1_0_core.R")
source("otsi_1_0_single.R")

# ------------------------------------------------------------
# Example soil data
# ------------------------------------------------------------

# This example uses four available indicators:
# - SOC (%)
# - pH H2O
# - Root depth (cm)
# - FAO drainage class

field_a <- otsi_single(
  unit_id = "Field_A",
  soc = 1.8,
  ph = 6.4,
  root_depth = 75,
  drainage = "MW"
)

# ------------------------------------------------------------
# View results
# ------------------------------------------------------------

print(field_a)

# Detailed indicator-level results
field_a$indicator_table

# Summary of the OTSI calculation
field_a$summary
