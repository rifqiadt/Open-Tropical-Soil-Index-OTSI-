# OTSI 1.0 User Guide

This guide provides a concise overview of how to run the current OTSI 1.0 implementation for a single land unit or multiple land units and how to interpret the resulting scores.

---

## 1. Loading OTSI

OTSI 1.0 currently consists of three R scripts:

```text
otsi_1_0_core.R
otsi_1_0_single.R
otsi_1_0_multiple.R
```

The scoring functions use the `OBIC` R package. Install it first if necessary:

```r
install.packages("OBIC")
```

Load the OTSI scripts in the following order:

```r
source("otsi_1_0_core.R")
source("otsi_1_0_single.R")
source("otsi_1_0_multiple.R")
```

The core script contains the scoring and aggregation engine. The single and multiple scripts provide the user-facing functions.

---

## 2. Calculating OTSI for a single land unit

Use `otsi_single()` when one set of soil measurements represents one land unit.

A land unit is defined by the user and may represent, for example, a field, parcel, soil profile, restoration plot, sampling unit, or soil mapping unit.

Example:

```r
field_a <- otsi_single(
  unit_id = "Field_A",
  soc = 1.8,
  ph = 6.4,
  root_depth = 75,
  drainage = "MW"
)

print(field_a)
```

Example output:

```text
Open Tropical Soil Index 1.0
Land unit: Field_A
Assessment type: Partial OTSI
Indicators used: 4 of 9
Chemical indicators: 2
Physical indicators: 2
Chemical score: 0.317
Physical score: 0.820
Final OTSI: 0.568
Primary limiting indicator(s): SOC
```

More detailed results can be viewed with:

```r
field_a$indicator_table
field_a$summary
```

Not all nine OTSI indicators need to be supplied. However, a final OTSI score is only calculated when at least one chemical and one physical indicator are available.

---

## 3. Calculating OTSI for multiple land units

Use `otsi_multiple()` when several land units are stored in one table.

Example input:

```r
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
```

Run OTSI by identifying the land-unit column and mapping the dataset columns to the corresponding OTSI indicators:

```r
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
```

The main unit-level results are available through:

```r
multiple_result$scores
```

Additional outputs include:

```r
multiple_result$summary
multiple_result$indicator_table
multiple_result$group_summary
multiple_result$indicator_coverage
multiple_result$qc
```

`unit_id` defines the unit for which one OTSI value is calculated.

`group_by` is optional and can be used to summarize results by a broader category such as land use. It does not change the definition of the land unit.

---

## 4. Repeated observations within a land unit

A land unit may contain several soil observations.

For example:

| Parcel  | Sample | SOC (%) |  pH |
| ------- | ------ | ------: | --: |
| Field_A | A1     |     1.2 | 6.2 |
| Field_A | A2     |     1.8 | 6.4 |
| Field_A | A3     |     2.4 | 6.6 |

OTSI can combine these repeated observations before scoring.

The available options are:

```r
aggregate_within_unit = "none"
aggregate_within_unit = "mean"
aggregate_within_unit = "median"
```

Use:

* `"none"` when every row already represents one unique land unit
* `"mean"` to use the mean of repeated numeric measurements
* `"median"` to use the median of repeated numeric measurements

For categorical drainage, the most frequent drainage class is used.

Aggregation occurs **within the same indicator** before OTSI scoring. For example, repeated SOC measurements are combined with other SOC measurements, and repeated pH measurements are combined with other pH measurements.

Different soil indicators are not averaged together.

---

## 5. Primary limiting indicator

For each land unit, OTSI identifies the **primary limiting indicator** as the individual indicator with the lowest standardized score.

For example:

```text
SOC score          = 0.18
pH score           = 0.85
Root depth score   = 0.75
Drainage score     = 0.90
```

In this case, SOC is the primary limiting indicator.

For a single land unit, the limiting indicator is shown directly in the printed output.

For multiple land units, it can be viewed in:

```r
multiple_result$scores
```

Relevant columns include:

```text
primary_limiter
primary_limiter_label
primary_limiter_score
n_tied_primary_limiters
```

When several indicators share the same minimum score, OTSI records the tied limiting indicators rather than selecting one arbitrarily.

---

## 6. Interpreting the final OTSI

The resulting OTSI ranges from **0 to 1**, with higher values indicating more favourable soil conditions across the functions represented by the included indicators and lower values indicating increasing constraints.

The final score therefore represents an **integrated soil health index of the functional conditions captured by the available indicators**.

These soil functions contribute to associated ecosystem services, but OTSI does not directly quantify ecosystem service delivery.

The final OTSI should therefore be interpreted together with:

* the indicators included in the assessment
* the individual indicator scores
* the chemical and physical domain scores
* the primary limiting indicator

Two land units may have similar final OTSI values but different underlying soil constraints.

OTSI results calculated from substantially different indicator combinations should also be compared carefully because the final score reflects the functional conditions represented by the indicators included in each assessment.
    
