# ============================================================
# OTSI 1.0 - SELF-TESTS
# ============================================================
#
# Purpose:
# Internal checks for the OTSI 1.0 core, single-unit,
# and multiple-unit calculation functions.
#
# Run this file from the root directory of the repository:
#
# source("tests/self_test.R")
# run_all_otsi_self_tests()
#
# ============================================================


# ============================================================
# 1. LOAD OTSI
# ============================================================

source("otsi_1_0_core.R")
source("otsi_1_0_single.R")
source("otsi_1_0_multiple.R")


# ============================================================
# 2. CORE SELF-TESTS
# ============================================================

otsi_self_test <- function() {
  .otsi_require_obic()

  tolerance <- 1e-8

  # SOC scoring boundaries
  stopifnot(
    isTRUE(
      all.equal(
        score_otsi_soc(c(1.0, 4.4)),
        c(0, 1),
        tolerance = tolerance
      )
    )
  )

  # Total N scoring boundaries and upper plateau
  stopifnot(
    isTRUE(
      all.equal(
        score_otsi_total_n(c(1.0, 3.1, 5.0)),
        c(0, 1, 1),
        tolerance = tolerance
      )
    )
  )

  # Root-depth classification
  stopifnot(
    identical(
      classify_root_depth_fao(
        c(101, 100, 50, 49.9, 10, 9.9)
      ),
      c(1L, 2L, 2L, 3L, 3L, 4L)
    )
  )

  # Drainage scoring
  stopifnot(
    isTRUE(
      all.equal(
        score_otsi_drainage(
          c("WD", "MW", "SE", "I", "P", "VP", "ED")
        ),
        c(1.0, 0.9, 0.7, 0.6, 0.4, 0.0, 0.0),
        tolerance = tolerance
      )
    )
  )

  # Chemical domain only:
  # final OTSI should not be calculated
  chemical_only <- otsi_calculate_core(
    list(
      soc = 2.0,
      ph = 6.5
    )
  )

  stopifnot(
    is.na(chemical_only$final_otsi)
  )

  # Partial OTSI with both domains represented
  partial_result <- otsi_calculate_core(
    list(
      soc = 2.0,
      ph = 6.5,
      root_depth = 75,
      drainage = "MW"
    )
  )

  stopifnot(
    !is.na(partial_result$final_otsi),
    identical(
      partial_result$assessment_type,
      "Partial OTSI"
    )
  )

  message("All OTSI 1.0 core self-tests passed.")
  invisible(TRUE)
}


# ============================================================
# 3. SINGLE-UNIT SELF-TESTS
# ============================================================

otsi_single_self_test <- function() {
  .otsi_single_require_core()

  # Test a land unit containing indicators from both domains
  result <- otsi_single(
    unit_id = "test_unit",
    soc = 1.8,
    ph = 6.4,
    root_depth = 75,
    drainage = "MW"
  )

  stopifnot(
    inherits(result, "otsi_single_result"),
    identical(result$unit_id, "test_unit"),
    identical(result$assessment_type, "Partial OTSI"),
    result$indicator_counts[["chemical"]] == 2,
    result$indicator_counts[["physical"]] == 2,
    result$indicator_counts[["total"]] == 4,
    !is.na(result$final_otsi),
    result$standardized_input$root_depth_class == 2L,
    result$standardized_input$drainage_fao == "MW",
    nrow(result$indicator_table) == 4L,
    nrow(result$summary) == 1L
  )

  # Test a land unit containing only chemical indicators
  chemical_only <- otsi_single(
    unit_id = "chemical_only",
    soc = 2.0,
    ph = 6.5
  )

  stopifnot(
    identical(
      chemical_only$assessment_type,
      "Chemical-domain result only"
    ),
    is.na(chemical_only$final_otsi)
  )

  message("All otsi_single() self-tests passed.")
  invisible(TRUE)
}


# ============================================================
# 4. MULTIPLE-UNIT SELF-TESTS
# ============================================================

otsi_multiple_self_test <- function() {
  .otsi_multiple_require_core()

  # Map example dataset columns to canonical OTSI names
  test_indicator_map <- stats::setNames(
    c(
      "SOC_percent",
      "pH_H2O",
      "Root_cm",
      "Drainage_FAO"
    ),
    c(
      "soc",
      "ph",
      "root_depth",
      "drainage"
    )
  )

  # ----------------------------------------------------------
  # Test one observation per land unit
  # ----------------------------------------------------------

  test_data <- data.frame(
    parcel_id = c("A", "B", "C"),
    land_use = c(
      "Agroforestry",
      "Forest",
      "Cropland"
    ),
    SOC_percent = c(1.8, 2.5, NA),
    pH_H2O = c(6.4, 6.8, NA),
    Root_cm = c(75, 110, 60),
    Drainage_FAO = c("MW", "W", "P"),
    stringsAsFactors = FALSE
  )

  result <- otsi_multiple(
    data = test_data,
    unit_id = "parcel_id",
    group_by = "land_use",
    indicator_map = test_indicator_map
  )

  # Calculate unit A independently using the core engine
  expected_a <- otsi_calculate_core(
    list(
      soc = 1.8,
      ph = 6.4,
      root_depth = 75,
      drainage = "MW"
    )
  )

  stopifnot(
    inherits(result, "otsi_multiple_result"),
    nrow(result$scores) == 3L,
    nrow(result$indicator_table) == 12L,

    identical(
      result$scores$assessment_type[[1]],
      "Partial OTSI"
    ),

    isTRUE(
      all.equal(
        result$scores$final_otsi[[1]],
        expected_a$final_otsi,
        tolerance = 1e-10
      )
    ),

    identical(
      result$scores$assessment_type[[3]],
      "Physical-domain result only"
    ),

    is.na(
      result$scores$final_otsi[[3]]
    ),

    nrow(result$group_summary) == 3L
  )

  # ----------------------------------------------------------
  # Test repeated observations within land units
  # ----------------------------------------------------------

  repeated_data <- data.frame(
    parcel_id = c("A", "A", "B", "B"),
    land_use = c(
      "Forest",
      "Forest",
      "Cropland",
      "Cropland"
    ),
    SOC_percent = c(1, 3, 2, 4),
    pH_H2O = c(6.2, 6.6, 6.5, 6.7),
    Root_cm = c(60, 80, 100, 120),
    Drainage_FAO = c("MW", "MW", "W", "W"),
    stringsAsFactors = FALSE
  )

  aggregated <- otsi_multiple(
    data = repeated_data,
    unit_id = "parcel_id",
    group_by = "land_use",
    indicator_map = test_indicator_map,
    aggregate_within_unit = "median"
  )

  stopifnot(
    nrow(aggregated$scores) == 2L,

    aggregated$scores$soc[
      aggregated$scores$parcel_id == "A"
    ] == 2,

    aggregated$scores$root_depth[
      aggregated$scores$parcel_id == "A"
    ] == 70,

    length(
      aggregated$qc$repeated_units_in_input
    ) == 2L
  )

  # ----------------------------------------------------------
  # Repeated units should produce an error when
  # aggregate_within_unit = "none"
  # ----------------------------------------------------------

  duplicate_error <- tryCatch(
    {
      otsi_multiple(
        data = repeated_data,
        unit_id = "parcel_id",
        indicator_map = test_indicator_map,
        aggregate_within_unit = "none"
      )

      FALSE
    },

    error = function(e) TRUE
  )

  stopifnot(
    isTRUE(duplicate_error)
  )

  message("All otsi_multiple() self-tests passed.")
  invisible(TRUE)
}


# ============================================================
# 5. RUN ALL SELF-TESTS
# ============================================================

run_all_otsi_self_tests <- function() {

  message("Running OTSI 1.0 self-tests...")
  message("")

  otsi_self_test()
  otsi_single_self_test()
  otsi_multiple_self_test()

  message("")
  message("========================================")
  message("All OTSI 1.0 self-tests passed.")
  message("========================================")

  invisible(TRUE)
}
