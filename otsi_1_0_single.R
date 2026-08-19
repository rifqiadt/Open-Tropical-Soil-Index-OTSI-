# =========================================================
# OPEN TROPICAL SOIL INDEX (OTSI) 1.0
# USER-FACING FUNCTION FOR ONE LAND UNIT
# =========================================================
#
# This module provides otsi_single(), a user-facing wrapper
# around otsi_calculate_core().
#
# Requirements:
# - Source otsi_1_0_core.R first.
# - Supply one value per available indicator.
# - Any subset of the nine registered indicators may be used.
# - Final OTSI is calculated only when both the chemical and
#   physical domains are represented by at least one valid
#   indicator.
# =========================================================


# =========================================================
# 1. INTERNAL CHECKS
# =========================================================

.otsi_single_require_core <- function() {
  required_objects <- c(
    "otsi_calculate_core",
    "otsi_registry_table",
    "OTSI_INDICATOR_REGISTRY",
    "OTSI_METHOD_VERSION"
  )

  missing_objects <- required_objects[
    !vapply(required_objects, exists, logical(1), inherits = TRUE)
  ]

  if (length(missing_objects) > 0) {
    stop(
      "The OTSI core engine is not loaded. Source ",
      "'otsi_1_0_core.R' before using otsi_single().",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.otsi_single_value_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA)
  }

  x[[1]]
}


# =========================================================
# 2. USER-FACING SINGLE-LAND-UNIT FUNCTION
# =========================================================

otsi_single <- function(
    unit_id = "unit_1",
    soc = NULL,
    total_n = NULL,
    effective_cec = NULL,
    ph = NULL,
    esp = NULL,
    bulk_density = NULL,
    root_depth = NULL,
    drainage = NULL,
    awc = NULL
) {
  .otsi_single_require_core()

  if (length(unit_id) != 1L || is.na(unit_id)) {
    stop("'unit_id' must contain one non-missing value.", call. = FALSE)
  }

  unit_id <- trimws(as.character(unit_id))

  if (unit_id == "") {
    stop("'unit_id' cannot be an empty string.", call. = FALSE)
  }

  supplied_values <- list(
    soc = soc,
    total_n = total_n,
    effective_cec = effective_cec,
    ph = ph,
    esp = esp,
    bulk_density = bulk_density,
    root_depth = root_depth,
    drainage = drainage,
    awc = awc
  )

  supplied_values <- supplied_values[
    !vapply(supplied_values, is.null, logical(1))
  ]

  if (length(supplied_values) == 0) {
    stop(
      "No OTSI indicators were supplied. Provide at least one ",
      "registered indicator.",
      call. = FALSE
    )
  }

  invalid_lengths <- names(supplied_values)[
    vapply(supplied_values, length, integer(1)) != 1L
  ]

  if (length(invalid_lengths) > 0) {
    stop(
      "otsi_single() accepts exactly one value per indicator. ",
      "Invalid inputs: ",
      paste(invalid_lengths, collapse = ", "),
      ". Use otsi_multiple() for tabular data.",
      call. = FALSE
    )
  }

  core_result <- otsi_calculate_core(supplied_values)
  registry <- otsi_registry_table()

  supplied_indicator_names <- names(supplied_values)
  supplied_registry <- registry[
    match(supplied_indicator_names, registry$indicator),
    ,
    drop = FALSE
  ]

  # Keep a readable display column because the supplied
  # indicators may mix numeric and categorical values.
  input_display <- vapply(
    supplied_values,
    function(x) {
      if (length(x) == 0 || is.na(x[[1]])) {
        return(NA_character_)
      }
      as.character(x[[1]])
    },
    character(1)
  )

  score_values <- core_result$indicator_scores[
    supplied_registry$score_column
  ]

  standardized_value <- rep(NA_character_, nrow(supplied_registry))

  root_row <- supplied_registry$indicator == "root_depth"
  if (any(root_row)) {
    root_class <- core_result$standardized_input$root_depth_class
    standardized_value[root_row] <- if (
      is.na(root_class)
    ) {
      NA_character_
    } else {
      paste0("FAO class ", root_class)
    }
  }

  drainage_row <- supplied_registry$indicator == "drainage"
  if (any(drainage_row)) {
    standardized_value[drainage_row] <-
      core_result$standardized_input$drainage_fao
  }

  indicator_table <- data.frame(
    unit_id = unit_id,
    indicator = supplied_registry$indicator,
    label = supplied_registry$label,
    domain = supplied_registry$domain,
    expected_unit = supplied_registry$unit,
    input_value = unname(input_display),
    standardized_value = standardized_value,
    score = as.numeric(score_values),
    valid_for_calculation = !is.na(as.numeric(score_values)),
    stringsAsFactors = FALSE
  )

  limiter_score_columns <- core_result$primary_limiter$indicator

  if (
    length(limiter_score_columns) == 0 ||
    all(is.na(limiter_score_columns))
  ) {
    limiter_indicators <- NA_character_
    limiter_labels <- NA_character_
  } else {
    limiter_match <- match(
      limiter_score_columns,
      registry$score_column
    )
    limiter_indicators <- registry$indicator[limiter_match]
    limiter_labels <- registry$label[limiter_match]
  }

  summary_table <- data.frame(
    unit_id = unit_id,
    method = core_result$method$name,
    method_version = core_result$method$version,
    assessment_type = core_result$assessment_type,
    chemical_n_indicators = unname(
      core_result$indicator_counts[["chemical"]]
    ),
    physical_n_indicators = unname(
      core_result$indicator_counts[["physical"]]
    ),
    total_n_indicators = unname(
      core_result$indicator_counts[["total"]]
    ),
    chemical_score = unname(
      core_result$domain_scores[["chemical_score"]]
    ),
    physical_score = unname(
      core_result$domain_scores[["physical_score"]]
    ),
    final_otsi = unname(core_result$final_otsi),
    primary_limiter = if (
      all(is.na(limiter_indicators))
    ) {
      NA_character_
    } else {
      paste(limiter_indicators, collapse = "; ")
    },
    primary_limiter_label = if (
      all(is.na(limiter_labels))
    ) {
      NA_character_
    } else {
      paste(limiter_labels, collapse = "; ")
    },
    primary_limiter_score = unname(
      core_result$primary_limiter$score
    ),
    n_tied_primary_limiters = unname(
      core_result$primary_limiter$n_tied
    ),
    stringsAsFactors = FALSE
  )

  output <- core_result
  output$unit_id <- unit_id
  output$indicator_table <- indicator_table
  output$summary <- summary_table
  output$primary_limiter$indicator <- limiter_indicators
  output$primary_limiter$label <- limiter_labels
  output$primary_limiter$score_column <- limiter_score_columns

  class(output) <- c(
    "otsi_single_result",
    "otsi_result",
    "list"
  )

  output
}


# =========================================================
# 3. PRINT METHOD
# =========================================================

print.otsi_single_result <- function(x, ...) {
  cat(
    "Open Tropical Soil Index ",
    x$method$version,
    "\n",
    sep = ""
  )
  cat("Land unit: ", x$unit_id, "\n", sep = "")
  cat("Assessment type: ", x$assessment_type, "\n", sep = "")
  cat(
    "Indicators used: ",
    x$indicator_counts[["total"]],
    " of ",
    length(OTSI_INDICATOR_REGISTRY),
    "\n",
    sep = ""
  )
  cat(
    "Chemical indicators: ",
    x$indicator_counts[["chemical"]],
    "\n",
    sep = ""
  )
  cat(
    "Physical indicators: ",
    x$indicator_counts[["physical"]],
    "\n",
    sep = ""
  )
  cat(
    "Chemical score: ",
    ifelse(
      is.na(x$domain_scores[["chemical_score"]]),
      "NA",
      sprintf("%.3f", x$domain_scores[["chemical_score"]])
    ),
    "\n",
    sep = ""
  )
  cat(
    "Physical score: ",
    ifelse(
      is.na(x$domain_scores[["physical_score"]]),
      "NA",
      sprintf("%.3f", x$domain_scores[["physical_score"]])
    ),
    "\n",
    sep = ""
  )
  cat(
    "Final OTSI: ",
    ifelse(
      is.na(x$final_otsi),
      "NA (both domains are required)",
      sprintf("%.3f", x$final_otsi)
    ),
    "\n",
    sep = ""
  )

  limiter_text <- if (
    length(x$primary_limiter$label) == 0 ||
    all(is.na(x$primary_limiter$label))
  ) {
    "NA"
  } else {
    paste(x$primary_limiter$label, collapse = "; ")
  }

  cat(
    "Primary limiting indicator(s): ",
    limiter_text,
    "\n",
    sep = ""
  )

  invisible(x)
}


# =========================================================
# 4. CONVERSION TO A ONE-ROW DATA FRAME
# =========================================================

as.data.frame.otsi_single_result <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...
) {
  x$summary
}






