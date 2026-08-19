# =========================================================
# OPEN TROPICAL SOIL INDEX (OTSI) 1.0
# USER-FACING FUNCTION FOR MULTIPLE LAND UNITS
# =========================================================
#
# This module provides otsi_multiple(), a user-facing wrapper
# for calculating OTSI across a tabular dataset.
#
# Requirements:
# - Source otsi_1_0_core.R first.
# - The user defines the land-unit identifier.
# - Dataset columns are mapped to canonical OTSI indicators.
# - Any subset of the nine registered indicators may be used.
# - Final OTSI is calculated only when both chemical and
#   physical domains are represented for a land unit.
# - Repeated observations within a land unit may optionally
#   be aggregated at the raw-property level before scoring.
# =========================================================


# =========================================================
# 1. INTERNAL CHECKS AND HELPERS
# =========================================================

.otsi_multiple_require_core <- function() {
  required_objects <- c(
    "otsi_calculate_core",
    "otsi_registry_table",
    "OTSI_INDICATOR_REGISTRY",
    "OTSI_METHOD_VERSION",
    "normalize_drainage_fao"
  )

  missing_objects <- required_objects[
    !vapply(required_objects, exists, logical(1), inherits = TRUE)
  ]

  if (length(missing_objects) > 0) {
    stop(
      "The OTSI core engine is not loaded. Source ",
      "'otsi_1_0_core.R' before using otsi_multiple().",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.otsi_normalize_indicator_map <- function(indicator_map) {
  
  if (is.null(indicator_map)) {
    stop(
      "'indicator_map' cannot be NULL.",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------
  # Handle a named list
  # -------------------------------------------------------
  
  if (
    is.list(indicator_map) &&
    !is.data.frame(indicator_map)
  ) {
    
    canonical_names <- names(indicator_map)
    
    if (is.null(canonical_names)) {
      stop(
        "'indicator_map' must have canonical OTSI indicator names.",
        call. = FALSE
      )
    }
    
    if (any(lengths(indicator_map) != 1L)) {
      stop(
        "Every element of 'indicator_map' must contain exactly ",
        "one dataset column name.",
        call. = FALSE
      )
    }
    
    source_columns <- vapply(
      indicator_map,
      function(x) {
        as.character(x[[1]])
      },
      character(1)
    )
    
    # -------------------------------------------------------
    # Handle a named atomic vector
    # -------------------------------------------------------
    
  } else if (is.atomic(indicator_map)) {
    
    canonical_names <- names(indicator_map)
    
    if (is.null(canonical_names)) {
      stop(
        "'indicator_map' must be a named character vector.",
        call. = FALSE
      )
    }
    
    source_columns <- as.character(
      unname(indicator_map)
    )
    
  } else {
    
    stop(
      "'indicator_map' must be a named character vector ",
      "or named list.",
      call. = FALSE
    )
  }
  
  canonical_names <- trimws(
    as.character(canonical_names)
  )
  
  source_columns <- trimws(
    as.character(source_columns)
  )
  
  # -------------------------------------------------------
  # Validate canonical indicator names
  # -------------------------------------------------------
  
  if (
    any(is.na(canonical_names)) ||
    any(canonical_names == "")
  ) {
    stop(
      "Canonical indicator names cannot be missing or empty.",
      call. = FALSE
    )
  }
  
  duplicated_indicators <- unique(
    canonical_names[
      duplicated(canonical_names) |
        duplicated(canonical_names, fromLast = TRUE)
    ]
  )
  
  if (length(duplicated_indicators) > 0) {
    stop(
      "Canonical indicator names in 'indicator_map' must be unique. ",
      "Duplicated name(s): ",
      paste(
        duplicated_indicators,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------
  # Validate source-column names
  # -------------------------------------------------------
  
  if (
    any(is.na(source_columns)) ||
    any(source_columns == "")
  ) {
    stop(
      "Dataset column names in 'indicator_map' cannot be ",
      "missing or empty.",
      call. = FALSE
    )
  }
  
  duplicated_columns <- unique(
    source_columns[
      duplicated(source_columns) |
        duplicated(source_columns, fromLast = TRUE)
    ]
  )
  
  if (length(duplicated_columns) > 0) {
    stop(
      "Each dataset column may be mapped to only one OTSI ",
      "indicator. Duplicated column(s): ",
      paste(
        duplicated_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------
  # Check against the OTSI registry
  # -------------------------------------------------------
  
  unknown_indicators <- setdiff(
    canonical_names,
    names(OTSI_INDICATOR_REGISTRY)
  )
  
  if (length(unknown_indicators) > 0) {
    stop(
      "Unrecognized OTSI indicators in 'indicator_map': ",
      paste(
        unknown_indicators,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  stats::setNames(
    source_columns,
    canonical_names
  )
}


.otsi_as_scalar <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (length(x) == 0) {
    return(NA)
  }

  x[[1]]
}


.otsi_safe_numeric_aggregate <- function(
    x,
    method,
    indicator,
    unit_label
) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  original_missing <- is.na(x)
  numeric_x <- suppressWarnings(as.numeric(x))
  conversion_failed <- !original_missing & is.na(numeric_x)

  if (any(conversion_failed)) {
    warning(
      "Some values for '", indicator,
      "' in land unit '", unit_label,
      "' could not be converted to numeric and were ignored during ",
      method, " aggregation.",
      call. = FALSE
    )
  }

  numeric_x <- numeric_x[!is.na(numeric_x)]

  if (length(numeric_x) == 0) {
    return(NA_real_)
  }

  switch(
    method,
    mean = mean(numeric_x),
    median = median(numeric_x),
    stop("Unsupported numeric aggregation method.", call. = FALSE)
  )
}


.otsi_drainage_mode <- function(x) {
  drainage <- normalize_drainage_fao(
    x,
    warn_unknown = TRUE
  )
  drainage <- drainage[!is.na(drainage)]

  if (length(drainage) == 0) {
    return(
      list(
        value = NA_character_,
        tied = FALSE,
        tied_values = character(0)
      )
    )
  }

  frequencies <- table(drainage)
  maximum_frequency <- max(frequencies)
  modes <- names(frequencies)[frequencies == maximum_frequency]

  if (length(modes) > 1) {
    return(
      list(
        value = NA_character_,
        tied = TRUE,
        tied_values = sort(modes)
      )
    )
  }

  list(
    value = modes[[1]],
    tied = FALSE,
    tied_values = character(0)
  )
}


.otsi_consistent_group_value <- function(
    x,
    group_column,
    unit_label
) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  non_missing <- x[!is.na(x)]

  if (is.character(non_missing)) {
    non_missing <- non_missing[trimws(non_missing) != ""]
  }

  unique_values <- unique(non_missing)

  if (length(unique_values) > 1) {
    stop(
      "Grouping column '", group_column,
      "' has more than one value within land unit '", unit_label,
      "'. Grouping information must be consistent within each unit.",
      call. = FALSE
    )
  }

  if (length(unique_values) == 0) {
    return(NA)
  }

  unique_values[[1]]
}


.otsi_safe_stat <- function(x, statistic) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  switch(
    statistic,
    mean = mean(x),
    median = median(x),
    min = min(x),
    max = max(x),
    sd = if (length(x) > 1) stats::sd(x) else NA_real_,
    stop("Unsupported summary statistic.", call. = FALSE)
  )
}


.otsi_make_group_key <- function(data, columns) {
  values <- lapply(
    data[columns],
    function(x) {
      x <- as.character(x)
      x[is.na(x)] <- "<NA>"
      x
    }
  )

  do.call(
    paste,
    c(values, sep = "\r")
  )
}


# =========================================================
# 2. PREPARE ONE ROW PER LAND UNIT
# =========================================================

.otsi_prepare_multiple_units <- function(
    data,
    unit_id,
    indicator_map,
    group_by,
    aggregate_within_unit
) {
  unit_values_character <- trimws(as.character(data[[unit_id]]))

  invalid_unit_id <- is.na(data[[unit_id]]) | unit_values_character == ""

  if (any(invalid_unit_id)) {
    stop(
      "The land-unit identifier column '", unit_id,
      "' contains missing or empty values.",
      call. = FALSE
    )
  }

  unit_keys <- unit_values_character
  unit_order <- unique(unit_keys)
  repeated_table <- table(unit_keys)
  repeated_units <- names(repeated_table)[repeated_table > 1]

  canonical_indicators <- names(indicator_map)
  source_columns <- unname(indicator_map)

  if (aggregate_within_unit == "none") {
    if (length(repeated_units) > 0) {
      stop(
        "Duplicated land-unit identifiers were found: ",
        paste(repeated_units, collapse = ", "),
        ". Use aggregate_within_unit = 'mean' or 'median', or provide ",
        "a unit identifier that uniquely identifies each row.",
        call. = FALSE
      )
    }

    prepared <- data.frame(
      data[[unit_id]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(prepared)[1] <- unit_id

    for (column in group_by) {
      prepared[[column]] <- data[[column]]
    }

    for (indicator in canonical_indicators) {
      value <- data[[indicator_map[[indicator]]]]
      if (is.factor(value)) {
        value <- as.character(value)
      }
      prepared[[indicator]] <- value
    }

    return(
      list(
        data = prepared,
        repeated_units = character(0),
        drainage_tie_units = character(0),
        n_input_rows = nrow(data)
      )
    )
  }

  rows <- vector("list", length(unit_order))
  drainage_tie_units <- character(0)

  for (i in seq_along(unit_order)) {
    current_key <- unit_order[[i]]
    index <- which(unit_keys == current_key)
    current_unit_value <- data[[unit_id]][index][[1]]

    row <- list()
    row[[unit_id]] <- current_unit_value

    for (column in group_by) {
      row[[column]] <- .otsi_consistent_group_value(
        data[[column]][index],
        group_column = column,
        unit_label = current_key
      )
    }

    for (indicator in canonical_indicators) {
      source_column <- indicator_map[[indicator]]
      current_values <- data[[source_column]][index]
      input_type <- OTSI_INDICATOR_REGISTRY[[indicator]]$input_type

      if (input_type == "numeric") {
        row[[indicator]] <- .otsi_safe_numeric_aggregate(
          current_values,
          method = aggregate_within_unit,
          indicator = indicator,
          unit_label = current_key
        )
      } else if (indicator == "drainage") {
        drainage_result <- .otsi_drainage_mode(current_values)
        row[[indicator]] <- drainage_result$value

        if (isTRUE(drainage_result$tied)) {
          drainage_tie_units <- c(
            drainage_tie_units,
            current_key
          )
        }
      } else {
        stop(
          "No aggregation rule is currently defined for categorical ",
          "indicator '", indicator, "'.",
          call. = FALSE
        )
      }
    }

    rows[[i]] <- as.data.frame(
      row,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  prepared <- do.call(rbind, rows)
  rownames(prepared) <- NULL

  if (length(drainage_tie_units) > 0) {
    warning(
      "Drainage had a tied modal class and was set to NA for land ",
      "unit(s): ",
      paste(unique(drainage_tie_units), collapse = ", "),
      call. = FALSE
    )
  }

  list(
    data = prepared,
    repeated_units = repeated_units,
    drainage_tie_units = unique(drainage_tie_units),
    n_input_rows = nrow(data)
  )
}


# =========================================================
# 3. USER-FACING MULTIPLE-LAND-UNIT FUNCTION
# =========================================================

otsi_multiple <- function(
    data,
    unit_id,
    indicator_map,
    group_by = NULL,
    aggregate_within_unit = c("none", "mean", "median")
) {
  .otsi_multiple_require_core()

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble.", call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("'data' contains no rows.", call. = FALSE)
  }

  if (
    length(unit_id) != 1L ||
    is.na(unit_id) ||
    trimws(as.character(unit_id)) == ""
  ) {
    stop("'unit_id' must be one non-empty column name.", call. = FALSE)
  }

  unit_id <- trimws(as.character(unit_id))

  if (!unit_id %in% names(data)) {
    stop(
      "The land-unit identifier column was not found: ",
      unit_id,
      call. = FALSE
    )
  }

  indicator_map <- .otsi_normalize_indicator_map(indicator_map)
  aggregate_within_unit <- match.arg(aggregate_within_unit)

  missing_source_columns <- setdiff(
    unname(indicator_map),
    names(data)
  )

  if (length(missing_source_columns) > 0) {
    stop(
      "The following mapped dataset columns were not found: ",
      paste(missing_source_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (unit_id %in% unname(indicator_map)) {
    stop(
      "The land-unit identifier column cannot also be used as an ",
      "indicator column.",
      call. = FALSE
    )
  }

  if (is.null(group_by)) {
    group_by <- character(0)
  } else {
    group_by <- unique(trimws(as.character(group_by)))

    if (any(is.na(group_by) | group_by == "")) {
      stop(
        "'group_by' must contain valid column names.",
        call. = FALSE
      )
    }

    missing_group_columns <- setdiff(group_by, names(data))

    if (length(missing_group_columns) > 0) {
      stop(
        "The following grouping columns were not found: ",
        paste(missing_group_columns, collapse = ", "),
        call. = FALSE
      )
    }

    if (unit_id %in% group_by) {
      group_by <- setdiff(group_by, unit_id)
    }

    overlapping_group_columns <- intersect(
      group_by,
      unname(indicator_map)
    )

    if (length(overlapping_group_columns) > 0) {
      stop(
        "Grouping columns cannot also be mapped as indicators: ",
        paste(overlapping_group_columns, collapse = ", "),
        call. = FALSE
      )
    }
  }

  preparation <- .otsi_prepare_multiple_units(
    data = data,
    unit_id = unit_id,
    indicator_map = indicator_map,
    group_by = group_by,
    aggregate_within_unit = aggregate_within_unit
  )

  prepared <- preparation$data
  canonical_indicators <- names(indicator_map)
  registry <- otsi_registry_table()
  supplied_registry <- registry[
    match(canonical_indicators, registry$indicator),
    ,
    drop = FALSE
  ]

  all_score_columns <- registry$score_column
  result_rows <- vector("list", nrow(prepared))
  indicator_rows <- vector("list", nrow(prepared))

  for (i in seq_len(nrow(prepared))) {
    values <- lapply(
      canonical_indicators,
      function(indicator) {
        .otsi_as_scalar(prepared[[indicator]][i])
      }
    )
    names(values) <- canonical_indicators

    core_result <- otsi_calculate_core(values)

    complete_scores <- stats::setNames(
      rep(NA_real_, length(all_score_columns)),
      all_score_columns
    )
    complete_scores[names(core_result$indicator_scores)] <-
      core_result$indicator_scores

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

    output_row <- prepared[
      i,
      c(unit_id, group_by, canonical_indicators),
      drop = FALSE
    ]

    output_row$root_depth_class <- if (
      "root_depth" %in% canonical_indicators
    ) {
      core_result$standardized_input$root_depth_class
    } else {
      NA_integer_
    }

    output_row$drainage_fao <- if (
      "drainage" %in% canonical_indicators
    ) {
      core_result$standardized_input$drainage_fao
    } else {
      NA_character_
    }

    for (score_column in all_score_columns) {
      output_row[[score_column]] <- complete_scores[[score_column]]
    }

    output_row$chemical_n_indicators <- unname(
      core_result$indicator_counts[["chemical"]]
    )
    output_row$physical_n_indicators <- unname(
      core_result$indicator_counts[["physical"]]
    )
    output_row$total_n_indicators <- unname(
      core_result$indicator_counts[["total"]]
    )
    output_row$chemical_score <- unname(
      core_result$domain_scores[["chemical_score"]]
    )
    output_row$physical_score <- unname(
      core_result$domain_scores[["physical_score"]]
    )
    output_row$final_otsi <- unname(core_result$final_otsi)
    output_row$assessment_type <- core_result$assessment_type
    output_row$primary_limiter <- if (
      all(is.na(limiter_indicators))
    ) {
      NA_character_
    } else {
      paste(limiter_indicators, collapse = "; ")
    }
    output_row$primary_limiter_label <- if (
      all(is.na(limiter_labels))
    ) {
      NA_character_
    } else {
      paste(limiter_labels, collapse = "; ")
    }
    output_row$primary_limiter_score <- unname(
      core_result$primary_limiter$score
    )
    output_row$n_tied_primary_limiters <- unname(
      core_result$primary_limiter$n_tied
    )
    output_row$indicators_used <- if (
      length(core_result$indicators_used) == 0
    ) {
      NA_character_
    } else {
      paste(core_result$indicators_used, collapse = "; ")
    }
    output_row$indicators_missing_or_invalid <- if (
      length(core_result$indicators_missing_or_invalid) == 0
    ) {
      NA_character_
    } else {
      paste(
        core_result$indicators_missing_or_invalid,
        collapse = "; "
      )
    }
    output_row$indicator_set <- if (
      length(core_result$indicators_used) == 0
    ) {
      "none"
    } else {
      paste(sort(core_result$indicators_used), collapse = "|")
    }

    result_rows[[i]] <- output_row

    input_display <- vapply(
      values,
      function(x) {
        if (length(x) == 0 || is.na(x[[1]])) {
          return(NA_character_)
        }
        as.character(x[[1]])
      },
      character(1)
    )

    standardized_value <- rep(
      NA_character_,
      nrow(supplied_registry)
    )

    root_row <- supplied_registry$indicator == "root_depth"
    if (any(root_row)) {
      root_class <- core_result$standardized_input$root_depth_class
      standardized_value[root_row] <- if (is.na(root_class)) {
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

    score_values <- complete_scores[
      supplied_registry$score_column
    ]

    unit_indicator_table <- data.frame(
      indicator = supplied_registry$indicator,
      label = supplied_registry$label,
      domain = supplied_registry$domain,
      expected_unit = supplied_registry$unit,
      input_value = unname(input_display),
      standardized_value = standardized_value,
      score = as.numeric(score_values),
      valid_for_calculation = !is.na(
        as.numeric(score_values)
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Add the user-defined land-unit identifier
    unit_indicator_table[[unit_id]] <- rep(
      prepared[[unit_id]][i],
      nrow(supplied_registry)
    )
    
    # Place the land-unit identifier in the first column
    unit_indicator_table <- unit_indicator_table[
      ,
      c(
        unit_id,
        setdiff(
          names(unit_indicator_table),
          unit_id
        )
      ),
      drop = FALSE
    ]

    for (column in group_by) {
      unit_indicator_table[[column]] <- prepared[[column]][i]
    }

    indicator_rows[[i]] <- unit_indicator_table
  }

  scores <- do.call(rbind, result_rows)
  rownames(scores) <- NULL

  indicator_table <- do.call(rbind, indicator_rows)
  rownames(indicator_table) <- NULL

  assessment_count_table <- as.data.frame(
    table(scores$assessment_type),
    stringsAsFactors = FALSE
  )
  names(assessment_count_table) <- c("assessment_type", "n_units")
  assessment_count_table$percentage <-
    100 * assessment_count_table$n_units / nrow(scores)

  coverage_key <- paste(
    scores$assessment_type,
    scores$indicator_set,
    sep = "\r"
  )
  coverage_split <- split(seq_len(nrow(scores)), coverage_key)

  coverage_table <- do.call(
    rbind,
    lapply(
      coverage_split,
      function(index) {
        data.frame(
          assessment_type = scores$assessment_type[index[[1]]],
          indicator_set = scores$indicator_set[index[[1]]],
          n_units = length(index),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  rownames(coverage_table) <- NULL
  coverage_table$percentage <-
    100 * coverage_table$n_units / nrow(scores)
  coverage_table <- coverage_table[
    order(-coverage_table$n_units),
    ,
    drop = FALSE
  ]

  valid_final <- scores$final_otsi[!is.na(scores$final_otsi)]

  summary_table <- data.frame(
    method = "Open Tropical Soil Index",
    method_version = OTSI_METHOD_VERSION,
    n_input_rows = preparation$n_input_rows,
    n_land_units = nrow(scores),
    aggregate_within_unit = aggregate_within_unit,
    n_indicators_mapped = length(canonical_indicators),
    n_units_with_final_otsi = length(valid_final),
    n_units_without_final_otsi = sum(is.na(scores$final_otsi)),
    mean_final_otsi = .otsi_safe_stat(valid_final, "mean"),
    median_final_otsi = .otsi_safe_stat(valid_final, "median"),
    min_final_otsi = .otsi_safe_stat(valid_final, "min"),
    max_final_otsi = .otsi_safe_stat(valid_final, "max"),
    sd_final_otsi = .otsi_safe_stat(valid_final, "sd"),
    stringsAsFactors = FALSE
  )

  group_summary <- NULL

  if (length(group_by) > 0) {
    group_key <- .otsi_make_group_key(scores, group_by)
    group_split <- split(seq_len(nrow(scores)), group_key)

    group_summary_rows <- lapply(
      group_split,
      function(index) {
        row <- scores[index[[1]], group_by, drop = FALSE]
        row$n_land_units <- length(index)
        row$n_units_with_final_otsi <- sum(
          !is.na(scores$final_otsi[index])
        )
        row$mean_chemical_score <- .otsi_safe_stat(
          scores$chemical_score[index],
          "mean"
        )
        row$median_chemical_score <- .otsi_safe_stat(
          scores$chemical_score[index],
          "median"
        )
        row$mean_physical_score <- .otsi_safe_stat(
          scores$physical_score[index],
          "mean"
        )
        row$median_physical_score <- .otsi_safe_stat(
          scores$physical_score[index],
          "median"
        )
        row$mean_final_otsi <- .otsi_safe_stat(
          scores$final_otsi[index],
          "mean"
        )
        row$median_final_otsi <- .otsi_safe_stat(
          scores$final_otsi[index],
          "median"
        )
        row
      }
    )

    group_summary <- do.call(rbind, group_summary_rows)
    rownames(group_summary) <- NULL
  }

  varying_indicator_coverage <-
    length(unique(scores$indicator_set)) > 1

  no_final_units <- scores[
    is.na(scores$final_otsi),
    c(unit_id, "assessment_type", "indicator_set"),
    drop = FALSE
  ]

  if (varying_indicator_coverage) {
    warning(
      "Land units were calculated using different valid indicator ",
      "combinations. Direct comparison of partial OTSI scores should ",
      "therefore be made cautiously.",
      call. = FALSE
    )
  }

  if (nrow(no_final_units) > 0) {
    warning(
      nrow(no_final_units),
      " land unit(s) did not receive a final OTSI because at least ",
      "one chemical and one physical indicator are required.",
      call. = FALSE
    )
  }

  output <- list(
    method = list(
      name = "Open Tropical Soil Index",
      version = OTSI_METHOD_VERSION
    ),
    scores = scores,
    indicator_table = indicator_table,
    summary = summary_table,
    assessment_counts = assessment_count_table,
    indicator_coverage = coverage_table,
    group_summary = group_summary,
    metadata = list(
      unit_id = unit_id,
      group_by = group_by,
      indicator_map = indicator_map,
      indicators_mapped = canonical_indicators,
      registered_indicators_not_mapped = setdiff(
        names(OTSI_INDICATOR_REGISTRY),
        canonical_indicators
      ),
      aggregate_within_unit = aggregate_within_unit
    ),
    qc = list(
      repeated_units_in_input = preparation$repeated_units,
      drainage_mode_tie_units = preparation$drainage_tie_units,
      varying_indicator_coverage = varying_indicator_coverage,
      units_without_final_otsi = no_final_units
    )
  )

  class(output) <- c(
    "otsi_multiple_result",
    "list"
  )

  output
}


# =========================================================
# 4. PRINT AND DATA-FRAME METHODS
# =========================================================

print.otsi_multiple_result <- function(x, ...) {
  cat(
    "Open Tropical Soil Index ",
    x$method$version,
    "\n",
    sep = ""
  )
  cat(
    "Land units calculated: ",
    nrow(x$scores),
    "\n",
    sep = ""
  )
  cat(
    "Input rows: ",
    x$summary$n_input_rows,
    "\n",
    sep = ""
  )
  cat(
    "Indicators mapped: ",
    x$summary$n_indicators_mapped,
    " of ",
    length(OTSI_INDICATOR_REGISTRY),
    "\n",
    sep = ""
  )
  cat(
    "Within-unit aggregation: ",
    x$summary$aggregate_within_unit,
    "\n",
    sep = ""
  )
  cat(
    "Units with final OTSI: ",
    x$summary$n_units_with_final_otsi,
    "\n",
    sep = ""
  )
  cat(
    "Units without final OTSI: ",
    x$summary$n_units_without_final_otsi,
    "\n",
    sep = ""
  )
  cat(
    "Median final OTSI: ",
    ifelse(
      is.na(x$summary$median_final_otsi),
      "NA",
      sprintf("%.3f", x$summary$median_final_otsi)
    ),
    "\n",
    sep = ""
  )
  cat(
    "Mean final OTSI: ",
    ifelse(
      is.na(x$summary$mean_final_otsi),
      "NA",
      sprintf("%.3f", x$summary$mean_final_otsi)
    ),
    "\n",
    sep = ""
  )

  if (isTRUE(x$qc$varying_indicator_coverage)) {
    cat(
      "QC note: indicator coverage differs among land units.\n"
    )
  }

  invisible(x)
}


as.data.frame.otsi_multiple_result <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...
) {
  x$scores
}






