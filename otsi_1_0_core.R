# =========================================================
# OPEN TROPICAL SOIL INDEX (OTSI) 1.0
# CORE SCORING AND AGGREGATION ENGINE
# =========================================================
#
# This module contains reusable OTSI 1.0 calculation rules.
# It has no file paths, raster processing, plots, or
# dataset-specific column names.
#
# Rules:
# - Any subset of the nine registered indicators may be used.
# - Final OTSI requires at least one chemical and one physical indicator.
# - Root depth input is centimetres and is internally classified.
# - Drainage input must use accepted FAO codes.
# =========================================================

OTSI_METHOD_VERSION <- "1.0"


# =========================================================
# 1. INTERNAL HELPERS
# =========================================================

.otsi_require_obic <- function() {
  if (!requireNamespace("OBIC", quietly = TRUE)) {
    stop(
      "The 'OBIC' package is required. Install it before ",
      "running the OTSI functions.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


.otsi_prepare_numeric <- function(
    x,
    indicator,
    minimum = -Inf,
    maximum = Inf,
    minimum_inclusive = TRUE,
    maximum_inclusive = TRUE
) {
  original_na <- is.na(x)

  if (!is.numeric(x)) {
    converted <- suppressWarnings(as.numeric(x))
    newly_missing <- !original_na & is.na(converted)

    if (any(newly_missing)) {
      warning(
        "Some values for '", indicator,
        "' could not be converted to numeric and were set to NA.",
        call. = FALSE
      )
    }
    x <- converted
  } else {
    x <- as.numeric(x)
  }

  below_minimum <- if (minimum_inclusive) x < minimum else x <= minimum
  above_maximum <- if (maximum_inclusive) x > maximum else x >= maximum

  invalid <- !is.na(x) & (
    below_minimum | above_maximum | !is.finite(x)
  )

  if (any(invalid)) {
    warning(
      "Some values for '", indicator,
      "' were outside the accepted input range and were set to NA.",
      call. = FALSE
    )
    x[invalid] <- NA_real_
  }

  x
}


.otsi_cap_01 <- function(x) {
  pmax(pmin(x, 1), 0)
}


# =========================================================
# 2. GENERIC CONTINUOUS SCORING FUNCTIONS
# =========================================================

score_linear_more <- function(x, lower, upper) {
  if (upper <= lower) {
    stop("'upper' must be greater than 'lower'.", call. = FALSE)
  }

  score <- (x - lower) / (upper - lower)
  score <- .otsi_cap_01(score)

  ifelse(is.na(x), NA_real_, score)
}


score_linear_less <- function(x, optimum, restrictive) {
  if (restrictive <= optimum) {
    stop(
      "'restrictive' must be greater than 'optimum'.",
      call. = FALSE
    )
  }

  score <- (restrictive - x) / (restrictive - optimum)
  score <- .otsi_cap_01(score)

  ifelse(is.na(x), NA_real_, score)
}


score_obic_logistic_more <- function(
    x,
    lower,
    upper,
    b = 6,
    x0 = 0.5,
    v = 1
) {
  .otsi_require_obic()

  if (upper <= lower) {
    stop("'upper' must be greater than 'lower'.", call. = FALSE)
  }

  z <- (x - lower) / (upper - lower)

  raw <- OBIC::evaluate_logistic(
    x = z, b = b, x0 = x0, v = v, increasing = TRUE
  )
  raw_lower <- OBIC::evaluate_logistic(
    x = 0, b = b, x0 = x0, v = v, increasing = TRUE
  )
  raw_upper <- OBIC::evaluate_logistic(
    x = 1, b = b, x0 = x0, v = v, increasing = TRUE
  )

  score <- (raw - raw_lower) / (raw_upper - raw_lower)
  score <- .otsi_cap_01(score)

  ifelse(is.na(x), NA_real_, score)
}


score_obic_logistic_less <- function(
    x,
    lower,
    upper,
    b = 6,
    x0 = 0.5,
    v = 1
) {
  .otsi_require_obic()

  if (upper <= lower) {
    stop("'upper' must be greater than 'lower'.", call. = FALSE)
  }

  z <- (x - lower) / (upper - lower)

  raw <- OBIC::evaluate_logistic(
    x = z, b = b, x0 = x0, v = v, increasing = FALSE
  )
  raw_lower <- OBIC::evaluate_logistic(
    x = 0, b = b, x0 = x0, v = v, increasing = FALSE
  )
  raw_upper <- OBIC::evaluate_logistic(
    x = 1, b = b, x0 = x0, v = v, increasing = FALSE
  )

  score <- (raw - raw_upper) / (raw_lower - raw_upper)
  score <- .otsi_cap_01(score)

  ifelse(is.na(x), NA_real_, score)
}


score_obic_parabolic_plateau <- function(x, lower, upper) {
  .otsi_require_obic()

  if (upper <= lower) {
    stop("'upper' must be greater than 'lower'.", call. = FALSE)
  }

  z <- (x - lower) / (upper - lower)
  z <- .otsi_cap_01(z)

  score <- OBIC::evaluate_parabolic(x = z, x.top = 1)
  score <- .otsi_cap_01(score)

  ifelse(is.na(x), NA_real_, score)
}


# =========================================================
# 3. INDICATOR-SPECIFIC SCORING FUNCTIONS
# =========================================================

score_otsi_soc <- function(x) {
  x <- .otsi_prepare_numeric(
    x, "soc", minimum = 0
  )

  score_obic_logistic_more(
    x, lower = 1.0, upper = 4.4, b = 6, x0 = 0.5, v = 1
  )
}


score_otsi_total_n <- function(x) {
  x <- .otsi_prepare_numeric(
    x, "total_n", minimum = 0
  )

  score_obic_parabolic_plateau(
    x, lower = 1.0, upper = 3.1
  )
}


score_otsi_effective_cec <- function(x) {
  x <- .otsi_prepare_numeric(
    x, "effective_cec", minimum = 0
  )

  score_linear_more(
    x, lower = 6, upper = 10
  )
}


score_otsi_ph <- function(
    x,
    lower_limit = 4.00,
    lower_optimum = 6.81,
    upper_optimum = 7.20,
    upper_limit = 8.50,
    b = 6,
    x0 = 0.5,
    v = 1
) {
  .otsi_require_obic()

  x <- .otsi_prepare_numeric(
    x, "ph", minimum = 0, maximum = 14
  )

  if (
    lower_limit >= lower_optimum ||
    lower_optimum > upper_optimum ||
    upper_optimum >= upper_limit
  ) {
    stop("The pH thresholds are not in the correct order.", call. = FALSE)
  }

  distance_normalized <- rep(NA_real_, length(x))

  optimum <- !is.na(x) & x >= lower_optimum & x <= upper_optimum
  acidic <- !is.na(x) & x < lower_optimum
  alkaline <- !is.na(x) & x > upper_optimum

  distance_normalized[optimum] <- 0
  distance_normalized[acidic] <- (
    lower_optimum - x[acidic]
  ) / (
    lower_optimum - lower_limit
  )
  distance_normalized[alkaline] <- (
    x[alkaline] - upper_optimum
  ) / (
    upper_limit - upper_optimum
  )

  raw <- OBIC::evaluate_logistic(
    x = distance_normalized,
    b = b,
    x0 = x0,
    v = v,
    increasing = FALSE
  )
  raw_best <- OBIC::evaluate_logistic(
    x = 0, b = b, x0 = x0, v = v, increasing = FALSE
  )
  raw_limit <- OBIC::evaluate_logistic(
    x = 1, b = b, x0 = x0, v = v, increasing = FALSE
  )

  score <- (raw - raw_limit) / (raw_best - raw_limit)
  score <- .otsi_cap_01(score)

  ifelse(is.na(x), NA_real_, score)
}


score_otsi_esp <- function(x) {
  x <- .otsi_prepare_numeric(
    x, "esp", minimum = 0, maximum = 100
  )

  score_obic_logistic_less(
    x, lower = 15, upper = 30, b = 6, x0 = 0.5, v = 1
  )
}


score_otsi_bulk_density <- function(x) {
  x <- .otsi_prepare_numeric(
    x,
    "bulk_density",
    minimum = 0,
    minimum_inclusive = FALSE
  )

  score_linear_less(
    x, optimum = 0.95, restrictive = 1.40
  )
}


# Root-depth classes:
# 1 = >100 cm
# 2 = 50-100 cm
# 3 = 10-<50 cm
# 4 = 0-<10 cm

classify_root_depth_fao <- function(x) {
  x <- .otsi_prepare_numeric(
    x, "root_depth", minimum = 0
  )

  root_class <- rep(NA_integer_, length(x))

  root_class[!is.na(x) & x > 100] <- 1L
  root_class[!is.na(x) & x >= 50 & x <= 100] <- 2L
  root_class[!is.na(x) & x >= 10 & x < 50] <- 3L
  root_class[!is.na(x) & x >= 0 & x < 10] <- 4L

  root_class
}


score_otsi_root_depth <- function(x) {
  root_class <- classify_root_depth_fao(x)

  score_lookup <- c(
    "1" = 1.00,
    "2" = 0.75,
    "3" = 0.40,
    "4" = 0.10
  )

  unname(score_lookup[as.character(root_class)])
}


# Accepted drainage codes:
# W/WD, MW, SE, I, P, VP, E/ED

normalize_drainage_fao <- function(
    x,
    warn_unknown = TRUE
) {
  drainage <- toupper(trimws(as.character(x)))
  drainage[drainage == ""] <- NA_character_
  drainage[drainage == "WD"] <- "W"
  drainage[drainage == "ED"] <- "E"

  accepted <- c("W", "MW", "SE", "I", "P", "VP", "E")
  unknown <- !is.na(drainage) & !drainage %in% accepted

  if (warn_unknown && any(unknown)) {
    warning(
      "Unrecognized FAO drainage codes were set to NA: ",
      paste(sort(unique(drainage[unknown])), collapse = ", "),
      call. = FALSE
    )
  }

  drainage[unknown] <- NA_character_
  drainage
}


score_otsi_drainage <- function(x) {
  drainage <- normalize_drainage_fao(x)

  score_lookup <- c(
    "W" = 1.00,
    "MW" = 0.90,
    "SE" = 0.70,
    "I" = 0.60,
    "P" = 0.40,
    "VP" = 0.00,
    "E" = 0.00
  )

  unname(score_lookup[drainage])
}


score_otsi_awc <- function(x) {
  x <- .otsi_prepare_numeric(
    x, "awc", minimum = 0
  )

  score_obic_logistic_more(
    x, lower = 50, upper = 150, b = 6, x0 = 0.5, v = 1
  )
}


# =========================================================
# 4. OTSI 1.0 INDICATOR REGISTRY
# =========================================================

OTSI_INDICATOR_REGISTRY <- list(
  soc = list(
    label = "SOC",
    domain = "chemical",
    unit = "%",
    input_type = "numeric",
    score_column = "score_soc",
    score_function = score_otsi_soc
  ),
  total_n = list(
    label = "Total N",
    domain = "chemical",
    unit = "g kg-1",
    input_type = "numeric",
    score_column = "score_n",
    score_function = score_otsi_total_n
  ),
  effective_cec = list(
    label = "Effective CEC",
    domain = "chemical",
    unit = "cmol(+) kg-1",
    input_type = "numeric",
    score_column = "score_cec",
    score_function = score_otsi_effective_cec
  ),
  ph = list(
    label = "pH",
    domain = "chemical",
    unit = "pH H2O",
    input_type = "numeric",
    score_column = "score_ph",
    score_function = score_otsi_ph
  ),
  esp = list(
    label = "ESP",
    domain = "chemical",
    unit = "%",
    input_type = "numeric",
    score_column = "score_esp",
    score_function = score_otsi_esp
  ),
  bulk_density = list(
    label = "Bulk density",
    domain = "physical",
    unit = "g cm-3",
    input_type = "numeric",
    score_column = "score_bulk_density",
    score_function = score_otsi_bulk_density
  ),
  root_depth = list(
    label = "Root depth",
    domain = "physical",
    unit = "cm",
    input_type = "numeric",
    score_column = "score_root_depth",
    score_function = score_otsi_root_depth
  ),
  drainage = list(
    label = "Drainage",
    domain = "physical",
    unit = "FAO drainage code",
    input_type = "categorical",
    score_column = "score_drainage",
    score_function = score_otsi_drainage
  ),
  awc = list(
    label = "AWC",
    domain = "physical",
    unit = "mm",
    input_type = "numeric",
    score_column = "score_awc",
    score_function = score_otsi_awc
  )
)


otsi_registry_table <- function(
    registry = OTSI_INDICATOR_REGISTRY
) {
  data.frame(
    indicator = names(registry),
    label = vapply(registry, function(x) x$label, character(1)),
    domain = vapply(registry, function(x) x$domain, character(1)),
    unit = vapply(registry, function(x) x$unit, character(1)),
    input_type = vapply(
      registry, function(x) x$input_type, character(1)
    ),
    score_column = vapply(
      registry, function(x) x$score_column, character(1)
    ),
    stringsAsFactors = FALSE
  )
}


# =========================================================
# 5. OSI-STYLE AGGREGATION
# =========================================================

otsi_subscore <- function(scores) {
  scores <- as.numeric(scores)
  scores <- scores[!is.na(scores)]

  if (length(scores) == 0) {
    return(NA_real_)
  }

  if (any(scores < 0 | scores > 1)) {
    stop(
      "All indicator scores must be between 0 and 1.",
      call. = FALSE
    )
  }

  indicator_weights <- 1 / (scores + 0.2)

  sum(
    scores * (
      indicator_weights / sum(indicator_weights)
    )
  )
}


otsi_final_score <- function(
    chemical_score,
    physical_score,
    chemical_n_indicators,
    physical_n_indicators
) {
  valid_chemical <- (
    length(chemical_score) == 1 &&
    !is.na(chemical_score) &&
    length(chemical_n_indicators) == 1 &&
    !is.na(chemical_n_indicators) &&
    chemical_n_indicators > 0
  )

  valid_physical <- (
    length(physical_score) == 1 &&
    !is.na(physical_score) &&
    length(physical_n_indicators) == 1 &&
    !is.na(physical_n_indicators) &&
    physical_n_indicators > 0
  )

  if (!valid_chemical || !valid_physical) {
    return(NA_real_)
  }

  category_scores <- c(chemical_score, physical_score)
  category_weights <- log(
    c(chemical_n_indicators, physical_n_indicators) + 1
  )

  sum(
    category_scores * (
      category_weights / sum(category_weights)
    )
  )
}


# =========================================================
# 6. PRIMARY LIMITING INDICATOR
# =========================================================

otsi_primary_limiter <- function(
    scores,
    tolerance = 1e-10
) {
  if (is.null(names(scores))) {
    stop("'scores' must be a named numeric vector.", call. = FALSE)
  }

  score_names <- names(scores)
  scores <- as.numeric(scores)
  names(scores) <- score_names

  valid <- !is.na(scores)

  if (!any(valid)) {
    return(
      list(
        indicator = NA_character_,
        score = NA_real_,
        n_tied = 0L
      )
    )
  }

  minimum_score <- min(scores[valid])

  limiting_names <- names(scores)[
    valid & abs(scores - minimum_score) <= tolerance
  ]

  list(
    indicator = limiting_names,
    score = minimum_score,
    n_tied = length(limiting_names)
  )
}


# =========================================================
# 7. CORE CALCULATION FOR ONE LAND UNIT
# =========================================================

otsi_calculate_core <- function(
    values,
    registry = OTSI_INDICATOR_REGISTRY
) {
  if (
    !is.list(values) ||
    is.null(names(values)) ||
    any(names(values) == "")
  ) {
    stop("'values' must be a named list.", call. = FALSE)
  }

  if (anyDuplicated(names(values))) {
    stop("Indicator names in 'values' must be unique.", call. = FALSE)
  }

  supplied_names <- names(values)
  recognized_names <- intersect(supplied_names, names(registry))
  unknown_names <- setdiff(supplied_names, names(registry))

  if (length(unknown_names) > 0) {
    warning(
      "Unrecognized indicators were ignored: ",
      paste(unknown_names, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(recognized_names) == 0) {
    stop("No recognized OTSI indicators were supplied.", call. = FALSE)
  }

  value_lengths <- vapply(
    values[recognized_names], length, integer(1)
  )

  if (any(value_lengths != 1L)) {
    stop(
      "otsi_calculate_core() accepts one value per indicator. ",
      "The multiple-unit wrapper will handle tables.",
      call. = FALSE
    )
  }

  indicator_scores <- numeric(0)

  for (indicator_name in recognized_names) {
    definition <- registry[[indicator_name]]
    current_score <- definition$score_function(
      values[[indicator_name]]
    )
    indicator_scores[definition$score_column] <- current_score
  }

  registry_table <- otsi_registry_table(registry)

  score_metadata <- registry_table[
    match(names(indicator_scores), registry_table$score_column),
  ]

  chemical_scores <- indicator_scores[
    score_metadata$domain == "chemical"
  ]
  physical_scores <- indicator_scores[
    score_metadata$domain == "physical"
  ]

  chemical_n <- sum(!is.na(chemical_scores))
  physical_n <- sum(!is.na(physical_scores))

  chemical_score <- otsi_subscore(chemical_scores)
  physical_score <- otsi_subscore(physical_scores)

  final_otsi <- otsi_final_score(
    chemical_score = chemical_score,
    physical_score = physical_score,
    chemical_n_indicators = chemical_n,
    physical_n_indicators = physical_n
  )

  limiter <- otsi_primary_limiter(indicator_scores)
  total_valid <- chemical_n + physical_n

  assessment_type <- if (
    chemical_n > 0 &&
    physical_n > 0 &&
    total_valid == length(registry)
  ) {
    "Full OTSI"
  } else if (
    chemical_n > 0 &&
    physical_n > 0
  ) {
    "Partial OTSI"
  } else if (chemical_n > 0) {
    "Chemical-domain result only"
  } else if (physical_n > 0) {
    "Physical-domain result only"
  } else {
    "No valid result"
  }

  standardized_values <- list()

  if ("root_depth" %in% recognized_names) {
    standardized_values$root_depth_class <-
      classify_root_depth_fao(values$root_depth)
  }

  if ("drainage" %in% recognized_names) {
    standardized_values$drainage_fao <-
      normalize_drainage_fao(
        values$drainage,
        warn_unknown = FALSE
      )
  }

  output <- list(
    method = list(
      name = "Open Tropical Soil Index",
      version = OTSI_METHOD_VERSION
    ),
    input = values[recognized_names],
    standardized_input = standardized_values,
    indicator_scores = indicator_scores,
    domain_scores = c(
      chemical_score = chemical_score,
      physical_score = physical_score
    ),
    final_otsi = final_otsi,
    indicator_counts = c(
      chemical = chemical_n,
      physical = physical_n,
      total = total_valid
    ),
    assessment_type = assessment_type,
    primary_limiter = limiter,
    indicators_used = score_metadata$indicator[
      !is.na(indicator_scores)
    ],
    indicators_missing_or_invalid = score_metadata$indicator[
      is.na(indicator_scores)
    ],
    registered_indicators_not_supplied = setdiff(
      names(registry),
      recognized_names
    )
  )

  class(output) <- c("otsi_result", "list")
  output
}


# =========================================================
# 8. PRINT METHOD
# =========================================================

print.otsi_result <- function(x, ...) {
  cat(
    "Open Tropical Soil Index ", x$method$version, "\n",
    sep = ""
  )
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

  limiter_label <- if (
    length(x$primary_limiter$indicator) == 0 ||
    all(is.na(x$primary_limiter$indicator))
  ) {
    "NA"
  } else {
    paste(x$primary_limiter$indicator, collapse = "; ")
  }

  cat(
    "Primary limiting indicator(s): ",
    limiter_label,
    "\n",
    sep = ""
  )

  invisible(x)
}


# =========================================================
# 9. SELF-TESTS
# =========================================================

otsi_self_test <- function() {
  .otsi_require_obic()
  tolerance <- 1e-8

  stopifnot(
    isTRUE(
      all.equal(
        score_otsi_soc(c(1.0, 4.4)),
        c(0, 1),
        tolerance = tolerance
      )
    )
  )

  stopifnot(
    isTRUE(
      all.equal(
        score_otsi_total_n(c(1.0, 3.1, 5.0)),
        c(0, 1, 1),
        tolerance = tolerance
      )
    )
  )

  stopifnot(
    identical(
      classify_root_depth_fao(
        c(101, 100, 50, 49.9, 10, 9.9)
      ),
      c(1L, 2L, 2L, 3L, 3L, 4L)
    )
  )

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

  chemical_only <- otsi_calculate_core(
    list(soc = 2.0, ph = 6.5)
  )
  stopifnot(is.na(chemical_only$final_otsi))

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
    identical(partial_result$assessment_type, "Partial OTSI")
  )

  message("All OTSI 1.0 core self-tests passed.")
  invisible(TRUE)
}


# =========================================================
# 10. EXAMPLE
# =========================================================
#
# source("otsi_1_0_core.R")
# otsi_self_test()
#
# example_result <- otsi_calculate_core(
#   list(
#     soc = 1.8,
#     ph = 6.4,
#     root_depth = 75,
#     drainage = "MW"
#   )
# )
#
# print(example_result)
# otsi_registry_table()
# =========================================================
