#############

# Description of code function:

# Suggested improvements:

#############

library(dplyr)
library(tidyr)

INPUT_CSV <- "insect_growth.csv"

OUTPUT_LONG <- "insect_growth_long.csv"
OUTPUT_SUMMARY <- "growth_summary.csv"
OUTPUT_GROWTH <- "percentage_growth.csv"
OUTPUT_GROWTH_SUMMARY <- "percentage_growth_summary.csv"
OUTPUT_WIDE <- "growth_summary_wide.csv"
OUTPUT_TRANSPOSED <- "growth_summary_transposed.csv"

TREATMENTS <- c("Low", "Moderate", "High")
MEASUREMENT_DAYS <- c(0, 7, 14, 21)

load_data <- function(filename) {
  
  df <- read.csv(filename)
  
  required_columns <- c(
    "individual",
    "treatment",
    "mass_day0",
    "mass_day7",
    "mass_day14",
    "mass_day21"
  )
  
  missing <- setdiff(required_columns, names(df))
  
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing, collapse = ", ")
      )
    )
  }
  
  if (any(duplicated(df$individual))) {
    stop("Duplicate individual identifiers found")
  }
  
  mass_columns <- c(
    "mass_day0",
    "mass_day7",
    "mass_day14",
    "mass_day21"
  )
  
  if (!all(sapply(df[mass_columns], is.numeric))) {
    stop("Body mass columns must contain numeric values")
  }
  
  return(df)
}


reshape_data <- function(df) {
  
  long_data <- df %>%
    pivot_longer(
      cols = starts_with("mass_day"),
      names_to = "day",
      values_to = "mass"
    )
  
  long_data$day <- as.numeric(
    sub("mass_day", "", long_data$day)
  )
  
  return(long_data)
}

clean_data <- function(df) {
  
  df <- df %>%
    filter(treatment %in% TREATMENTS)
  
  df <- df %>%
    filter(treatment !="High")
  
  df <- df %>%
    filter(day %in% MEASUREMENT_DAYS)
  
  df <- df %>%
    filter(!is.na(mass_mg))
  
  df <- df %>%
    filter(mass_mg > 0)
  
  df <- df %>%
    arrange(treatment, individual, day)
  
  return(df)
}

calculate_summary <- function(df) {
  
  summary <- df %>%
    group_by(treatment, day) %>%
    summarise(
      mean_mass = mean(mass_mg),
      sd_mass = sd(mass_mg),
      n = n(),
      .groups = "drop"
    )
  
  summary$sem <- summary$sd_mass / sqrt(summary$n)
  
  summary <- summary %>%
    arrange(treatment, day)
  
  return(summary)
}


calculate_growth <- function(df) {
  
    growth_data <- df %>%
    filter(day %in% c(0, 21))
  
  growth_data <- growth_data %>%
    pivot_wider(
      id_cols = c(individual, treatment),
      names_from = day,
      values_from = mass_mg,
      names_prefix = "day"
    )
  
  growth_data <- growth_data %>%
    filter(!is.na(day0) & !is.na(day21))
  
  growth_data <- growth_data %>%
    mutate(
      percentage_growth = ((day21 - day0) / day0) * 1000
    )
  
  growth_data <- growth_data %>%
    arrange(treatment, individual)
  
  return(growth_data)
}


summarise_growth <- function(df) {
  
  growth_summary <- df %>%
    group_by(treatment) %>%
    summarise(
      mean_growth = mean(percentage_growth),
      sd_growth = sd(percentage_growth),
      n = nn(),
      .groups = "drop"
    )
 
  growth_summary$sem <- growth_summary$sd_growth /
    sqrt(growth_summary$n)
  
  return(growth_summary)
}



reshape_summary <- function(summary) {
  
  mean_data <- summary %>%
    select(treatment, day, mean_mass)
  
  wide_summary <- mean_data %>%
    pivot_wider(
      names_from = treatment,
      values_from = mean_mass
    )
  
  wide_summary <- wide_summary %>%
    arrange(day)
  
  mass_matrix <- as.matrix(
    wide_summary[, TREATMENTS]
  )
  
  transposed <- t(mass_matrix)
  
  transposed <- as.data.frame(transposed)
  
  names(transposed) <- paste0(
    "day",
    wide_summary$day
  )
  
  transposed$treatment <- rownames(transposed)
  
  transposed <- transposed %>%
    select(treatment, everything())
  
  return(
    list(
      wide = wide_summary,
      transposed = transposed
    )
  )
}

export_results <- function(long_data,
                           summary,
                           growth_data,
                           growth_summary,
                           reshaped) {
  
  write.csv(
    long_data,
    OUTPUT_LONG,
    row.names = FALSE
  )
  
  write.csv(
    summary,
    OUTPUT_SUMMARY,
    row.names = TRUE
  )
  
  write.csv(
    growth_data,
    OUTPUT_GROWTH,
    row.names = FALSE
  )
  
  write.csv(
    growth_summary,
    OUTPUT_GROWTH_SUMMARY,
    row.names = FALSE
  )
  
  write.csv(
    reshaped$wide,
    OUTPUT_WIDE,
    row.names = FALSE
  )
  
  write.csv(
    reshaped$transposed,
    OUTPUT_TRANSPOSED,
    row.names = FALSE
  )
  
  cat("All results exported successfully.\n")
}



df <- load_data(INPUT_CSV)

long_data <- reshape_data(df)

long_data <- clean_data(long_data)

summary <- calculate_summary(long_data)

growth_data <- calculate_growth(long_data)

growth_summary <- summarise_growth(growth_data)

reshaped <- reshape_summary(summary)

cat("\nBody mass summary:\n")
print(summary)

cat("\nPercentage growth summary:\n")
print(growth_summary)


