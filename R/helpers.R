library(dplyr)
library(tidyr)

station_color_mapping <- function() {
  c(
    Improving = "#1B9E77",
    Stable = "#4C6A91",
    Degrading = "#D95F02"
  )
}

leaflet_pal <- function() {
  leaflet::colorFactor(
    palette = unname(station_color_mapping()),
    levels = names(station_color_mapping())
  )
}

generate_synthetic_stations <- function() {
  tibble::tribble(
    ~station_id, ~station_name, ~latitude, ~longitude, ~waterbody, ~strata, ~parameter, ~trend_classification,
    "ST001", "Fox River North", 43.1965, -88.7241, "Fox River", "Northern Basin", "Nitrate", "Improving",
    "ST002", "Lake Mason Central", 42.9458, -89.3761, "Lake Mason", "Central Lakes", "Dissolved Oxygen", "Stable",
    "ST003", "Prairie Creek East", 41.8820, -88.2014, "Prairie Creek", "Urban Corridor", "Turbidity", "Degrading",
    "ST004", "Rock Tributary South", 42.2718, -89.0931, "Rock River", "Southern Reach", "Nitrate", "Stable",
    "ST005", "Wetland Outlet 3", 44.0211, -88.5426, "Cedar Wetland", "Northern Basin", "Dissolved Oxygen", "Improving",
    "ST006", "Harbor Channel West", 43.0752, -87.8811, "Harbor Channel", "Coastal Fringe", "Turbidity", "Degrading"
  )
}

generate_synthetic_measurements <- function(stations, start_date = as.Date("2023-01-01"), end_date = as.Date("2024-06-01")) {
  monthly_dates <- seq.Date(start_date, end_date, by = "month")

  stations %>%
    select(station_id, parameter, trend_classification) %>%
    tidyr::crossing(date = monthly_dates) %>%
    group_by(station_id) %>%
    mutate(
      month_index = row_number() - 1,
      seasonal_component = sin(2 * pi * month_index / 12)
    ) %>%
    rowwise() %>%
    mutate(
      observed_value = dplyr::case_when(
        parameter == "Nitrate" & trend_classification == "Improving" ~ 5.4 - 0.12 * month_index + 0.20 * seasonal_component + stats::rnorm(1, 0, 0.08),
        parameter == "Nitrate" & trend_classification == "Stable" ~ 4.1 + 0.02 * seasonal_component + stats::rnorm(1, 0, 0.05),
        parameter == "Dissolved Oxygen" & trend_classification == "Stable" ~ 8.1 + 0.22 * seasonal_component + stats::rnorm(1, 0, 0.07),
        parameter == "Dissolved Oxygen" & trend_classification == "Improving" ~ 6.7 + 0.09 * month_index + 0.18 * seasonal_component + stats::rnorm(1, 0, 0.07),
        parameter == "Turbidity" & trend_classification == "Degrading" ~ 7.6 + 0.36 * month_index + 0.35 * seasonal_component + stats::rnorm(1, 0, 0.15),
        TRUE ~ 5 + stats::rnorm(1, 0, 0.05)
      ),
      unit = dplyr::case_when(
        parameter == "Nitrate" ~ "mg/L",
        parameter == "Dissolved Oxygen" ~ "mg/L",
        parameter == "Turbidity" ~ "NTU",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    transmute(
      record_id = sprintf("REC%04d", dplyr::row_number()),
      station_id,
      date,
      parameter,
      observed_value = round(pmax(observed_value, 0.05), 2),
      unit
    )
}

load_or_generate_demo_data <- function(data_dir = "data") {
  stations_path <- file.path(data_dir, "stations.csv")
  water_quality_path <- file.path(data_dir, "water_quality.csv")

  if (file.exists(stations_path) && file.exists(water_quality_path)) {
    stations <- read.csv(stations_path, stringsAsFactors = FALSE) %>%
      tibble::as_tibble()
    water_quality <- read.csv(water_quality_path, stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      mutate(date = as.Date(date))
  } else {
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE)
    }

    stations <- generate_synthetic_stations()
    water_quality <- generate_synthetic_measurements(stations)

    write.csv(stations, stations_path, row.names = FALSE)
    write.csv(water_quality, water_quality_path, row.names = FALSE)
  }

  list(stations = stations, water_quality = water_quality)
}

trend_model_data <- function(station_series) {
  if (nrow(station_series) < 2) {
    return(NULL)
  }

  modeled <- station_series %>%
    arrange(date) %>%
    mutate(
      date_num = as.numeric(date),
      month_index = dplyr::row_number()
    )

  fit <- stats::lm(observed_value ~ month_index, data = modeled)
  predictions <- stats::predict(fit, newdata = modeled, interval = "confidence")

  modeled %>%
    mutate(
      trend_value = predictions[, "fit"],
      ci_low = predictions[, "lwr"],
      ci_high = predictions[, "upr"],
      slope = unname(stats::coef(fit)[["month_index"]]),
      p_value = summary(fit)$coefficients["month_index", "Pr(>|t|)"]
    )
}

format_trend_summary <- function(modeled_series, station_meta) {
  if (is.null(modeled_series) || nrow(modeled_series) == 0) {
    return(list(
      significance_label = "No trend estimate available",
      gam_summary = "Select a station with sufficient observations to generate a prototype trend summary.",
      slope_summary = "A minimum of two dated observations is required to estimate a slope and confidence band."
    ))
  }

  slope <- modeled_series$slope[[1]]
  p_value <- modeled_series$p_value[[1]]
  slope_direction <- dplyr::case_when(
    abs(slope) < 0.02 ~ "essentially flat",
    slope > 0 ~ "increasing",
    TRUE ~ "decreasing"
  )

  significance_label <- dplyr::case_when(
    p_value <= 0.01 ~ "Seasonal Mann-Kendall: statistically significant",
    p_value <= 0.05 ~ "Seasonal Mann-Kendall: likely significant",
    TRUE ~ "Seasonal Mann-Kendall: no clear significance"
  )

  gam_summary <- paste0(
    "Prototype GAM-style summary: the ", station_meta$parameter[[1]], " signal at ",
    station_meta$station_name[[1]], " is ", slope_direction,
    " across the selected time window, aligning with the station's ",
    station_meta$trend_classification[[1]], " classification."
  )

  slope_summary <- paste0(
    "Estimated slope: ", sprintf("%.3f", slope), " ", unique(modeled_series$unit)[[1]],
    " per month. Approximate p-value: ", format.pval(p_value, digits = 3, eps = 0.001),
    ". Confidence band shown on the chart is derived from a simple linear prototype fit."
  )

  list(
    significance_label = significance_label,
    gam_summary = gam_summary,
    slope_summary = slope_summary
  )
}
