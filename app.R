library(shiny)
library(bslib)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(htmltools)

source("R/helpers.R")

demo_data <- load_or_generate_demo_data()

stations <- demo_data$stations %>%
  mutate(
    trend_classification = factor(
      trend_classification,
      levels = c("Improving", "Stable", "Degrading")
    )
  )

water_quality <- demo_data$water_quality %>%
  mutate(
    date = as.Date(date),
    observed_value = as.numeric(observed_value)
  )

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B5C74",
  secondary = "#6C7A89",
  success = "#1B9E77",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Merriweather Sans")
)

ui <- page_fillable(
  theme = app_theme,
  fillable_mobile = TRUE,
  tags$head(
    tags$style(HTML("
      :root {
        --dashboard-accent: #0B5C74;
        --dashboard-soft: #E9F3F6;
        --dashboard-border: #D6E1E6;
        --dashboard-text: #243746;
      }
      body {
        background:
          radial-gradient(circle at top left, rgba(11, 92, 116, 0.16), transparent 32%),
          linear-gradient(180deg, #F6FBFC 0%, #F1F6F8 100%);
        color: var(--dashboard-text);
      }
      .app-shell {
        gap: 1rem;
      }
      .hero-panel {
        background: linear-gradient(135deg, rgba(11, 92, 116, 0.98), rgba(20, 116, 140, 0.94));
        border-radius: 1rem;
        padding: 1.25rem 1.5rem;
        color: #FFFFFF;
        box-shadow: 0 18px 40px rgba(11, 92, 116, 0.16);
      }
      .hero-panel p {
        margin-bottom: 0;
        max-width: 58rem;
        color: rgba(255, 255, 255, 0.9);
      }
      .control-card,
      .bslib-card {
        border: 1px solid var(--dashboard-border);
        box-shadow: 0 12px 30px rgba(36, 55, 70, 0.08);
      }
      .metric-strip {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
        gap: 0.75rem;
      }
      .metric-box {
        background: rgba(255, 255, 255, 0.14);
        border: 1px solid rgba(255, 255, 255, 0.16);
        border-radius: 0.85rem;
        padding: 0.85rem 1rem;
      }
      .metric-box .label {
        display: block;
        font-size: 0.78rem;
        letter-spacing: 0.03em;
        text-transform: uppercase;
        opacity: 0.82;
      }
      .metric-box .value {
        display: block;
        font-size: 1.35rem;
        font-weight: 700;
      }
      .trend-summary p:last-child {
        margin-bottom: 0;
      }
      .empty-state {
        min-height: 260px;
        display: grid;
        place-items: center;
        text-align: center;
        color: #597080;
        padding: 1.5rem;
        background: linear-gradient(180deg, #F9FCFD 0%, #F3F8FA 100%);
      }
      .app-footer {
        font-size: 0.92rem;
        color: #607787;
        padding: 0.5rem 0 1rem 0;
      }
    "))
  ),
  div(
    class = "app-shell",
    div(
      class = "hero-panel",
      h2("Environmental Water Quality Dashboard"),
      p("Public-facing prototype for exploring environmental monitoring stations, recent water-quality observations, and simulated trend summaries suitable for a client demonstration or rapid pilot deployment."),
      div(
        class = "metric-strip mt-3",
        uiOutput("hero_metrics")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        class = "control-card",
        width = 320,
        h4("Filters"),
        selectInput("parameter_filter", "Parameter", choices = NULL, multiple = TRUE),
        dateRangeInput("date_filter", "Date range"),
        selectInput("waterbody_filter", "Waterbody", choices = NULL, multiple = TRUE),
        selectInput("strata_filter", "Geographic strata", choices = NULL, multiple = TRUE),
        fileInput(
          "upload_records",
          "Upload new records (CSV)",
          accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")
        ),
        helpText("Upload columns: station_id, date, parameter, observed_value, unit."),
        hr(),
        uiOutput("selection_status")
      ),
      fillable = TRUE,
      gap = "1rem",
      card(
        full_screen = TRUE,
        card_header("Monitoring Station Map"),
        leafletOutput("station_map", height = 420)
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          full_screen = TRUE,
          card_header("Station Time Series"),
          plotOutput("trend_plot", height = 360)
        ),
        card(
          card_header("Trend Results"),
          uiOutput("trend_summary")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Filtered Observation Records"),
        DTOutput("records_table")
      )
    ),
    div(
      class = "app-footer",
      "Prototype note: this dashboard is designed for local demos, rapid shinyapps.io deployment, and validation as an iframe-embedded component in a public website."
    )
  )
)

server <- function(input, output, session) {
  pal <- leaflet_pal()

  records_rv <- reactiveVal(water_quality)
  selected_station_id <- reactiveVal(NULL)

  observe({
    updateSelectInput(
      session,
      "parameter_filter",
      choices = sort(unique(stations$parameter)),
      selected = sort(unique(stations$parameter))
    )
    updateSelectInput(
      session,
      "waterbody_filter",
      choices = sort(unique(stations$waterbody)),
      selected = sort(unique(stations$waterbody))
    )
    updateSelectInput(
      session,
      "strata_filter",
      choices = sort(unique(stations$strata)),
      selected = sort(unique(stations$strata))
    )
    updateDateRangeInput(
      session,
      "date_filter",
      start = min(records_rv()$date, na.rm = TRUE),
      end = max(records_rv()$date, na.rm = TRUE)
    )
  })

  observeEvent(input$upload_records, {
    req(input$upload_records$datapath)
    uploaded <- read.csv(input$upload_records$datapath, stringsAsFactors = FALSE) %>%
      tibble::as_tibble()

    required_cols <- c("station_id", "date", "parameter", "observed_value", "unit")
    if (!all(required_cols %in% names(uploaded))) {
      showNotification(
        "Uploaded CSV must include station_id, date, parameter, observed_value, and unit.",
        type = "error"
      )
      return()
    }

    uploaded_clean <- uploaded %>%
      transmute(
        record_id = if ("record_id" %in% names(uploaded)) as.character(record_id) else paste0("UPL", seq_len(n())),
        station_id = as.character(station_id),
        date = as.Date(date),
        parameter = as.character(parameter),
        observed_value = as.numeric(observed_value),
        unit = as.character(unit)
      ) %>%
      filter(!is.na(date), !is.na(observed_value))

    records_rv(
      bind_rows(records_rv(), uploaded_clean) %>%
        arrange(date, station_id)
    )
  })

  # Keep the station layer synced to the sidebar filters so the map, summaries,
  # and selectable station all move together during demos.
  filtered_stations <- reactive({
    req(input$parameter_filter, input$waterbody_filter, input$strata_filter)

    stations %>%
      filter(
        parameter %in% input$parameter_filter,
        waterbody %in% input$waterbody_filter,
        strata %in% input$strata_filter
      )
  })

  filtered_records <- reactive({
    req(input$date_filter)

    records_rv() %>%
      filter(date >= input$date_filter[1], date <= input$date_filter[2]) %>%
      inner_join(filtered_stations(), by = c("station_id", "parameter")) %>%
      arrange(desc(date))
  })

  observe({
    available_station_ids <- filtered_stations()$station_id
    if (length(available_station_ids) == 0) {
      selected_station_id(NULL)
    } else if (is.null(selected_station_id()) || !selected_station_id() %in% available_station_ids) {
      selected_station_id(available_station_ids[[1]])
    }
  })

  observeEvent(input$station_map_marker_click, {
    selected_station_id(input$station_map_marker_click$id)
  })

  selected_station_meta <- reactive({
    req(selected_station_id())
    filtered_stations() %>%
      filter(station_id == selected_station_id())
  })

  selected_station_series <- reactive({
    req(selected_station_id(), input$date_filter)

    records_rv() %>%
      filter(
        station_id == selected_station_id(),
        date >= input$date_filter[1],
        date <= input$date_filter[2]
      ) %>%
      inner_join(
        stations %>% select(station_id, station_name, waterbody, strata, parameter, trend_classification),
        by = c("station_id", "parameter")
      ) %>%
      arrange(date)
  })

  modeled_series <- reactive({
    trend_model_data(selected_station_series())
  })

  output$hero_metrics <- renderUI({
    current_records <- filtered_records()
    current_stations <- filtered_stations()

    tagList(
      div(class = "metric-box", span(class = "label", "Visible stations"), span(class = "value", nrow(current_stations))),
      div(class = "metric-box", span(class = "label", "Visible observations"), span(class = "value", scales::comma(nrow(current_records)))),
      div(class = "metric-box", span(class = "label", "Waterbodies"), span(class = "value", dplyr::n_distinct(current_stations$waterbody))),
      div(class = "metric-box", span(class = "label", "Trend flags"), span(class = "value", paste(sort(unique(as.character(current_stations$trend_classification))), collapse = " / ")))
    )
  })

  output$selection_status <- renderUI({
    if (is.null(selected_station_id())) {
      return(tags$p("No station is currently available for the active filters."))
    }

    station_row <- selected_station_meta()
    tags$div(
      tags$strong(station_row$station_name[[1]]),
      tags$div(paste("Station ID:", station_row$station_id[[1]])),
      tags$div(paste("Waterbody:", station_row$waterbody[[1]])),
      tags$div(paste("Trend classification:", station_row$trend_classification[[1]]))
    )
  })

  output$station_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addScaleBar(position = "bottomleft")
  })

  observe({
    station_rows <- filtered_stations()

    leafletProxy("station_map", data = station_rows) %>%
      clearMarkers() %>%
      clearControls() %>%
      {
        if (nrow(station_rows) == 0) {
          .
        } else {
          addCircleMarkers(
            .,
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~station_id,
            radius = 8,
            stroke = TRUE,
            weight = 1.5,
            color = "#FFFFFF",
            fillOpacity = 0.92,
            fillColor = ~pal(trend_classification),
            label = ~paste0(station_name, " (", trend_classification, ")"),
            popup = ~paste0(
              "<strong>", station_name, "</strong><br/>",
              "Station ID: ", station_id, "<br/>",
              "Waterbody: ", waterbody, "<br/>",
              "Strata: ", strata, "<br/>",
              "Parameter: ", parameter, "<br/>",
              "Trend: ", trend_classification
            )
          ) %>%
            fitBounds(
              lng1 = min(station_rows$longitude) - 0.3,
              lat1 = min(station_rows$latitude) - 0.3,
              lng2 = max(station_rows$longitude) + 0.3,
              lat2 = max(station_rows$latitude) + 0.3
            ) %>%
            addLegend(
              "bottomright",
              pal = pal,
              values = ~trend_classification,
              title = "Trend Classification",
              opacity = 0.95
            )
        }
      }
  })

  output$trend_plot <- renderPlot({
    station_series <- selected_station_series()

    validate(
      need(!is.null(selected_station_id()), "Select a station from the map to view the time-series chart."),
      need(nrow(station_series) > 0, "No observations are available for the selected station within the current date range.")
    )

    modeled <- modeled_series()
    validate(need(!is.null(modeled), "At least two observations are needed to estimate the prototype trend line."))

    ggplot(modeled, aes(x = date, y = observed_value)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#8CC6D7", alpha = 0.28) +
      geom_line(aes(y = trend_value), color = "#0B5C74", linewidth = 1.2) +
      geom_point(color = "#1B4B5A", size = 2.6) +
      geom_line(color = "#5F7D8A", linewidth = 0.7, alpha = 0.7) +
      labs(
        title = paste0(station_series$station_name[[1]], " - ", station_series$parameter[[1]]),
        subtitle = "Observed values with a simulated trend line and confidence interval",
        x = NULL,
        y = paste0("Observed value (", station_series$unit[[1]], ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = "#243746"),
        plot.subtitle = element_text(color = "#557080"),
        panel.grid.minor = element_blank()
      )
  })

  output$trend_summary <- renderUI({
    if (is.null(selected_station_id())) {
      return(div(class = "empty-state", "Select a station marker to review the prototype trend summary and supporting narrative."))
    }

    station_meta <- selected_station_meta()
    station_series <- modeled_series()

    if (is.null(station_series) || nrow(station_series) == 0) {
      return(div(class = "empty-state", "No dated observations are available for the selected station under the current filters."))
    }

    summary_text <- format_trend_summary(station_series, station_meta)

    div(
      class = "trend-summary",
      tags$p(tags$strong(summary_text$significance_label)),
      tags$p(summary_text$gam_summary),
      tags$p(summary_text$slope_summary)
    )
  })

  output$records_table <- renderDT({
    datatable(
      filtered_records() %>%
        select(date, station_id, station_name, waterbody, strata, parameter, observed_value, unit, trend_classification),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 8, autoWidth = TRUE, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
