# R Shiny Water Quality Dashboard Demo

This repository contains a prototype of a public-facing environmental water quality dashboard built in R Shiny. It is intended for demonstration and early deployment planning, not as a productionized application. The app showcases interactive station mapping, parameter/date/geography filtering, time-series visualization, trend summary display, tabular exploration, and sample upload/refresh behavior.

## Quick Start

```r
install.packages(c(
  "shiny",
  "bslib",
  "leaflet",
  "ggplot2",
  "dplyr",
  "tidyr",
  "DT",
  "htmltools",
  "tibble",
  "scales"
))

shiny::runApp()
```

## Features

- Interactive `leaflet` map of monitoring stations
- Trend classification symbology for improving, stable, and degrading stations
- Sidebar filters for parameter, date range, waterbody, and strata
- Time-series chart with a simulated trend line and confidence interval
- Trend summary and results panel
- Filtered summary table
- CSV upload workflow for appending new records in memory
- Responsive, public-dashboard-style layout built with `bslib`
- Prototype footer noting iframe suitability

## Project Structure

```text
.
|-- app.R
|-- R/
|   `-- helpers.R
|-- data/
|   |-- stations.csv
|   `-- water_quality.csv
|-- README.md
`-- rsconnect-note.md
```

## Local Setup

1. Install [R](https://cran.r-project.org/).
2. Install RStudio Desktop or use VS Code with R support.
3. Open this project directory.
4. Install the required packages shown in Quick Start.
5. Run `app.R` or launch the app from the R console.

## Run Locally

From RStudio, open `app.R` and click `Run App`.

From the R console in the repository root:

```r
shiny::runApp()
```

From another working directory:

```r
shiny::runApp("path/to/repo")
```

## Data

The repository includes synthetic sample data in `data/stations.csv` and `data/water_quality.csv` so the dashboard works immediately for demos. In a real engagement, this prototype would typically be connected to client-provided environmental monitoring data, station metadata, and analysis-ready trend outputs.

## Deployment

This demo can be deployed quickly to shinyapps.io for review. Depending on project requirements, production-oriented hosting could also be evaluated with Shiny Server, Posit Connect, or another approved hosting environment. Deployment and embedding constraints should be validated early so the prototype remains aligned with the intended delivery context.

## Deploy to shinyapps.io

```r
install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT_NAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

rsconnect::deployApp(appDir = ".")
```

Additional repository-specific notes can be recorded in `rsconnect-note.md`.

## Demo

[Environmental Water Quality Dashboard](https://nashid-analytics.shinyapps.io/water-quality-demo/)

## iFrame Embedding Notes

This prototype is designed with embedding in mind, but final iframe behavior should be validated early against the target website. Cross-domain behavior, response headers, content security policy, authentication patterns, and host-page styling can affect whether the embedded app works as intended.

## Maintainability

- Helper logic is separated into `R/helpers.R` to keep `app.R` readable
- Setup and deployment steps are documented for reproducibility
- The repository is easy to hand off in a public GitHub workflow
- The structure supports iterative refinement from prototype to a more production-oriented implementation

## Prototype Limitations

- The included dataset is synthetic and intended for demonstration only
- Trend and model outputs are lightweight demo representations
- Authentication and authorization are not included
- Production security review and final embedding validation are out of scope for this prototype
