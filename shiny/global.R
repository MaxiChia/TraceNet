# =============================================================================
# global.R — TraceNet Shiny App
# =============================================================================

# -----------------------------------------------------------------------------
# 1. PACKAGES — trimmed to only what modules actually use
# -----------------------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)
library(ggplot2)
library(plotly)
library(visNetwork)
library(DT)
library(jsonlite)


# -----------------------------------------------------------------------------
# 2. DATA LOADING
# -----------------------------------------------------------------------------
data_dir <- file.path(getwd(), "data")

mc2_df  <- readRDS(file.path(data_dir, "mc2_df.rds"))
org_raw <- jsonlite::fromJSON(
  file.path(data_dir, "org_chart.json"),
  simplifyVector = FALSE
)


# -----------------------------------------------------------------------------
# 3. PARSE org_chart.json
# -----------------------------------------------------------------------------
org_nodes <- org_raw$nodes |>
  purrr::map_dfr(~ tibble::tibble(
    id    = .x[["id"]],
    label = .x[["label"]] %||% .x[["id"]],
    type  = .x[["type"]]  %||% NA_character_
  ))

org_edges <- org_raw$edges |>
  purrr::map_dfr(~ tibble::tibble(
    from   = .x[["source"]] %||% .x[["from"]],
    to     = .x[["target"]] %||% .x[["to"]],
    weight = .x[["weight"]] %||% 1L
  ))


# -----------------------------------------------------------------------------
# 4. mc2_df NORMALISATION
# Pre-processed locally — RDS already contains all derived columns.
# No transformation needed on startup.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 5. CONFIRMED MC2 CONSTANTS
# -----------------------------------------------------------------------------
INCIDENT_DATES    <- as.Date(c("2046-05-10", "2046-05-11", "2046-05-17"))
TERMINAL_EXECUTOR <- "john_windward"
INJECTED_FILES    <- c("HiddenOrca.txt", "MellowOtter.txt", "SwiftWren.txt")
RELAY_CHAIN_ROOT  <- "james_stern"

TERMINAL_SEQUENCE <- c(
  "saidit_post_check",
  "saidit_post",
  "delete_file",
  "delete_file"
)


# -----------------------------------------------------------------------------
# 6. SHARED DERIVED OBJECTS
# -----------------------------------------------------------------------------
saidit_posts <- mc2_df |>
  dplyr::filter(short_name == "saidit_post")

anomalous_events <- saidit_posts |>
  dplyr::filter(!is.na(content_source))

normal_posts <- saidit_posts |>
  dplyr::filter(is.na(content_source))

incident_events <- mc2_df |>
  dplyr::filter(date %in% INCIDENT_DATES)

jw_events <- mc2_df |>
  dplyr::filter(
    target_agent == TERMINAL_EXECUTOR |
      stringr::str_detect(as.character(parties), TERMINAL_EXECUTOR)
  )

terminal_seq_events <- incident_events |>
  dplyr::filter(short_name %in% TERMINAL_SEQUENCE) |>
  dplyr::arrange(datetime)

actor_activity <- mc2_df |>
  dplyr::group_by(date, target_agent) |>
  dplyr::summarise(n_events = dplyr::n(), .groups = "drop") |>
  dplyr::filter(!is.na(target_agent))

cutoff_date       <- max(INCIDENT_DATES)
pre_intervention  <- mc2_df |> dplyr::filter(date <= cutoff_date)
post_intervention <- mc2_df |> dplyr::filter(date >  cutoff_date)


# -----------------------------------------------------------------------------
# 7. UI COLOUR CONSTANTS
# -----------------------------------------------------------------------------
ANOMALY_COLOUR <- "#E15759"
NORMAL_COLOUR  <- "#76B7B2"

MODULE_COLOURS <- c(
  "Module1" = "#4E79A7",
  "Module2" = "#F28E2B",
  "Module3" = "#59A14F"
)


# -----------------------------------------------------------------------------
# 8. SOURCE MODULE FILES
# -----------------------------------------------------------------------------
source("modules/mod1_topology.R")
source("modules/mod2_anomaly.R")
source("modules/mod3_historical.R")

cat("global.R loaded successfully.\n")
cat("mc2_df rows       :", nrow(mc2_df), "\n")
cat("anomalous_events  :", nrow(anomalous_events), "\n")
cat("incident_events   :", nrow(incident_events), "\n")
