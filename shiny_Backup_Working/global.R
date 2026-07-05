# =============================================================================
# global.R — TraceNet Shiny App
# Flexible loader version: supports the normal project structure
#   data/mc2_df.rds, data/org_chart.json, modules/*.R
# and the flat uploaded structure used during review.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. PACKAGES
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

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# 2. FLEXIBLE PATH HELPERS
# -----------------------------------------------------------------------------
resolve_file <- function(...) {
  candidates <- c(...)
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop(
      "Cannot find required file. Checked: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  hit
}

source_if_exists <- function(...) {
  candidates <- c(...)
  hit <- candidates[file.exists(candidates)][1]
  if (!is.na(hit)) {
    source(hit, local = FALSE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# -----------------------------------------------------------------------------
# 3. DATA LOADING
# -----------------------------------------------------------------------------
data_dir <- file.path(getwd(), "data")
module_dir <- file.path(getwd(), "modules")

mc2_df <- readRDS(resolve_file(
  file.path(data_dir, "mc2_df.rds"),
  file.path(getwd(), "mc2_df.rds")
))

org_raw <- jsonlite::fromJSON(
  resolve_file(
    file.path(data_dir, "org_chart.json"),
    file.path(getwd(), "org_chart.json")
  ),
  simplifyVector = FALSE
)

# -----------------------------------------------------------------------------
# 4. PARSE org_chart.json
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
ANOMALY_COLOUR  <- "#D84040"
NORMAL_COLOUR   <- "#5DCAA5"
CHAIN_COLOUR    <- "#1D9E75"
ROOT_COLOUR     <- "#E8A52B"
REUSED_COLOUR   <- "#E8C547"
TERMINAL_COLOUR <- "#D84040"
DELETED_COLOUR  <- "#3d5a63"
GATE_COLOUR     <- "#E8C547"

MODULE_COLOURS <- c(
  "Module1" = "#5DCAA5",
  "Module2" = "#E8C547",
  "Module3" = "#1D9E75"
)

# Shared plot background values for module plots.
TN_PLOT_BG     <- "#0B1418"
TN_PANEL_BG    <- "#0F1D22"
TN_GRID        <- "#243D47"
TN_TEXT        <- "#E8EAEA"
TN_MUTED_TEXT  <- "#AAB7BA"

# -----------------------------------------------------------------------------
# 8. SOURCE MODULE FILES
# -----------------------------------------------------------------------------
source_if_exists(file.path(module_dir, "mod1_topology.R"), file.path(getwd(), "mod1_topology.R"))
source_if_exists(file.path(module_dir, "mod2_anomaly.R"), file.path(getwd(), "mod2_anomaly.R"))
source_if_exists(file.path(module_dir, "mod3_historical.R"), file.path(getwd(), "mod3_historical.R"))

# Optional investigation walkthrough module. The upgraded UI does not depend on
# it, but existing deployments can keep using it if the file is present.
source_if_exists(file.path(module_dir, "mod_investigation.R"), file.path(getwd(), "mod_investigation.R"))

cat("global.R loaded successfully.\n")
cat("mc2_df rows       :", nrow(mc2_df), "\n")
cat("anomalous_events  :", nrow(anomalous_events), "\n")
cat("incident_events   :", nrow(incident_events), "\n")
