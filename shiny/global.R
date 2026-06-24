# =============================================================================
# global.R — TraceNet Shiny App
# Column names confirmed from mc2_df.rds inspection:
#   when              = Unix epoch seconds (numeric)
#   parties           = character string e.g. "person:isaac_mast, person:levi_signal"
#   short_name        = event type (NOT type)
#   details.content_source = anomaly indicator column
#   details.target_agent   = destination agent
# =============================================================================


# -----------------------------------------------------------------------------
# 1. PACKAGES
# -----------------------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  shiny, bslib,
  tidyverse, lubridate, jsonlite, purrr,
  tidygraph, ggraph, igraph,
  visNetwork,
  DT,
  plotly, ggiraph,
  patchwork,        # ← add this
  RColorBrewer, viridis
)


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
# 3. PARSE org_chart.json → flat tibbles
# Uses $edges (confirmed — NOT $links).
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

org_graph <- tidygraph::tbl_graph(
  nodes    = org_nodes,
  edges    = org_edges,
  directed = TRUE
)


# -----------------------------------------------------------------------------
# 4. mc2_df NORMALISATION
# -----------------------------------------------------------------------------
mc2_df <- mc2_df |>
  tidyr::unnest_wider(details, names_sep = "_") |>
  dplyr::rename(
    content_source = details_content_source,
    target_agent   = details_target_agent,
    event_file     = details_file,
    from_agent     = details_from,
    to_agent       = details_to
  ) |>
  dplyr::mutate(
    datetime = lubridate::as_datetime(when),
    date     = as.Date(datetime)
  )


# -----------------------------------------------------------------------------
# 5. CONFIRMED MC2 CONSTANTS — do NOT re-derive in module code
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

# 6a. All saidit_post events
saidit_posts <- mc2_df |>
  dplyr::filter(short_name == "saidit_post")

# 6b. Anomalous: content_source IS NOT NULL (fires on exactly 3 incidents)
anomalous_events <- saidit_posts |>
  dplyr::filter(!is.na(content_source))

# 6c. Normal baseline saidit_post events
normal_posts <- saidit_posts |>
  dplyr::filter(is.na(content_source))

# 6d. All events on the three confirmed incident dates
incident_events <- mc2_df |>
  dplyr::filter(date %in% INCIDENT_DATES)

# 6e. Events involving terminal executor john_windward
jw_events <- mc2_df |>
  dplyr::filter(
    target_agent == TERMINAL_EXECUTOR |
      stringr::str_detect(as.character(parties), TERMINAL_EXECUTOR)
  )

# 6f. Four-event terminal sequences across all three incidents
terminal_seq_events <- incident_events |>
  dplyr::filter(short_name %in% TERMINAL_SEQUENCE) |>
  dplyr::arrange(datetime)

# 6g. Actor activity summary — Module 3 heatmap
actor_activity <- mc2_df |>
  dplyr::group_by(date, target_agent) |>
  dplyr::summarise(n_events = dplyr::n(), .groups = "drop") |>
  dplyr::filter(!is.na(target_agent))

# 6h. Pre / post-intervention split at last confirmed incident
cutoff_date       <- max(INCIDENT_DATES)
pre_intervention  <- mc2_df |> dplyr::filter(date <= cutoff_date)
post_intervention <- mc2_df |> dplyr::filter(date >  cutoff_date)


# -----------------------------------------------------------------------------
# 7. UI COLOUR CONSTANTS
# -----------------------------------------------------------------------------
ANOMALY_COLOUR  <- "#D84040"   # deep red — anomalous events
NORMAL_COLOUR   <- "#5DCAA5"   # teal — normal events
CHAIN_COLOUR    <- "#1D9E75"   # dark teal — relay nodes
ROOT_COLOUR     <- "#E8A52B"   # amber — chain root
REUSED_COLOUR   <- "#E8C547"   # gold — reused/obfuscation nodes
TERMINAL_COLOUR <- "#D84040"   # red — terminal executor
DELETED_COLOUR  <- "#3d5a63"   # slate — delete_file events
GATE_COLOUR     <- "#E8C547"   # gold — saidit_post_check

MODULE_COLOURS <- c(
  "Module1" = "#5DCAA5",
  "Module2" = "#E8C547",
  "Module3" = "#1D9E75"
)


# -----------------------------------------------------------------------------
# 8. SOURCE MODULE FILES
# -----------------------------------------------------------------------------
source("modules/mod1_topology.R")
source("modules/mod2_anomaly.R")
source("modules/mod3_historical.R")

cat("global.R loaded successfully.\n")
cat("mc2_df rows       :", nrow(mc2_df), "\n")
cat("saidit_posts      :", nrow(saidit_posts), "\n")
cat("anomalous_events  :", nrow(anomalous_events), "\n")
cat("incident_events   :", nrow(incident_events), "\n")