# =============================================================================
# precompute_app_data.R
# Precompute lightweight analytics objects for faster Shiny rendering
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(jsonlite)
library(igraph)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------
app_dir <- getwd()
data_dir <- file.path(app_dir, "data")
cache_dir <- file.path(data_dir, "app_cache")

if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# 2. Load source data
# -----------------------------------------------------------------------------
mc2_df <- readRDS(file.path(data_dir, "mc2_df.rds"))

# -----------------------------------------------------------------------------
# 3. Constants
# -----------------------------------------------------------------------------
INCIDENT_DATES <- as.Date(c("2046-05-10", "2046-05-11", "2046-05-17"))

TERMINAL_EXECUTOR <- "john_windward"

CORE_ACTOR_IDS <- c(
  "Agent/person:victoria_rigging",
  "Agent/person:mia_fender",
  "Agent/person:lily_anchorline",
  "Agent/person:john_windward",
  "Agent/person:daniel_gangway",
  "Agent/person:chloe_ballast"
)

clean_actor <- function(x) {
  x |>
    stringr::str_remove("^(Agent/person:|person:)") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
}

# -----------------------------------------------------------------------------
# 4. Precompute saidit_post anomaly data
# -----------------------------------------------------------------------------
saidit_posts_cache <- mc2_df |>
  filter(short_name == "saidit_post") |>
  mutate(
    status = ifelse(is.na(content_source), "Normal", "Anomalous"),
    time_of_day = as.numeric(format(datetime, "%H")) +
      as.numeric(format(datetime, "%M")) / 60,
    date_label = format(date, "%d %b %Y"),
    actor = purrr::map_chr(parties, ~ {
      if (length(.x) == 0) return(NA_character_)
      clean_actor(.x[1])
    })
  ) |>
  select(
    any_of(c(
      "datetime", "date", "date_label", "time_of_day", "actor",
      "short_name", "content_source", "file_name", "filename", "file",
      "status", "target_agent", "parties"
    )),
    everything()
  )

saveRDS(
  saidit_posts_cache,
  file.path(cache_dir, "mod2_saidit_posts.rds")
)

# -----------------------------------------------------------------------------
# 5. Precompute core network edges
# -----------------------------------------------------------------------------
core_events <- mc2_df |>
  filter(
    purrr::map_lgl(parties, ~ any(.x %in% CORE_ACTOR_IDS)),
    purrr::map_int(parties, length) >= 2
  ) |>
  mutate(
    from = purrr::map_chr(parties, ~ .x[1]),
    to = purrr::map_chr(parties, ~ .x[2]),
    from_clean = clean_actor(from),
    to_clean = clean_actor(to)
  )

core_edges <- core_events |>
  group_by(from, to, from_clean, to_clean) |>
  summarise(
    weight = n(),
    event_types = paste(sort(unique(short_name)), collapse = ", "),
    .groups = "drop"
  )

saveRDS(
  core_edges,
  file.path(cache_dir, "mod1_core_edges.rds")
)

# -----------------------------------------------------------------------------
# 6. Precompute network centrality metrics
# -----------------------------------------------------------------------------
edge_for_graph <- core_edges |>
  select(from_clean, to_clean, weight)

g <- igraph::graph_from_data_frame(
  d = edge_for_graph,
  directed = TRUE
)

network_metrics <- tibble(
  actor = igraph::V(g)$name,
  degree = igraph::degree(g, mode = "all"),
  in_degree = igraph::degree(g, mode = "in"),
  out_degree = igraph::degree(g, mode = "out"),
  betweenness = igraph::betweenness(g, directed = TRUE, normalized = TRUE),
  closeness = igraph::closeness(g, mode = "all", normalized = TRUE),
  eigenvector = igraph::eigen_centrality(g, directed = TRUE)$vector
) |>
  mutate(
    role = case_when(
      str_to_lower(str_replace_all(actor, " ", "_")) == "john_windward" ~ "Terminal executor",
      actor %in% clean_actor(CORE_ACTOR_IDS) ~ "Core actor",
      TRUE ~ "Peripheral"
    )
  ) |>
  arrange(desc(degree))

saveRDS(
  network_metrics,
  file.path(cache_dir, "network_metrics.rds")
)

# -----------------------------------------------------------------------------
# 7. Precompute actor recurrence data for Module 3
# -----------------------------------------------------------------------------
get_chain_actors_per_date <- function(target_date, cutoff_time) {
  mc2_df |>
    filter(
      date == as.Date(target_date),
      short_name == "queue_subordinate_task",
      details_task == "read_file",
      datetime <= as.POSIXct(cutoff_time, tz = "UTC")
    ) |>
    mutate(
      issuer = purrr::map_chr(parties, ~ stringr::str_remove(.x[[1]], "Agent/person:")),
      receiver = stringr::str_remove(target_agent, "Agent/person:")
    ) |>
    select(issuer, receiver) |>
    pivot_longer(everything(), values_to = "actor") |>
    distinct(actor) |>
    filter(!is.na(actor), actor != "") |>
    mutate(date = as.Date(target_date))
}

chain_may10 <- get_chain_actors_per_date("2046-05-10", "2046-05-10 12:45:40")
chain_may11 <- get_chain_actors_per_date("2046-05-11", "2046-05-11 00:56:03")
chain_may17 <- get_chain_actors_per_date("2046-05-17", "2046-05-17 11:21:13")

recurrence_data <- bind_rows(chain_may10, chain_may11, chain_may17) |>
  mutate(
    present = 1,
    actor_label = clean_actor(actor),
    date_label = case_when(
      date == as.Date("2046-05-10") ~ "10 May\n(HiddenOrca.txt)",
      date == as.Date("2046-05-11") ~ "11 May\n(MellowOtter.txt)",
      date == as.Date("2046-05-17") ~ "17 May\n(SwiftWren.txt)"
    )
  )

saveRDS(
  recurrence_data,
  file.path(cache_dir, "mod3_recurrence_data.rds")
)

cat("Precompute completed successfully.\n")
cat("Cache saved to:", cache_dir, "\n")
cat("saidit_posts:", nrow(saidit_posts_cache), "\n")
cat("core_edges:", nrow(core_edges), "\n")
cat("network_metrics:", nrow(network_metrics), "\n")
cat("recurrence_data:", nrow(recurrence_data), "\n")