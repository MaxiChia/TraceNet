# =============================================================================
# modules/mod1_topology.R — VISUAL EXCELLENCE EDITION
# =============================================================================

RELAY_NODES <- tibble::tibble(
  id    = c("james_stern", "gabriel_sonar", "daniel_gangway",
            "zoey_drydock", "mia_fender", "victoria_rigging",
            "lily_anchorline", "chloe_ballast", "john_windward"),
  label = c("James Stern\n(Root)",
            "Gabriel Sonar",
            "Daniel Gangway\n(reused ×2)",
            "Zoey Drydock\n(reused ×2)",
            "Mia Fender",
            "Victoria Rigging",
            "Lily Anchorline",
            "Chloe Ballast",
            "John Windward\n(Terminal)"),
  color.background = c(
    "#E8A52B", "#1D9E75", "#E8C547", "#E8C547",
    "#1D9E75", "#1D9E75", "#1D9E75", "#1D9E75", "#D84040"
  ),
  color.border               = "white",
  color.highlight.background = "#FFD700",
  value    = c(35, 20, 28, 28, 20, 20, 20, 20, 45),
  font.size = c(13, 11, 13, 13, 11, 11, 11, 11, 14),
  font.color = "#E0C060",
  shadow = TRUE,
  x = c(500, 300, 100, -100, -300, -300, -100, 100, 300),
  y = c(0,   0,   -100, 0,   -100, 100,  100,  200, 300),
  title = c(
    "<b>James Stern — Chain Root</b><br>First delegation: 00:51:43 UTC<br>Initiates the relay on all 3 incident dates",
    "<b>Gabriel Sonar</b><br>Received: 00:51 | Delegated: 02:26<br>Delay: 1h 35min",
    "<b>Daniel Gangway</b><br><span style='color:#E8A52B'>Reused node — step 2 and step 8</span><br>Deliberate obfuscation tactic",
    "<b>Zoey Drydock</b><br><span style='color:#E8A52B'>Reused node — step 3 and step 6</span><br>Deliberate obfuscation tactic",
    "<b>Mia Fender</b><br>Received: 05:34 | Delegated: 05:52",
    "<b>Victoria Rigging</b><br>Received: 05:52 | Delegated: 06:44",
    "<b>Lily Anchorline</b><br>Received: 08:16 | Delegated: 10:11",
    "<b>Chloe Ballast</b><br>Received: 11:12 | Delegated: 11:21",
    "<b style='color:#D84040'>John Windward — TERMINAL EXECUTOR</b><br>Received task: 11:21:13 UTC<br>Anomalous post: 11:21:15 UTC (+2s)<br>Evidence deleted: 11:21:16–17 UTC<br>Injected file: SwiftWren.txt"
  )
)

RELAY_EDGES <- tibble::tibble(
  from  = c("james_stern", "gabriel_sonar", "daniel_gangway",
            "zoey_drydock", "mia_fender", "victoria_rigging",
            "zoey_drydock", "lily_anchorline", "daniel_gangway",
            "chloe_ballast"),
  to    = c("gabriel_sonar", "daniel_gangway", "zoey_drydock",
            "mia_fender", "victoria_rigging", "zoey_drydock",
            "lily_anchorline", "daniel_gangway", "chloe_ballast",
            "john_windward"),
  label       = c("00:51", "02:26", "04:12",
                  "05:34", "05:52", "06:44",
                  "08:16", "10:11", "11:12",
                  "11:21 ⚡"),
  is_terminal = c(rep(FALSE, 9), TRUE),
  color       = c(rep("#3d5a63", 9), "#D84040"),
  width       = c(rep(2, 9), 6),
  arrows      = "to",
  font.size   = c(rep(10, 9), 13),
  font.color  = c(rep("#5DCAA5", 9), "#D84040"),
  font.bold   = c(rep(FALSE, 9), TRUE),
  smooth      = TRUE,
  dashes      = FALSE
)

CORE_ACTOR_IDS <- c(
  "Agent/person:victoria_rigging",
  "Agent/person:mia_fender",
  "Agent/person:lily_anchorline",
  "Agent/person:john_windward",
  "Agent/person:daniel_gangway",
  "Agent/person:chloe_ballast"
)

clean_name <- function(x) stringr::str_remove(x, "^(person:|Agent/person:)")

mod1_render_network <- function(data, network_mode = "relay",
                                show_labels = TRUE, selected_date = "All") {
  if (network_mode == "relay") {
    mod1_network_relay(show_labels)
  } else {
    mod1_network_core(data, show_labels, selected_date)
  }
}

# -----------------------------------------------------------------------------
# RELAY MODE
# -----------------------------------------------------------------------------
mod1_network_relay <- function(show_labels) {
  nodes <- RELAY_NODES
  if (!show_labels) nodes$label <- ""
  
  visNetwork::visNetwork(
    nodes = nodes,
    edges = RELAY_EDGES,
    width  = "100%",
    height = "450px",
    main   = list(
      text  = "11-Node Task Delegation Chain — 17 May 2046 (same structure confirmed across all 3 incidents)",
      style = "font-size:13px; font-weight:bold; color:#E0C060; text-align:left; padding-left:10px;"
    )
  ) |>
    visNetwork::visEdges(
      smooth = list(type = "curvedCW", roundness = 0.3),
      font   = list(align = "top", strokeWidth = 0)
    ) |>
    visNetwork::visNodes(
      font    = list(color = "#E0C060", strokeWidth = 2, strokeColor = "rgba(0,0,0,0.3)"),
      shadow  = list(enabled = TRUE, size = 8)
    ) |>
    visNetwork::visPhysics(enabled = FALSE) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = FALSE)
    ) |>
    visNetwork::visLegend(
      addNodes = data.frame(
        label            = c("Chain Root", "Reused (obfuscation)",
                             "Relay Node", "Terminal Executor"),
        color.background = c("#E8A52B", "#E8C547", "#1D9E75", "#D84040"),
        font.color       = "#E0C060",
        shape = "dot", size = 20, stringsAsFactors = FALSE
      ),
      useGroups = FALSE, position = "right", width = 0.18
    ) |>
    visNetwork::visInteraction(
      navigationButtons = TRUE, tooltipDelay = 80,
      dragNodes = TRUE, zoomView = TRUE
    )
}

# -----------------------------------------------------------------------------
# CORE MODE
# -----------------------------------------------------------------------------
mod1_network_core <- function(data, show_labels, selected_date = "All") {
  core_events <- data |>
    dplyr::filter(
      purrr::map_lgl(parties, ~ any(.x %in% CORE_ACTOR_IDS)),
      purrr::map_int(parties, length) >= 2
    ) |>
    dplyr::mutate(
      from_p = purrr::map_chr(parties, ~ .x[1]),
      to_p   = purrr::map_chr(parties, ~ .x[2])
    )
  
  if (nrow(core_events) == 0) {
    return(visNetwork::visNetwork(
      data.frame(id = "none",
                 label = "No core actor events for selected date",
                 color.background = "#1a2e35"),
      data.frame(from = character(0), to = character(0))
    ))
  }
  
  edge_df <- core_events |>
    dplyr::group_by(from_p, to_p) |>
    dplyr::summarise(
      weight      = dplyr::n(),
      event_types = paste(unique(short_name), collapse = ", "),
      .groups     = "drop"
    ) |>
    dplyr::rename(from = from_p, to = to_p) |>
    dplyr::mutate(
      width  = pmin(1 + log1p(weight) * 1.5, 10),
      title  = paste0("<b>Interactions: ", weight, "</b><br>Types: ", event_types),
      arrows = "to",
      color  = ifelse(
        from %in% CORE_ACTOR_IDS & to %in% CORE_ACTOR_IDS,
        ANOMALY_COLOUR, "#3d5a63"
      )
    )
  
  all_ids <- unique(c(edge_df$from, edge_df$to))
  node_df <- tibble::tibble(id = all_ids) |>
    dplyr::mutate(
      is_core     = id %in% CORE_ACTOR_IDS,
      is_terminal = id == "Agent/person:john_windward",
      label       = ifelse(show_labels, clean_name(id), ""),
      color.background = dplyr::case_when(
        is_terminal ~ ANOMALY_COLOUR,
        is_core     ~ "#1D9E75",
        TRUE        ~ "#3d5a63"
      ),
      color.border               = "white",
      color.highlight.background = "#FFD700",
      value     = ifelse(is_core, 28, 12),
      font.size = ifelse(is_core, 13, 10),
      font.color = "#E0C060",
      shadow    = TRUE,
      title     = paste0(
        "<b>", clean_name(id), "</b><br>",
        ifelse(is_terminal, "⚡ Terminal Executor",
               ifelse(is_core, "Core Relay Actor", "Peripheral Agent"))
      )
    )
  
  visNetwork::visNetwork(
    nodes = node_df, edges = edge_df,
    width = "100%", height = "450px",
    main  = list(
      text  = paste0("Core Actor Interaction Network — ", selected_date),
      style = "font-size:13px; font-weight:bold; color:#E0C060; text-align:left; padding-left:10px;"
    )
  ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = FALSE)
    ) |>
    visNetwork::visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01),
      stabilization    = list(iterations = 300)
    ) |>
    visNetwork::visNodes(
      font   = list(color = "#E0C060"),
      shadow = list(enabled = TRUE, size = 6)
    ) |>
    visNetwork::visLegend(
      addNodes = data.frame(
        label            = c("Terminal Executor", "Core Actor", "Peripheral"),
        color.background = c(ANOMALY_COLOUR, "#1D9E75", "#3d5a63"),
        font.color       = "#E0C060",
        shape = "dot", size = 20, stringsAsFactors = FALSE
      ),
      useGroups = FALSE, position = "right"
    ) |>
    visNetwork::visInteraction(navigationButtons = TRUE, tooltipDelay = 80)
}

# -----------------------------------------------------------------------------
# TIMELINE — Swimlane Gantt style
# Each chain actor = one horizontal lane
# Events shown as coloured segments/points with clear anomaly callout
# -----------------------------------------------------------------------------
mod1_render_timeline <- function(data, selected_date = "All") {
  
  chain_actors_clean <- c("james_stern", "gabriel_sonar", "daniel_gangway",
                          "zoey_drydock", "mia_fender", "victoria_rigging",
                          "lily_anchorline", "chloe_ballast", "john_windward")
  
  actor_labels <- c(
    "james_stern"      = "James Stern (Root)",
    "gabriel_sonar"    = "Gabriel Sonar",
    "daniel_gangway"   = "Daniel Gangway ×2",
    "zoey_drydock"     = "Zoey Drydock ×2",
    "mia_fender"       = "Mia Fender",
    "victoria_rigging" = "Victoria Rigging",
    "lily_anchorline"  = "Lily Anchorline",
    "chloe_ballast"    = "Chloe Ballast",
    "john_windward"    = "John Windward (Terminal)"
  )
  
  timeline_df <- data |>
    dplyr::mutate(
      actor_clean = purrr::map_chr(parties, ~ {
        hits <- stringr::str_remove(.x, "Agent/person:")
        hits <- hits[hits %in% chain_actors_clean]
        if (length(hits) > 0) hits[1] else NA_character_
      }),
      date_label   = format(date, "%d %b %Y"),
      is_anomalous = short_name == "saidit_post" & !is.na(content_source)
    ) |>
    dplyr::filter(
      !is.na(actor_clean),
      short_name %in% c("queue_subordinate_task", "saidit_post_check",
                        "saidit_post", "delete_file", "read_file")
    ) |>
    dplyr::mutate(
      actor_label  = dplyr::recode(actor_clean, !!!actor_labels),
      actor_factor = factor(actor_label, levels = rev(unname(actor_labels))),
      event_label  = dplyr::case_when(
        is_anomalous                           ~ "Anomalous Post (INJECTION)",
        short_name == "queue_subordinate_task" ~ "Task Delegation",
        short_name == "saidit_post_check"      ~ "Gate Check",
        short_name == "delete_file"            ~ "Evidence Deleted",
        short_name == "read_file"              ~ "File Read",
        TRUE                                   ~ short_name
      ),
      tip = paste0(
        "<b>", actor_label, "</b><br>",
        "Event: <b>", short_name, "</b><br>",
        "Time: ", format(datetime, "%H:%M:%S UTC"), "<br>",
        "Date: ", date_label,
        ifelse(is_anomalous,
               paste0("<br><br><b>\u26a1 INJECTED: ", content_source, "</b>"), "")
      )
    )
  
  if (nrow(timeline_df) == 0) {
    return(plotly::plot_ly() |>
             plotly::layout(title = "No chain actor events for selected date range."))
  }
  
  colour_map <- c(
    "Task Delegation"            = "#1D9E75",
    "Gate Check"                 = "#E8A52B",
    "File Read"                  = "#B07AA1",
    "Evidence Deleted"           = "#555555",
    "Anomalous Post (INJECTION)" = "#D84040"
  )
  
  shape_map <- c(
    "Task Delegation"            = 16,
    "Gate Check"                 = 18,
    "File Read"                  = 16,
    "Evidence Deleted"           = 15,
    "Anomalous Post (INJECTION)" = 16
  )
  
  size_map <- c(
    "Task Delegation"            = 4,
    "Gate Check"                 = 5,
    "File Read"                  = 4,
    "Evidence Deleted"           = 5,
    "Anomalous Post (INJECTION)" = 10
  )
  
  p <- ggplot2::ggplot(
    timeline_df,
    ggplot2::aes(
      x      = datetime,
      y      = actor_factor,
      colour = event_label,
      size   = event_label,
      shape  = event_label,
      text   = tip
    )
  ) +
    # Swimlane bands BEHIND points — use rect not hline
    ggplot2::geom_rect(
      data = data.frame(
        ymin = seq(0.5, length(levels(timeline_df$actor_factor)) - 0.5, 1),
        ymax = seq(1.5, length(levels(timeline_df$actor_factor)) + 0.5, 1),
        fill = rep(c("#F9F9F9", "white"),
                   length.out = length(levels(timeline_df$actor_factor)))
      ),
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = fill),
      xmin = -Inf, xmax = Inf, inherit.aes = FALSE, alpha = 0.6
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_point(alpha = 0.9, stroke = 0) +
    # Pulse ring on anomalous only
    ggplot2::geom_point(
      data   = dplyr::filter(timeline_df, is_anomalous),
      colour = "#D84040", size = 16, alpha = 0.2,
      shape = 1, stroke = 1.5, inherit.aes = FALSE,
      mapping = ggplot2::aes(x = datetime, y = actor_factor)
    ) +
    ggplot2::scale_colour_manual(values = colour_map, name = "Event Type") +
    ggplot2::scale_size_manual(values = size_map, guide = "none") +
    ggplot2::scale_shape_manual(values = shape_map, guide = "none") +
    ggplot2::facet_wrap(~ date_label, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      title = paste0("Chain Actor Activity Timeline — ",
                     ifelse(selected_date == "All", "All Incident Dates", selected_date)),
      subtitle = paste0(
        "\u25cf Large red = anomalous post  \u2022  ",
        "\u25c6 Diamond = gate check  \u2022  ",
        "\u25a0 Square = evidence deleted  \u2022  ",
        "Same 9 actors appear across all 3 dates"
      ),
      x = "Time (UTC)", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle      = ggplot2::element_text(colour = "#666666", size = 9),
      legend.position    = "bottom",
      strip.text         = ggplot2::element_text(face = "bold", size = 10,
                                                 colour = "white",
                                                 margin = ggplot2::margin(3,3,3,3)),
      strip.background   = ggplot2::element_rect(fill = "#1D9E75", colour = NA),
      panel.grid.major.x = ggplot2::element_line(colour = "#1a2e35"),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 9, colour = "#333333"),
      panel.spacing      = ggplot2::unit(0.6, "lines")
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", y = -0.1,
                                 font = list(size = 10)))
}
