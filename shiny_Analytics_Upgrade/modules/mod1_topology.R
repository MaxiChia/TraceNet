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
  font.color = "#1E293B",
  shadow = TRUE,
  x = c(500, 300, 100, -100, -300, -300, -100, 100, 300),
  y = c(0,   0,   -100, 0,   -100, 100,  100,  200, 300),
  title = c(
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>James Stern — Chain Root</b><br>First delegation: 00:51:43 UTC<br>Initiates the relay on all 3 incident dates</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Gabriel Sonar</b><br>Received: 00:51 | Delegated: 02:26<br>Delay: 1h 35min</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Daniel Gangway</b><br><span style='color:#E8A52B'>Reused node — step 2 and step 8</span><br>Deliberate obfuscation tactic</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Zoey Drydock</b><br><span style='color:#E8A52B'>Reused node — step 3 and step 6</span><br>Deliberate obfuscation tactic</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Mia Fender</b><br>Received: 05:34 | Delegated: 05:52</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Victoria Rigging</b><br>Received: 05:52 | Delegated: 06:44</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Lily Anchorline</b><br>Received: 08:16 | Delegated: 10:11</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b>Chloe Ballast</b><br>Received: 11:12 | Delegated: 11:21</div>",
    "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5;font-family:Inter,sans-serif'><b style='color:#D84040'>John Windward — TERMINAL EXECUTOR</b><br>Received task: 11:21:13 UTC<br>Anomalous post: 11:21:15 UTC (+2s)<br>Evidence deleted: 11:21:16–17 UTC<br>Injected file: SwiftWren.txt</div>"
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
  font.color  = c(rep("#1D9E75", 9), "#D84040"),
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

# -----------------------------------------------------------------------------
# Fallback centrality metrics for deployed app
# Creates network_metrics if it was not loaded elsewhere.
# -----------------------------------------------------------------------------
if (!exists("network_metrics")) {
  
  relay_vertices <- RELAY_NODES |>
    dplyr::transmute(
      name = id,
      actor = stringr::str_to_title(stringr::str_replace_all(id, "_", " ")),
      role = dplyr::case_when(
        id == "james_stern" ~ "Chain Root",
        id == "john_windward" ~ "Terminal Executor",
        id %in% c("daniel_gangway", "zoey_drydock") ~ "Reused Relay Actor",
        TRUE ~ "Core Relay Actor"
      )
    )
  
  relay_edges_for_metrics <- RELAY_EDGES |>
    dplyr::select(from, to)
  
  relay_graph <- igraph::graph_from_data_frame(
    d = relay_edges_for_metrics,
    directed = TRUE,
    vertices = relay_vertices
  )
  
  safe_num <- function(x) {
    x[is.na(x) | is.nan(x) | is.infinite(x)] <- 0
    x
  }
  
  network_metrics <- tibble::tibble(
    actor = igraph::V(relay_graph)$actor,
    role = igraph::V(relay_graph)$role,
    degree = safe_num(igraph::degree(relay_graph, mode = "all")),
    in_degree = safe_num(igraph::degree(relay_graph, mode = "in")),
    out_degree = safe_num(igraph::degree(relay_graph, mode = "out")),
    betweenness = safe_num(igraph::betweenness(relay_graph, directed = TRUE)),
    closeness = safe_num(igraph::closeness(relay_graph, mode = "all", normalized = TRUE)),
    eigenvector = safe_num(igraph::eigen_centrality(relay_graph, directed = TRUE)$vector)
  )
}

mod1_render_network <- function(
    data,
    network_mode = "relay",
    show_labels = TRUE,
    selected_date = "all",
    centrality_metric = "degree",
    size_by_centrality = TRUE,
    focus_actor = "all"
) {
  if (network_mode == "relay") {
    mod1_network_relay(show_labels)
  } else {
    mod1_network_core(
      data = data,
      show_labels = show_labels,
      selected_date = selected_date,
      centrality_metric = centrality_metric,
      size_by_centrality = size_by_centrality,
      focus_actor = focus_actor
    )
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
    height = "calc(100vh - 220px)",
    main   = list(
      text  = "11-Step Relay Sequence — 17 May 2046 (9 unique agents; same structure confirmed across all 3 incidents)",
      style = "font-size:13px; font-weight:bold; color:#1D9E75; text-align:left; padding-left:10px;"
    )
  ) |>
    visNetwork::visEdges(
      smooth = list(type = "curvedCW", roundness = 0.3),
      font   = list(align = "top", strokeWidth = 0)
    ) |>
    visNetwork::visNodes(
      font = list(
        color = "#1E293B",
        size = 22,
        face = "arial",
        strokeWidth = 4,
        strokeColor = "#FFFFFF"
      ),
      shadow = list(enabled = TRUE, size = 8)
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
        font.color       = "#1E293B",
        shape = "dot", size = 20, stringsAsFactors = FALSE
      ),
      addEdges = data.frame(
        label = c("Task delegation", "Terminal injection step"),
        color = c("#3d5a63", "#D84040"),
        width = c(2, 6),
        font.color = c("#5DCAA5", "#D84040"),
        stringsAsFactors = FALSE
      ),
      useGroups = FALSE, position = "right", width = 0.22
    ) |>
    visNetwork::visInteraction(
      navigationButtons = TRUE, tooltipDelay = 80,
      dragNodes = TRUE, zoomView = TRUE
    ) |>
    visNetwork::visEvents(type = "once", afterDrawing = "function() { this.fit({animation: false}); }")
}

# -----------------------------------------------------------------------------
# CORE MODE
# -----------------------------------------------------------------------------
mod1_network_core <- function(
    data,
    show_labels,
    selected_date = "All",
    centrality_metric = "degree",
    size_by_centrality = TRUE,
    focus_actor = "all"
) {
  # Build from confirmed relay nodes with centrality-driven sizing
  # Lesson 7a: node size encodes centrality; colour encodes role
  node_df <- RELAY_NODES |>
    dplyr::mutate(
      id    = paste0("Agent/person:", id),
      label = if (isTRUE(show_labels)) RELAY_NODES$label else rep("", dplyr::n()),
      is_core     = TRUE,
      is_terminal = id == "Agent/person:john_windward",
      font.color  = "#1E293B"
    )
  
  # Apply centrality-driven sizing if metrics available
  if (
    exists("network_metrics") &&
    isTRUE(size_by_centrality) &&
    centrality_metric %in% names(network_metrics)
  ) {
    metric_df <- network_metrics |>
      dplyr::mutate(
        id = paste0("Agent/person:",
                    stringr::str_to_lower(stringr::str_replace_all(actor, " ", "_")))
      ) |>
      dplyr::select(id, centrality_value = dplyr::all_of(centrality_metric))
    
    node_df <- node_df |>
      dplyr::left_join(metric_df, by = "id") |>
      dplyr::mutate(
        centrality_value = dplyr::coalesce(centrality_value, 0),
        value = dplyr::case_when(
          is_terminal        ~ 55,
          centrality_value <= 0 ~ 22,
          TRUE               ~ scales::rescale(centrality_value, to = c(22, 55))
        ),
        font.size = dplyr::case_when(
          is_terminal        ~ 18,
          centrality_value <= 0 ~ 11,
          TRUE               ~ scales::rescale(centrality_value, to = c(11, 18))
        ),
        title = paste0(
          "<div style='background:#0f1d22;color:#1E293B;padding:8px 12px;border-radius:6px;border:1px solid #5DCAA5'>",
          "<b>", stringr::str_remove(id, "Agent/person:"), "</b><br>",
          ifelse(is_terminal, "⚡ Terminal Executor", "Core Relay Actor"), "<br>",
          "<b style='color:#5DCAA5'>", centrality_metric, " score:</b> ", round(centrality_value, 4),
          "</div>"
        )
      ) |>
      dplyr::select(-centrality_value)
  }
  
  # Focus actor highlight
  if (!is.null(focus_actor) && focus_actor != "all") {
    node_df <- node_df |>
      dplyr::mutate(
        color.background = ifelse(id == focus_actor, "#60A5FA", color.background),
        value = ifelse(id == focus_actor, value + 18, value)
      )
  }
  
  edge_df <- RELAY_EDGES |>
    dplyr::mutate(
      from  = paste0("Agent/person:", from),
      to    = paste0("Agent/person:", to),
      color = ifelse(is_terminal, "#D84040", "#3d5a63"),
      width = ifelse(is_terminal, 6, 2.5),
      arrows = "to",
      label = ""
    )
  
  visNetwork::visNetwork(
    nodes = node_df,
    edges = edge_df,
    width  = "100%",
    height = "calc(100vh - 220px)",
    main = list(
      text  = paste0("Relay Chain — Sized by ", centrality_metric, " centrality"),
      style = "font-size:13px; font-weight:bold; color:#1D9E75; text-align:left; padding-left:10px;"
    )
  ) |>
    visNetwork::visEdges(
      smooth = list(type = "curvedCW", roundness = 0.3),
      font   = list(align = "top", strokeWidth = 0)
    ) |>
    visNetwork::visNodes(
      font = list(color = "#1E293B", face = "arial",
                  strokeWidth = 4, strokeColor = "#FFFFFF"),
      shadow = list(enabled = TRUE, size = 8)
    ) |>
    visNetwork::visPhysics(enabled = FALSE) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = FALSE)
    ) |>
    visNetwork::visLegend(
      addNodes = data.frame(
        label            = c("Chain Root", "Reused (obfuscation)",
                             "Relay Node", "Terminal Executor",
                             "Selected Focus Actor",
                             paste0("Node size = ", centrality_metric, " centrality")),
        color.background = c("#E8A52B", "#E8C547", "#1D9E75",
                             "#D84040", "#60A5FA", "transparent"),
        font.color = "#1E293B",
        shape = c("dot","dot","dot","dot","dot","text"),
        size  = c(20, 20, 20, 20, 20, 1),
        stringsAsFactors = FALSE
      ),
      addEdges = data.frame(
        label = c("Task delegation", "Terminal injection step"),
        color = c("#3d5a63", "#D84040"),
        width = c(2, 6),
        font.color = c("#5DCAA5", "#D84040"),
        stringsAsFactors = FALSE
      ),
      useGroups = FALSE, position = "right", width = 0.22
    ) |>
    visNetwork::visInteraction(
      navigationButtons = TRUE, tooltipDelay = 80,
      dragNodes = TRUE, zoomView = TRUE
    ) |>
    visNetwork::visEvents(
      type = "once",
      afterDrawing = "function() { this.fit({animation: false}); }"
    )
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
             plotly::layout(title = "No chain actor events for selected date range.", paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF", font = list(color = "#1E293B")))
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
        fill = rep(c("#F1F5F9", "#FFFFFF"),
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
      plot.title         = ggplot2::element_text(face = "bold", size = 12, colour = "#1E293B"),
      plot.subtitle      = ggplot2::element_text(colour = "#64748B", size = 9),
      legend.position    = "bottom",
      strip.text         = ggplot2::element_text(face = "bold", size = 10,
                                                 colour = "white",
                                                 margin = ggplot2::margin(3,3,3,3)),
      strip.background   = ggplot2::element_rect(fill = "#1D9E75", colour = NA),
      panel.grid.major.x = ggplot2::element_line(colour = "#E2E8F0"),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 12, face = "bold", colour = "#1E293B"),
      panel.spacing      = ggplot2::unit(0.6, "lines"),
      plot.background    = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background   = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      legend.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      legend.key         = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      legend.text        = ggplot2::element_text(colour = "#1E293B"),
      legend.title       = ggplot2::element_text(colour = "#1E293B"),
      axis.text.x        = ggplot2::element_text(colour = "#1E293B"),
      axis.title.x       = ggplot2::element_text(colour = "#64748B")
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(color = "#1E293B"),
      yaxis  = list(tickfont = list(size = 13, color = "#1E293B")),
      yaxis2 = list(tickfont = list(size = 13, color = "#1E293B")),
      yaxis3 = list(tickfont = list(size = 13, color = "#1E293B")),
      legend = list(orientation = "h", y = -0.1, font = list(size = 10, color = "#1E293B"))
    )
}

# -----------------------------------------------------------------------------
# CENTRALITY BAR CHART
# Lesson 7a: Visualising and Analysing Network Data — centrality measures
# MC2: "integrates visuals with algorithms to enable interactive exploration"
# -----------------------------------------------------------------------------
mod1_render_centrality_chart <- function(metric = "degree", metrics_df = network_metrics, top_n = 15) {
  
  metric_labels <- c(
    degree      = "Degree (total connections)",
    in_degree   = "In-degree (incoming links)",
    out_degree  = "Out-degree (outgoing links)",
    betweenness = "Betweenness (bridge/control point)",
    closeness   = "Closeness (speed of reaching all nodes)",
    eigenvector = "Eigenvector (influence via important neighbours)"
  )
  
  metric_label <- metric_labels[[metric]]
  
  chart_df <- metrics_df |>
    dplyr::mutate(
      actor_label    = stringr::str_to_title(stringr::str_replace_all(actor, "_", " ")),
      centrality_val = as.numeric(.data[[metric]]),
      role_colour    = dplyr::case_when(
        role == "Chain Root"         ~ "#E8A52B",
        role == "Terminal Executor"  ~ "#D84040",
        role == "Reused Relay Actor" ~ "#E8C547",
        TRUE                         ~ "#1D9E75"
      )
    ) |>
    dplyr::arrange(dplyr::desc(centrality_val)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(
      actor_label = factor(actor_label, levels = rev(actor_label))
    )
  
  ggplot2::ggplot(chart_df,
                  ggplot2::aes(x = actor_label, y = centrality_val, fill = role_colour)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(
      ggplot2::aes(label = round(centrality_val, 3)),
      hjust = -0.15, colour = "#1E293B", size = 3.5
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = metric_label,
      subtitle = "Relay chain actors ranked by selected centrality measure",
      x = NULL, y = metric_label
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", colour = "#1E293B", size = 14),
      plot.subtitle    = ggplot2::element_text(colour = "#64748B", size = 11),
      axis.text.y      = ggplot2::element_text(colour = "#1E293B", size = 11),
      axis.text.x      = ggplot2::element_text(colour = "#64748B"),
      axis.title.x     = ggplot2::element_text(colour = "#64748B"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#E2E8F0"),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.background    = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background   = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      plot.margin        = ggplot2::margin(16, 40, 16, 16)
    )
}