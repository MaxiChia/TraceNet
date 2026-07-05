# =============================================================================
# modules/mod3_historical.R
# Module 3 — Historical Patterns & Intervention
#
# Three render functions:
#   mod3_render_heatmap()      → actor participation heatmap across 3 incidents
#   mod3_render_sequence()     → terminal 4-event sequence comparison
#   mod3_render_intervention() → before/after intervention flow diagram
#
# Confirmed terminal sequences (hardcoded from data):
#   10 May: 12:45:41-44 UTC | HiddenOrca.txt
#   11 May: 00:56:03-06 UTC | MellowOtter.txt
#   17 May: 11:21:14-17 UTC | SwiftWren.txt
# =============================================================================


# Helper to extract chain actors per incident date
# Uses queue_subordinate_task + task == read_file, same as THEx02 Figure 8.1
get_chain_actors_per_date <- function(target_date, cutoff_time) {
  mc2_df |>
    dplyr::filter(
      date       == as.Date(target_date),
      short_name == "queue_subordinate_task",
      details_task == "read_file",
      datetime   <= as.POSIXct(cutoff_time, tz = "UTC")
    ) |>
    dplyr::mutate(
      issuer   = purrr::map_chr(parties, ~ stringr::str_remove(.x[[1]], "Agent/person:")),
      receiver = stringr::str_remove(target_agent, "Agent/person:")
    ) |>
    dplyr::select(issuer, receiver) |>
    tidyr::pivot_longer(everything(), values_to = "actor") |>
    dplyr::distinct(actor) |>
    dplyr::filter(!is.na(actor), actor != "") |>
    dplyr::mutate(date = target_date)
}


# =============================================================================
# mod3_render_heatmap
# Actor × incident date presence/absence tile heatmap
# Replicates THEx02 Figure 8.1 as an interactive plotly chart
# =============================================================================
mod3_render_heatmap <- function(top_n = 15) {
  
  # Extract chain actors for each incident using confirmed cutoff times
  chain_may10 <- get_chain_actors_per_date("2046-05-10", "2046-05-10 12:45:40")
  chain_may11 <- get_chain_actors_per_date("2046-05-11", "2046-05-11 00:56:03")
  chain_may17 <- get_chain_actors_per_date("2046-05-17", "2046-05-17 11:21:13")
  
  all_chains <- dplyr::bind_rows(chain_may10, chain_may11, chain_may17) |>
    dplyr::mutate(
      present    = 1,
      date_label = dplyr::case_when(
        date == "2046-05-10" ~ "10 May\n(HiddenOrca.txt)",
        date == "2046-05-11" ~ "11 May\n(MellowOtter.txt)",
        date == "2046-05-17" ~ "17 May\n(SwiftWren.txt)"
      ),
      date_label = factor(date_label,
                          levels = c("10 May\n(HiddenOrca.txt)",
                                     "11 May\n(MellowOtter.txt)",
                                     "17 May\n(SwiftWren.txt)"))
    )
  
  # Rank actors by number of incidents they appear in
  actor_freq <- all_chains |>
    dplyr::group_by(actor) |>
    dplyr::summarise(n_incidents = dplyr::n_distinct(date), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n_incidents), actor) |>
    dplyr::slice_head(n = top_n)
  
  heatmap_data <- all_chains |>
    dplyr::filter(actor %in% actor_freq$actor) |>
    tidyr::complete(actor, date_label, fill = list(present = 0)) |>
    dplyr::left_join(actor_freq, by = "actor") |>
    dplyr::mutate(
      actor_label  = stringr::str_replace_all(actor, "_", " ") |>
        stringr::str_to_title(),
      fill_label   = ifelse(present == 1, "Present", "Absent"),
      is_terminal  = actor == TERMINAL_EXECUTOR,
      is_core      = actor %in% stringr::str_remove(
        c("person:victoria_rigging", "person:mia_fender",
          "person:lily_anchorline", "person:john_windward",
          "person:daniel_gangway", "person:chloe_ballast"), "person:"),
      tip = paste0(
        "<b>", stringr::str_replace_all(actor, "_", " ") |>
          stringr::str_to_title(), "</b><br>",
        "Incidents: ", n_incidents, " of 3<br>",
        "Status: ", fill_label,
        ifelse(is_terminal, "<br><b>★ Terminal Executor</b>", ""),
        ifelse(is_core & !is_terminal, "<br>Core network actor", "")
      )
    )
  
  p <- ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(
      x    = date_label,
      y    = reorder(actor_label, n_incidents),
      fill = fill_label,
      text = tip
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      data    = dplyr::filter(heatmap_data, is_terminal & present == 1),
      mapping = ggplot2::aes(x = date_label,
                             y = reorder(actor_label, n_incidents),
                             label = "\u2605"),
      colour = "white", size = 6.3, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("Present" = "#1D9E75", "Absent" = "#182C34"),
      name   = NULL
    ) +
    ggplot2::labs(
      title    = "Actor Participation Across Three Anomalous Incidents",
      subtitle = "\u2605 = John Windward (terminal executor, present in all 3)  |  Actors ordered by number of incidents",
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 17, colour = "#E8EAEA"),
      plot.subtitle   = ggplot2::element_text(colour = "#5DCAA5", size = 12),
      panel.grid      = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_text(size = 14, colour = "#E8EAEA"),
      axis.text.x     = ggplot2::element_text(size = 14, face = "bold", colour = "#E8EAEA"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.key = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.text = ggplot2::element_text(colour = "#E8EAEA")
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      paper_bgcolor = "#0B1418",
      plot_bgcolor  = "#0B1418",
      font = list(color = "#E8EAEA", size = 15),
      legend = list(orientation = "h", y = -0.08, font = list(color = "#E8EAEA", size = 14))
    )
}


# =============================================================================
# mod3_render_sequence
# Terminal 4-event sequence aligned at +0s baseline across all 3 incidents
# Proves scripted one-second-precision timing (not manual action)
# =============================================================================
mod3_render_sequence <- function() {
  
  # Hardcoded from confirmed diagnostic output
  seq_df <- tibble::tibble(
    incident = c(
      rep("10 May 2046\n(HiddenOrca.txt)", 4),
      rep("11 May 2046\n(MellowOtter.txt)", 4),
      rep("17 May 2046\n(SwiftWren.txt)", 4)
    ),
    step = rep(c(
      "Step 1: saidit_post_check",
      "Step 2: saidit_post (INJECTION)",
      "Step 3: delete_file",
      "Step 4: delete_file"
    ), 3),
    elapsed    = rep(c(0, 1, 2, 3), 3),
    event_type = rep(c("saidit_post_check", "saidit_post",
                       "delete_file", "delete_file"), 3),
    anchor_time = c(
      rep("12:45:41 UTC", 4),
      rep("00:56:03 UTC", 4),
      rep("11:21:14 UTC", 4)
    ),
    injected_file = c(
      rep("HiddenOrca.txt", 4),
      rep("MellowOtter.txt", 4),
      rep("SwiftWren.txt", 4)
    )
  ) |>
    dplyr::mutate(
      incident  = factor(incident,
                         levels = c("10 May 2046\n(HiddenOrca.txt)",
                                    "11 May 2046\n(MellowOtter.txt)",
                                    "17 May 2046\n(SwiftWren.txt)")),
      step      = factor(step, levels = rev(c(
        "Step 1: saidit_post_check",
        "Step 2: saidit_post (INJECTION)",
        "Step 3: delete_file",
        "Step 4: delete_file"
      ))),
      is_injection = event_type == "saidit_post",
      tip = paste0(
        "<b>", stringr::str_replace(as.character(step), "\n", " "), "</b><br>",
        "Incident: ", stringr::str_replace(as.character(incident), "\n", " "), "<br>",
        "Elapsed: +", elapsed, "s from post-check<br>",
        "Anchor time: ", anchor_time, "<br>",
        ifelse(is_injection,
               paste0("<b>Injected file: ", injected_file, "</b>"), "")
      )
    )
  
  colour_map <- c(
    "saidit_post_check" = "#1D9E75",
    "saidit_post"       = ANOMALY_COLOUR,
    "delete_file"       = "#3d5a63"
  )
  
  p <- ggplot2::ggplot(
    seq_df,
    ggplot2::aes(
      x      = incident,
      y      = step,
      colour = event_type,
      size   = is_injection,
      text   = tip
    )
  ) +
    ggplot2::geom_point(alpha = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("+", elapsed, "s")),
      vjust = -1.2, size = 4.4, colour = "#5DCAA5", show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = colour_map, name = "Event Type") +
    ggplot2::scale_size_manual(
      values = c("FALSE" = 6, "TRUE" = 12), guide = "none"
    ) +
    ggplot2::labs(
      title    = "Terminal Sequence Comparison — All Three Incidents",
      subtitle = "4-event sequence repeats at one-second precision across different times of day\u2003\u2192\u2003evidence of a scripted automated protocol",
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 17, colour = "#E8EAEA"),
      plot.subtitle   = ggplot2::element_text(colour = "#5DCAA5", size = 12),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#243D47"),
      axis.text.x     = ggplot2::element_text(face = "bold", size = 14, colour = "#E8EAEA"),
      axis.text.y     = ggplot2::element_text(size = 14, colour = "#E8EAEA"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.key = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.text = ggplot2::element_text(colour = "#E8EAEA"),
      legend.title = ggplot2::element_text(colour = "#E8EAEA")
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      paper_bgcolor = "#0B1418",
      plot_bgcolor  = "#0B1418",
      font = list(color = "#E8EAEA", size = 15),
      legend = list(orientation = "h", y = -0.08, font = list(color = "#E8EAEA", size = 14))
    )
}


# =============================================================================
# mod3_render_intervention
# Presentation-first intervention diagram.
# Uses a fixed aspect ratio and compact geometry so the flowchart reads clearly
# in the Shiny viewport instead of being stretched wide or vertically squeezed.
# =============================================================================
mod3_render_intervention <- function(panel = "intervention_before") {
  diamond_df <- function(cx, cy, w, h, id) {
    tibble::tibble(
      x     = c(cx, cx + w/2, cx, cx - w/2),
      y     = c(cy + h/2, cy, cy - h/2, cy),
      group = id
    )
  }
  
  arrow_obj <- ggplot2::arrow(
    length = ggplot2::unit(3.5, "mm"),
    type   = "closed"
  )
  
  base_theme <- ggplot2::theme_void() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      plot.margin      = ggplot2::margin(2, 2, 2, 2)
    )
  
  if (panel == "intervention_before") {
    ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin = 0.35, xmax = 11.65, ymin = 0.35, ymax = 8.65,
                        fill = "#170D0E", colour = "#D84040", linewidth = 1.0) +
      ggplot2::annotate("text", x = 6, y = 8.18,
                        label = "CURRENT STATE — Vulnerable",
                        colour = "#D84040", fontface = "bold", size = 6.4) +
      ggplot2::annotate("text", x = 6, y = 7.78,
                        label = "Validation gate fires, but does not inspect content_source",
                        colour = "#B8B8B8", fontface = "italic", size = 3.7) +
      
      ggplot2::annotate("rect", xmin = 3.05, xmax = 8.95, ymin = 6.85, ymax = 7.42,
                        fill = "#1D9E75", colour = "#E8EAEA", linewidth = 0.6) +
      ggplot2::annotate("text", x = 6, y = 7.13,
                        label = "queue_subordinate_task  (read_file)",
                        colour = "white", size = 4.4) +
      ggplot2::annotate("segment", x = 6, xend = 6, y = 6.85, yend = 6.25,
                        arrow = arrow_obj, colour = "#A0A0A0", linewidth = 0.7) +
      
      ggplot2::geom_polygon(
        data = diamond_df(6, 5.55, 5.8, 1.35, "d_current"),
        mapping = ggplot2::aes(x = x, y = y, group = group),
        fill = "#E8C547", colour = "#FFFFFF", linewidth = 0.7
      ) +
      ggplot2::annotate("text", x = 6, y = 5.78,
                        label = "saidit_post_check",
                        colour = "white", size = 4.9, fontface = "bold") +
      ggplot2::annotate("text", x = 6, y = 5.38,
                        label = "missing content_source check",
                        colour = "#FFF4B8", size = 3.5, fontface = "italic") +
      
      ggplot2::annotate("segment", x = 6, xend = 6, y = 4.88, yend = 4.18,
                        arrow = arrow_obj, colour = "#D84040", linewidth = 1.4) +
      ggplot2::annotate("text", x = 7.85, y = 4.52,
                        label = "PASS — injection not blocked",
                        colour = "#D84040", size = 3.7, fontface = "bold") +
      
      ggplot2::annotate("rect", xmin = 2.05, xmax = 9.95, ymin = 3.20, ymax = 4.12,
                        fill = "#D84040", colour = "#FFFFFF", linewidth = 0.7) +
      ggplot2::annotate("text", x = 6, y = 3.78,
                        label = "saidit_post — INJECTION SUCCEEDS",
                        colour = "white", size = 4.6, fontface = "bold") +
      ggplot2::annotate("text", x = 6, y = 3.40,
                        label = "content_source populated while file = NA",
                        colour = "#FFD2D2", size = 3.4) +
      
      ggplot2::annotate("segment", x = 6, xend = 6, y = 3.20, yend = 2.55,
                        arrow = arrow_obj, colour = "#A0A0A0", linewidth = 0.7) +
      ggplot2::annotate("rect", xmin = 3.20, xmax = 8.80, ymin = 1.65, ymax = 2.55,
                        fill = "#3D5A63", colour = "#FFFFFF", linewidth = 0.7) +
      ggplot2::annotate("text", x = 6, y = 2.20,
                        label = "delete_file × 2 — evidence destroyed",
                        colour = "white", size = 4.0, fontface = "bold") +
      ggplot2::annotate("text", x = 6, y = 1.13,
                        label = "Result: the anomalous post passes through and traces are removed within seconds.",
                        colour = "#B8B8B8", size = 3.5, fontface = "italic") +
      ggplot2::coord_fixed(ratio = 0.82, xlim = c(0, 12), ylim = c(0.5, 8.8), expand = FALSE) +
      base_theme
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin = 0.35, xmax = 11.65, ymin = 0.35, ymax = 8.65,
                        fill = "#091711", colour = "#1D9E75", linewidth = 1.0) +
      ggplot2::annotate("text", x = 6, y = 8.18,
                        label = "PROPOSED STATE — Fixed at the Gate",
                        colour = "#5DCAA5", fontface = "bold", size = 6.0) +
      ggplot2::annotate("text", x = 6, y = 7.78,
                        label = "One added check: normal posts continue; injected content is stopped before posting",
                        colour = "#B8B8B8", fontface = "italic", size = 3.4) +
      
      ggplot2::annotate("rect", xmin = 3.05, xmax = 8.95, ymin = 6.85, ymax = 7.42,
                        fill = "#1D9E75", colour = "#E8EAEA", linewidth = 0.6) +
      ggplot2::annotate("text", x = 6, y = 7.13,
                        label = "queue_subordinate_task  (read_file)",
                        colour = "white", size = 4.4) +
      ggplot2::annotate("segment", x = 6, xend = 6, y = 6.85, yend = 6.25,
                        arrow = arrow_obj, colour = "#A0A0A0", linewidth = 0.7) +
      
      ggplot2::geom_polygon(
        data = diamond_df(6, 5.55, 5.5, 1.55, "d_proposed"),
        mapping = ggplot2::aes(x = x, y = y, group = group),
        fill = "#E8C547", colour = "#FFFFFF", linewidth = 0.7
      ) +
      ggplot2::annotate("text", x = 6, y = 5.82,
                        label = "saidit_post_check",
                        colour = "white", size = 4.4, fontface = "bold") +
      ggplot2::annotate("text", x = 6, y = 5.50,
                        label = "+ content_source null-check",
                        colour = "#FFF4B8", size = 3.3, fontface = "bold") +
      ggplot2::annotate("text", x = 6, y = 5.20,
                        label = "Is content_source empty?",
                        colour = "#FFF4B8", size = 3.1, fontface = "italic") +
      
      ggplot2::annotate("segment", x = 5.15, xend = 3.35, y = 4.98, yend = 4.23,
                        arrow = arrow_obj, colour = "#1D9E75", linewidth = 1.4) +
      ggplot2::annotate("segment", x = 6.85, xend = 8.65, y = 4.98, yend = 4.23,
                        arrow = arrow_obj, colour = "#D84040", linewidth = 1.4) +
      ggplot2::annotate("text", x = 2.45, y = 4.75,
                        label = "ALLOW\nnormal post",
                        colour = "#5DCAA5", size = 3.3, fontface = "bold",
                        hjust = 0.5, lineheight = 0.95) +
      ggplot2::annotate("text", x = 9.55, y = 4.75,
                        label = "BLOCK\ninjected content",
                        colour = "#D84040", size = 3.3, fontface = "bold",
                        hjust = 0.5, lineheight = 0.95) +
      
      ggplot2::annotate("rect", xmin = 1.05, xmax = 5.25, ymin = 3.02, ymax = 4.02,
                        fill = "#1D9E75", colour = "#FFFFFF", linewidth = 0.7) +
      ggplot2::annotate("text", x = 3.15, y = 3.62,
                        label = "saidit_post",
                        colour = "white", size = 4.3, fontface = "bold") +
      ggplot2::annotate("text", x = 3.15, y = 3.23,
                        label = "normal post proceeds",
                        colour = "#CFF5E8", size = 3.3) +
      
      ggplot2::annotate("rect", xmin = 6.75, xmax = 10.95, ymin = 3.02, ymax = 4.02,
                        fill = "#D84040", colour = "#FFFFFF", linewidth = 0.7) +
      ggplot2::annotate("text", x = 8.85, y = 3.62,
                        label = "BLOCK + ALERT",
                        colour = "white", size = 4.3, fontface = "bold") +
      ggplot2::annotate("text", x = 8.85, y = 3.23,
                        label = "injection prevented",
                        colour = "#FFD2D2", size = 3.3) +
      
      ggplot2::annotate("rect", xmin = 1.25, xmax = 10.75, ymin = 1.10, ymax = 2.10,
                        fill = "#0B1418", colour = "#5DCAA5", linewidth = 0.8) +
      ggplot2::annotate("text", x = 6, y = 1.60,
                        label = "Fix impact: the same gate blocks all 3 confirmed incidents.",
                        colour = "#5DCAA5", size = 3.7, fontface = "bold") +
      ggplot2::coord_fixed(ratio = 0.82, xlim = c(0, 12), ylim = c(0.7, 8.8), expand = FALSE) +
      base_theme
  }
}
