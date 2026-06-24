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
      colour = "white", size = 5, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("Present" = "#4E79A7", "Absent" = "#E8E8E8"),
      name   = NULL
    ) +
    ggplot2::labs(
      title    = "Actor Participation Across Three Anomalous Incidents",
      subtitle = "\u2605 = John Windward (terminal executor, present in all 3)  |  Actors ordered by number of incidents",
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      plot.subtitle   = ggplot2::element_text(colour = "#666666", size = 10),
      panel.grid      = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_text(size = 10),
      axis.text.x     = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", y = -0.1))
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
    "saidit_post_check" = "#4E79A7",
    "saidit_post"       = ANOMALY_COLOUR,
    "delete_file"       = "#555555"
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
      vjust = -1.2, size = 3.5, colour = "#666666", show.legend = FALSE
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
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      plot.subtitle   = ggplot2::element_text(colour = "#666666", size = 10),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#EEEEEE"),
      axis.text.x     = ggplot2::element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", y = -0.1))
}


# =============================================================================
# mod3_render_intervention
# Before/after intervention flow diagram — replicates THEx02 Figure 9.1
# Shows the single null-check addition at saidit_post_check that blocks all 3
# panel = "intervention_before" → current vulnerable state
# panel = "intervention_after"  → proposed fix state
# =============================================================================
mod3_render_intervention <- function(panel = "intervention_before") {
  
  # Diamond shape helper
  diamond_df <- function(cx, cy, w, h, id) {
    tibble::tibble(
      x     = c(cx, cx + w/2, cx, cx - w/2),
      y     = c(cy + h/2, cy, cy - h/2, cy),
      group = id
    )
  }
  
  if (panel == "intervention_before") {
    
    # -------------------------------------------------------------------------
    # CURRENT STATE — Vulnerable
    # -------------------------------------------------------------------------
    ggplot2::ggplot() +
      ggplot2::annotate("rect",
                        xmin = 0.5, xmax = 9.5, ymin = 0.3, ymax = 11.2,
                        fill = "#FFF5F5", color = "#E15759", linewidth = 1.5) +
      ggplot2::annotate("text", x = 5, y = 10.7,
                        label = "CURRENT STATE \u2014 Vulnerable",
                        color = "#E15759", fontface = "bold", size = 6) +
      ggplot2::annotate("text", x = 5, y = 10.15,
                        label = "saidit_post_check does not inspect content_source \u2014 injection passes unchallenged",
                        color = "#888888", size = 3.5, fontface = "italic") +
      ggplot2::annotate("rect",
                        xmin = 2.5, xmax = 7.5, ymin = 8.8, ymax = 9.7,
                        fill = "#4E79A7", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 9.25,
                        label = "queue_subordinate_task  (read_file)",
                        color = "white", size = 4) +
      ggplot2::annotate("segment",
                        x = 5, xend = 5, y = 8.8, yend = 7.9,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
                        color = "grey50") +
      ggplot2::geom_polygon(
        data = diamond_df(5, 6.7, 5.0, 2.2, "d1"),
        mapping = ggplot2::aes(x = x, y = y, group = group),
        fill = "#F28E2B", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 7.0,
                        label = "saidit_post_check",
                        color = "white", size = 4.5, fontface = "bold") +
      ggplot2::annotate("text", x = 5, y = 6.6,
                        label = "Checks: can post be made?",
                        color = "white", size = 3.5) +
      ggplot2::annotate("text", x = 5, y = 6.2,
                        label = "Does NOT inspect content_source",
                        color = "#FFE0B2", size = 3.2, fontface = "italic") +
      ggplot2::annotate("text", x = 3.8, y = 5.55,
                        label = "PASS (unconditional \u2014 no content_source check)",
                        color = "#E15759", fontface = "bold", size = 3.2) +
      ggplot2::annotate("segment",
                        x = 5, xend = 5, y = 5.6, yend = 4.7,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
                        color = "#E15759", linewidth = 1.2) +
      ggplot2::annotate("rect",
                        xmin = 1.2, xmax = 8.8, ymin = 3.5, ymax = 4.7,
                        fill = "#E15759", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 4.2,
                        label = "saidit_post  \u2014  INJECTION SUCCEEDS",
                        color = "white", size = 4.2, fontface = "bold") +
      ggplot2::annotate("text", x = 5, y = 3.75,
                        label = "content_source = HiddenOrca.txt  |  file = NA  (anomalous field population)",
                        color = "#FFD0D0", size = 3.2) +
      ggplot2::annotate("text", x = 3.8, y = 3.1,
                        label = "+1s \u2192 delete_file \u00d7 2  (evidence destroyed within 2 seconds)",
                        color = "grey50", size = 3, fontface = "italic") +
      ggplot2::annotate("segment",
                        x = 5, xend = 5, y = 3.5, yend = 2.6,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
                        color = "grey50") +
      ggplot2::annotate("rect",
                        xmin = 2.5, xmax = 7.5, ymin = 1.5, ymax = 2.6,
                        fill = "#555555", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 2.05,
                        label = "delete_file \u00d7 2  \u2014  Evidence destroyed",
                        color = "white", size = 3.8) +
      ggplot2::coord_cartesian(xlim = c(0.5, 9.5), ylim = c(1.0, 11.5), expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(4, 4, 4, 4))
    
  } else {
    
    # -------------------------------------------------------------------------
    # PROPOSED STATE — Intervention Applied
    # -------------------------------------------------------------------------
    ggplot2::ggplot() +
      ggplot2::annotate("rect",
                        xmin = 0.5, xmax = 9.5, ymin = 0.3, ymax = 11.2,
                        fill = "#F5FFF5", color = "#59A14F", linewidth = 1.5) +
      ggplot2::annotate("text", x = 5, y = 10.7,
                        label = "PROPOSED STATE \u2014 Intervention Applied",
                        color = "#59A14F", fontface = "bold", size = 6) +
      ggplot2::annotate("text", x = 5, y = 10.15,
                        label = "One null-check added to saidit_post_check \u2014 blocks all 3 incidents with no new infrastructure",
                        color = "#888888", size = 3.5, fontface = "italic") +
      ggplot2::annotate("rect",
                        xmin = 2.5, xmax = 7.5, ymin = 8.8, ymax = 9.7,
                        fill = "#4E79A7", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 9.25,
                        label = "queue_subordinate_task  (read_file)",
                        color = "white", size = 4) +
      ggplot2::annotate("segment",
                        x = 5, xend = 5, y = 8.8, yend = 7.9,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
                        color = "grey50") +
      ggplot2::geom_polygon(
        data = diamond_df(5, 6.7, 5.0, 2.2, "d2"),
        mapping = ggplot2::aes(x = x, y = y, group = group),
        fill = "#F28E2B", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 7.05,
                        label = "saidit_post_check  (ENHANCED)",
                        color = "white", size = 4.2, fontface = "bold") +
      ggplot2::annotate("text", x = 5, y = 6.6,
                        label = "Checks: can post be made?",
                        color = "white", size = 3.5) +
      ggplot2::annotate("text", x = 5, y = 6.2,
                        label = "+ NEW: is content_source NULL?",
                        color = "#FFF176", size = 3.5, fontface = "bold") +
      ggplot2::annotate("segment",
                        x = 5, xend = 2.2, y = 5.6, yend = 4.7,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
                        color = "#59A14F", linewidth = 1.2) +
      ggplot2::annotate("text", x = 2.4, y = 5.25,
                        label = "PASS\ncontent_source = NA",
                        color = "#59A14F", fontface = "bold", size = 3.2) +
      ggplot2::annotate("segment",
                        x = 5, xend = 7.8, y = 5.6, yend = 4.7,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm"), type = "closed"),
                        color = "#E15759", linewidth = 1.2) +
      ggplot2::annotate("text", x = 7.6, y = 5.25,
                        label = "FAIL\ncontent_source \u2260 NA",
                        color = "#E15759", fontface = "bold", size = 3.2) +
      ggplot2::annotate("rect",
                        xmin = 0.6, xmax = 3.8, ymin = 3.5, ymax = 4.7,
                        fill = "#59A14F", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 2.2, y = 4.2,
                        label = "saidit_post",
                        color = "white", size = 4, fontface = "bold") +
      ggplot2::annotate("text", x = 2.2, y = 3.75,
                        label = "content_source = NA\nfile = filename  (normal)",
                        color = "#C8E6C9", size = 3) +
      ggplot2::annotate("rect",
                        xmin = 6.2, xmax = 9.4, ymin = 3.5, ymax = 4.7,
                        fill = "#E15759", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 7.8, y = 4.2,
                        label = "BLOCK + ALERT",
                        color = "white", size = 4, fontface = "bold") +
      ggplot2::annotate("text", x = 7.8, y = 3.75,
                        label = "Post prevented\nAnomaly flagged for review",
                        color = "#FFD0D0", size = 3) +
      ggplot2::annotate("rect",
                        xmin = 1.5, xmax = 8.5, ymin = 1.2, ymax = 2.5,
                        fill = "#E8F5E9", color = "#59A14F", linewidth = 0.8) +
      ggplot2::annotate("text", x = 5, y = 1.85,
                        label = "No new infrastructure required.  The gate already fires at the right moment.\nOne null-check on content_source is sufficient to block all 3 confirmed incidents.",
                        color = "#2E7D32", size = 3.2, fontface = "italic") +
      ggplot2::coord_cartesian(xlim = c(0.5, 9.5), ylim = c(1.0, 11.5), expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(4, 4, 4, 4))
  }
}
