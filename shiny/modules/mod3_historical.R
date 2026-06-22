# =============================================================================
# modules/mod3_historical.R — VISUAL EXCELLENCE EDITION
# =============================================================================

get_chain_actors_per_date <- function(target_date, cutoff_time) {
  mc2_df |>
    dplyr::filter(
      date         == as.Date(target_date),
      short_name   == "queue_subordinate_task",
      details_task == "read_file",
      datetime     <= as.POSIXct(cutoff_time, tz = "UTC")
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
# mod3_render_heatmap — UPGRADED
# Polished tile heatmap with clear actor role annotations
# =============================================================================
mod3_render_heatmap <- function(top_n = 15) {
  
  chain_may10 <- get_chain_actors_per_date("2046-05-10", "2046-05-10 12:45:40")
  chain_may11 <- get_chain_actors_per_date("2046-05-11", "2046-05-11 00:56:03")
  chain_may17 <- get_chain_actors_per_date("2046-05-17", "2046-05-17 11:21:13")
  
  all_chains <- dplyr::bind_rows(chain_may10, chain_may11, chain_may17) |>
    dplyr::mutate(
      present    = 1,
      date_label = dplyr::case_when(
        date == "2046-05-10" ~ "10 May\nHiddenOrca.txt",
        date == "2046-05-11" ~ "11 May\nMellowOtter.txt",
        date == "2046-05-17" ~ "17 May\nSwiftWren.txt"
      ),
      date_label = factor(date_label,
                          levels = c("10 May\nHiddenOrca.txt",
                                     "11 May\nMellowOtter.txt",
                                     "17 May\nSwiftWren.txt"))
    )
  
  actor_freq <- all_chains |>
    dplyr::group_by(actor) |>
    dplyr::summarise(n_incidents = dplyr::n_distinct(date), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n_incidents), actor) |>
    dplyr::slice_head(n = top_n)
  
  core_six <- c("victoria_rigging","mia_fender","lily_anchorline",
                "john_windward","daniel_gangway","chloe_ballast")
  
  heatmap_data <- all_chains |>
    dplyr::filter(actor %in% actor_freq$actor) |>
    tidyr::complete(actor, date_label, fill = list(present = 0)) |>
    dplyr::left_join(actor_freq, by = "actor") |>
    dplyr::mutate(
      actor_label = stringr::str_replace_all(actor, "_", " ") |>
        stringr::str_to_title(),
      fill_label  = ifelse(present == 1, "Present", "Absent"),
      is_terminal = actor == TERMINAL_EXECUTOR,
      is_core     = actor %in% core_six,
      role_tag    = dplyr::case_when(
        is_terminal ~ "Terminal",
        is_core     ~ "Core",
        TRUE        ~ "Support"
      ),
      tip = paste0(
        "<b>", stringr::str_replace_all(actor, "_", " ") |>
          stringr::str_to_title(), "</b><br>",
        "Role: ", role_tag, "<br>",
        "Incidents present: ", n_incidents, " of 3<br>",
        "Status: ", fill_label,
        ifelse(is_terminal, "<br><b>\u26a1 Terminal Executor</b>", ""),
        ifelse(is_core & !is_terminal, "<br>Core relay actor", "")
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
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    # Role strip on the right
    ggplot2::geom_tile(
      ggplot2::aes(x = 3.75, fill = role_tag),
      width = 0.25, colour = "white", linewidth = 0.5,
      show.legend = FALSE
    ) +
    # Star for terminal executor
    ggplot2::geom_text(
      data    = dplyr::filter(heatmap_data, is_terminal & present == 1),
      mapping = ggplot2::aes(x = date_label,
                             y = reorder(actor_label, n_incidents),
                             label = "\u2605"),
      colour = "white", size = 5, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Present"  = "#4E79A7",
        "Absent"   = "#EEEEEE",
        "Terminal" = "#E15759",
        "Core"     = "#F28E2B",
        "Support"  = "#B0C4DE"
      ),
      breaks = c("Present", "Absent"),
      name   = "Participation"
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.5, 0.7))) +
    ggplot2::labs(
      title    = "Actor Participation Across Three Anomalous Incidents",
      subtitle = paste0(
        "\u2605 = John Windward (terminal executor, present in all 3)  \u2022  ",
        "Right strip: \u25a0 Terminal  \u25a0 Core relay  \u25a0 Support  \u2022  ",
        "Actors ordered by number of incidents"
      ),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle   = ggplot2::element_text(colour = "#666666", size = 9.5),
      panel.grid      = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.x     = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", y = -0.1))
}


# =============================================================================
# mod3_render_sequence — UPGRADED
# Lollipop / connected dot chart instead of plain scatter
# =============================================================================
mod3_render_sequence <- function() {
  
  seq_df <- tibble::tibble(
    incident = c(
      rep("10 May 2046\nHiddenOrca.txt", 4),
      rep("11 May 2046\nMellowOtter.txt", 4),
      rep("17 May 2046\nSwiftWren.txt", 4)
    ),
    step = rep(c(
      "Step 1\nsaidit_post_check",
      "Step 2\nsaidit_post \u26a1 INJECTION",
      "Step 3\ndelete_file",
      "Step 4\ndelete_file"
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
                         levels = c("10 May 2046\nHiddenOrca.txt",
                                    "11 May 2046\nMellowOtter.txt",
                                    "17 May 2046\nSwiftWren.txt")),
      step      = factor(step, levels = rev(c(
        "Step 1\nsaidit_post_check",
        "Step 2\nsaidit_post \u26a1 INJECTION",
        "Step 3\ndelete_file",
        "Step 4\ndelete_file"
      ))),
      is_injection = event_type == "saidit_post",
      tip = paste0(
        "<b>", stringr::str_replace(as.character(step), "\n", " — "), "</b><br>",
        "Incident: ", stringr::str_replace(as.character(incident), "\n", " | "), "<br>",
        "Elapsed from sequence start: +", elapsed, "s<br>",
        "Anchor time: ", anchor_time, "<br>",
        ifelse(is_injection,
               paste0("<br><b>\u26a1 Injected file: ", injected_file, "</b>"), "")
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
      text   = tip
    )
  ) +
    # Connecting lines between incidents (same step)
    ggplot2::geom_line(
      ggplot2::aes(group = step),
      colour = "#DDDDDD", linewidth = 1.5, linetype = "solid"
    ) +
    # Lollipop stems (vertical within each incident)
    ggplot2::geom_segment(
      data = seq_df |>
        dplyr::group_by(incident) |>
        dplyr::summarise(ymin = min(as.numeric(step)),
                         ymax = max(as.numeric(step)), .groups="drop"),
      ggplot2::aes(x = incident, xend = incident,
                   y = ymin, yend = ymax),
      colour = "#DDDDDD", linewidth = 1, inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = is_injection),
      alpha = 0.95, stroke = 0.5
    ) +
    # Timing labels inside points
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("+", elapsed, "s")),
      vjust = -1.4, size = 3.2, colour = "#555555",
      fontface = "bold", show.legend = FALSE
    ) +
    # Highlight injection step
    ggplot2::geom_point(
      data   = dplyr::filter(seq_df, is_injection),
      colour = ANOMALY_COLOUR, size = 22, alpha = 0.12,
      shape = 1, stroke = 1.2
    ) +
    ggplot2::scale_colour_manual(values = colour_map, name = "Event Type") +
    ggplot2::scale_size_manual(
      values = c("FALSE" = 8, "TRUE" = 14), guide = "none"
    ) +
    ggplot2::labs(
      title    = "Terminal Sequence — Identical Timing Across All 3 Incidents",
      subtitle = paste0(
        "4-event sequence at \u00b11s precision across 3 different times of day  \u2022  ",
        "Proves scripted automated protocol, not manual error  \u2022  ",
        "Horizontal lines show perfect timing alignment"
      ),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle      = ggplot2::element_text(colour = "#666666", size = 9.5),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#F0F0F0"),
      axis.text.x        = ggplot2::element_text(face = "bold", size = 11),
      axis.text.y        = ggplot2::element_text(size = 10),
      legend.position    = "bottom"
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", y = -0.12))
}


# =============================================================================
# mod3_render_intervention — unchanged structure, kept clean
# =============================================================================
mod3_render_intervention <- function(panel = "intervention_before") {
  
  diamond_df <- function(cx, cy, w, h, id) {
    tibble::tibble(
      x     = c(cx, cx + w/2, cx, cx - w/2),
      y     = c(cy + h/2, cy, cy - h/2, cy),
      group = id
    )
  }
  
  if (panel == "intervention_before") {
    
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
                        arrow = ggplot2::arrow(length = ggplot2::unit(3,"mm"), type="closed"),
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
                        arrow = ggplot2::arrow(length = ggplot2::unit(3,"mm"), type="closed"),
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
                        arrow = ggplot2::arrow(length = ggplot2::unit(3,"mm"), type="closed"),
                        color = "grey50") +
      ggplot2::annotate("rect",
                        xmin = 2.5, xmax = 7.5, ymin = 1.5, ymax = 2.6,
                        fill = "#555555", color = "white", linewidth = 0.5) +
      ggplot2::annotate("text", x = 5, y = 2.05,
                        label = "delete_file \u00d7 2  \u2014  Evidence destroyed",
                        color = "white", size = 3.8) +
      ggplot2::coord_cartesian(xlim = c(0.5, 9.5), ylim = c(1.0, 11.5),
                               expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))
    
  } else {
    
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
                        arrow = ggplot2::arrow(length = ggplot2::unit(3,"mm"), type="closed"),
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
                        arrow = ggplot2::arrow(length = ggplot2::unit(3,"mm"), type="closed"),
                        color = "#59A14F", linewidth = 1.2) +
      ggplot2::annotate("text", x = 2.4, y = 5.25,
                        label = "PASS\ncontent_source = NA",
                        color = "#59A14F", fontface = "bold", size = 3.2) +
      ggplot2::annotate("segment",
                        x = 5, xend = 7.8, y = 5.6, yend = 4.7,
                        arrow = ggplot2::arrow(length = ggplot2::unit(3,"mm"), type="closed"),
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
      ggplot2::coord_cartesian(xlim = c(0.5, 9.5), ylim = c(1.0, 11.5),
                               expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))
  }
}
