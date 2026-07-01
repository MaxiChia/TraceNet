# =============================================================================
# modules/mod2_anomaly.R — VISUAL EXCELLENCE EDITION
# =============================================================================

extract_parties <- function(parties_col) {
  purrr::map_chr(parties_col, ~ paste(.x, collapse = " & "))
}

# -----------------------------------------------------------------------------
# mod2_render_compare
# All saidit_post events on a date x time-of-day scatter
# 3 anomalous ones highlighted — needle-in-haystack forensic story
# -----------------------------------------------------------------------------
mod2_render_compare <- function(normal_posts, anomalous_events) {
  
  all_posts <- dplyr::bind_rows(
    normal_posts     |> dplyr::mutate(status = "Normal"),
    anomalous_events |> dplyr::mutate(status = "Anomalous (Injected)")
  ) |>
    dplyr::mutate(
      time_of_day = as.numeric(format(datetime, "%H")) +
        as.numeric(format(datetime, "%M")) / 60,
      date_label  = format(date, "%d %b %Y"),
      actor       = purrr::map_chr(parties, ~ {
        x <- stringr::str_remove(.x[1], "Agent/person:")
        stringr::str_replace_all(x, "_", " ") |> stringr::str_to_title()
      }),
      tip = paste0(
        "<b>", status, "</b><br>",
        "Date: ", date_label, "<br>",
        "Time: ", format(datetime, "%H:%M:%S UTC"), "<br>",
        "Actor: ", actor,
        ifelse(!is.na(content_source),
               paste0("<br><b>\u26a1 Injected file: ", content_source, "</b>"), "")
      )
    )
  
  anomalous_df <- dplyr::filter(all_posts, status == "Anomalous (Injected)")
  
  p <- ggplot2::ggplot(
    all_posts,
    ggplot2::aes(
      x      = date,
      y      = time_of_day,
      colour = status,
      size   = status,
      alpha  = status,
      text   = tip
    )
  ) +
    ggplot2::geom_vline(
      xintercept = as.Date(c("2046-05-10", "2046-05-11", "2046-05-17")),
      colour = "#D84040", linetype = "dashed", linewidth = 0.5, alpha = 0.4
    ) +
    ggplot2::geom_jitter(width = 0.3, height = 0, stroke = 0) +
    ggplot2::geom_point(
      data    = anomalous_df,
      mapping = ggplot2::aes(x = date, y = time_of_day),
      colour = "#D84040", size = 18, alpha = 0.12,
      shape = 1, stroke = 1.2, inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data    = anomalous_df,
      mapping = ggplot2::aes(x = date, y = time_of_day, label = content_source),
      nudge_x = 1.5, hjust = 0, size = 3, colour = "#D84040",
      fontface = "bold", inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = c("Normal" = "#5DCAA5", "Anomalous (Injected)" = "#D84040"),
      name   = NULL
    ) +
    ggplot2::scale_size_manual(
      values = c("Normal" = 3, "Anomalous (Injected)" = 8),
      guide  = "none"
    ) +
    ggplot2::scale_alpha_manual(
      values = c("Normal" = 0.45, "Anomalous (Injected)" = 1),
      guide  = "none"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 24),
      breaks = c(0, 6, 12, 18, 24),
      labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
      name   = "Time of day (UTC)"
    ) +
    ggplot2::scale_x_date(date_labels = "%d %b", date_breaks = "7 days") +
    ggplot2::annotate("text", x = as.Date("2046-05-10"), y = 23,
                      label = "10 May", size = 3, colour = "#D84040",
                      hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", x = as.Date("2046-05-11"), y = 23,
                      label = "11 May", size = 3, colour = "#D84040",
                      hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", x = as.Date("2046-05-17"), y = 23,
                      label = "17 May", size = 3, colour = "#D84040",
                      hjust = 0.5, fontface = "bold") +
    ggplot2::labs(
      title    = "All saidit_post Events \u2014 Locating the 3 Anomalous Injections",
      subtitle = paste0(
        "108 total saidit_post events  \u2022  ",
        "3 anomalous (red) carry content_source \u2260 NULL  \u2022  ",
        "Dashed lines = confirmed incident dates"
      ),
      x = "Date"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 13, colour = "#E8EAEA"),
      plot.subtitle      = ggplot2::element_text(colour = "#AAB7BA", size = 9.5),
      legend.position    = "bottom",
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#243D47"),
      panel.grid.major.y = ggplot2::element_line(colour = "#1A2E35"),
      plot.background    = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      panel.background   = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.background  = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.key         = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      legend.text        = ggplot2::element_text(colour = "#E8EAEA"),
      axis.text          = ggplot2::element_text(colour = "#E8EAEA"),
      axis.title         = ggplot2::element_text(colour = "#AAB7BA")
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      paper_bgcolor = "#0B1418",
      plot_bgcolor  = "#0B1418",
      font = list(color = "#E8EAEA"),
      legend = list(orientation = "h", y = -0.12, font = list(color = "#E8EAEA"))
    )
}


# -----------------------------------------------------------------------------
# mod2_render_gate
# Static ggplot — vertical step sequence with coloured boxes and arrows
# Returns ggplot object (rendered via renderPlot in server.R, NOT renderPlotly)
# -----------------------------------------------------------------------------
mod2_render_gate <- function(data, selected_date) {
  
  anchor <- data |>
    dplyr::filter(
      date       == selected_date,
      short_name == "saidit_post",
      !is.na(content_source)
    )
  
  if (nrow(anchor) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No anomalous saidit_post found for this date.",
                          size = 5, colour = "#888888") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#0B1418", colour = NA))
    )
  }
  
  anchor_time   <- anchor$datetime[1]
  injected_file <- anchor$content_source[1]
  
  plot_df <- tibble::tibble(
    step       = 4:1,
    label_main = c(
      "saidit_post_check",
      "saidit_post  \u26a1 INJECTION",
      "delete_file  (\u00d71)",
      "delete_file  (\u00d72)"
    ),
    label_sub  = c(
      "Validation gate fires \u2014 does NOT check content_source",
      paste0("Content source: ", injected_file, "  |  file = NA"),
      "Evidence removal begins",
      "All traces destroyed"
    ),
    timing     = c("+0s", "+1s", "+2s", "+3s"),
    fill       = c("#1D9E75", "#D84040", "#3d5a63", "#3d5a63"),
    is_inject  = c(FALSE, TRUE, FALSE, FALSE)
  )
  
  ggplot2::ggplot(plot_df) +
    ggplot2::geom_tile(
      ggplot2::aes(x = 5, y = step, fill = fill),
      width = 7.5, height = 0.6, colour = "white", linewidth = 1.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_label(
      ggplot2::aes(x = 0.7, y = step, label = timing),
      size = 4.5, fontface = "bold", colour = "#444444",
      fill = "#F5F5F5", linewidth = 0.3,
      label.padding = ggplot2::unit(0.35, "lines")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 5, y = step + 0.12, label = label_main,
                   colour = is_inject),
      size = 3.8, fontface = "bold"
    ) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "white", "TRUE" = "#FFE0B2"),
      guide  = "none"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 5, y = step - 0.15, label = label_sub),
      size = 3, colour = "white", alpha = 0.85
    ) +
    ggplot2::geom_segment(
      data = data.frame(y_start = c(3.65, 2.65, 1.65)),
      ggplot2::aes(x = 5, xend = 5, y = y_start, yend = y_start - 0.3),
      arrow = ggplot2::arrow(length = ggplot2::unit(4, "mm"), type = "closed"),
      colour = "#BBBBBB", linewidth = 1
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 9.5), ylim = c(0.55, 4.55),
                             expand = FALSE) +
    ggplot2::labs(
      title    = paste0("Injection Spotlight \u2014 ", format(selected_date, "%d %B %Y")),
      subtitle = paste0(
        "Injected file: ", injected_file,
        "  \u2022  Anchor: ", format(anchor_time, "%H:%M:%S UTC"),
        "  \u2022  4-event sequence completes in 3 seconds"
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0B1418", colour = NA),
      plot.title    = ggplot2::element_text(face = "bold", size = 13,
                                            margin = ggplot2::margin(b = 4),
                                            colour = "#E8EAEA"),
      plot.subtitle = ggplot2::element_text(colour = "#FFB4B4",
                                            face = "bold", size = 10,
                                            margin = ggplot2::margin(b = 12)),
      plot.margin   = ggplot2::margin(16, 24, 16, 24)
    )
}


# -----------------------------------------------------------------------------
# mod2_render_log
# DT event log — all chain actor events for selected incident date
# -----------------------------------------------------------------------------
mod2_render_log <- function(data, selected_date) {
  
  log_df <- data |>
    dplyr::filter(
      date == selected_date,
      short_name %in% c("queue_subordinate_task", "saidit_post_check",
                        "saidit_post", "delete_file", "read_file",
                        "create_file", "access_files"),
      purrr::map_lgl(parties, ~ any(stringr::str_detect(
        .x, paste(c("john_windward", "chloe_ballast", "daniel_gangway",
                    "lily_anchorline", "mia_fender", "victoria_rigging",
                    "zoey_drydock", "gabriel_sonar", "james_stern"),
                  collapse = "|"))))
    ) |>
    dplyr::arrange(datetime) |>
    dplyr::mutate(Actors = extract_parties(parties)) |>
    dplyr::select(
      Time       = datetime,
      Event      = short_name,
      Actors,
      File       = event_file,
      ContentSrc = content_source,
      ID         = id
    ) |>
    dplyr::mutate(Time = format(Time, "%H:%M:%S"))
  
  DT::datatable(
    log_df,
    options = list(
      pageLength = 15,
      scrollX    = TRUE,
      dom        = "Bfrtip",
      buttons    = c("csv", "excel"),
      order      = list(list(0, "asc"))
    ),
    rownames   = FALSE,
    filter     = "top",
    class      = "display compact",
    extensions = "Buttons"
  ) |>
    DT::formatStyle(
      "Event",
      backgroundColor = DT::styleEqual(
        c("saidit_post", "saidit_post_check", "delete_file"),
        c(ANOMALY_COLOUR, "#1D9E75", "#3d5a63")
      ),
      color = DT::styleEqual(
        c("saidit_post", "saidit_post_check", "delete_file"),
        c("white", "white", "white")
      ),
      fontWeight = "bold"
    ) |>
    DT::formatStyle(
      "ContentSrc",
      color      = ANOMALY_COLOUR,
      fontWeight = "bold"
    )
}