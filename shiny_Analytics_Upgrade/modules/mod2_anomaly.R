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
               paste0("<br><b>⚡ Injected file: ", content_source, "</b>"), "")
      )
    )
  
  normal_df    <- dplyr::filter(all_posts, status == "Normal")
  anomalous_df <- dplyr::filter(all_posts, status == "Anomalous (Injected)")
  incident_dates <- as.Date(c("2046-05-10", "2046-05-11", "2046-05-17"))
  
  x_range <- c(min(all_posts$date, na.rm = TRUE) - 2,
               max(all_posts$date, na.rm = TRUE) + 2)
  
  vline_shapes <- purrr::map(incident_dates, ~ list(
    type = "line",
    xref = "x", yref = "y",
    x0 = .x, x1 = .x,
    y0 = 0, y1 = 24,
    line = list(color = "rgba(216,64,64,0.58)", width = 2, dash = "dash")
  ))
  
  # Date labels are intentionally nudged so 10 May and 11 May remain readable
  # even when the plot is embedded as a narrow screenshot in the poster.
  incident_label_df <- tibble::tibble(
    date   = incident_dates,
    label  = c("10 May", "11 May", "17 May"),
    xshift = c(-34, 34, 0),
    y      = c(23.35, 22.45, 23.35)
  )
  
  date_annotations <- purrr::pmap(
    list(incident_label_df$date,
         incident_label_df$label,
         incident_label_df$xshift,
         incident_label_df$y),
    function(date, label, xshift, y) {
      list(
        x = date, y = y,
        xref = "x", yref = "y",
        text = label,
        showarrow = FALSE,
        xanchor = "center",
        xshift = xshift,
        font = list(color = "#D84040", size = 16),
        bgcolor = "rgba(255,255,255,0.92)",
        bordercolor = "rgba(216,64,64,0.35)",
        borderpad = 1
      )
    }
  )
  
  file_annotations <- purrr::map(seq_len(nrow(anomalous_df)), function(i) {
    list(
      x = anomalous_df$date[i],
      y = anomalous_df$time_of_day[i],
      xref = "x", yref = "y",
      text = anomalous_df$content_source[i],
      showarrow = FALSE,
      xanchor = "left",
      xshift = 28,
      yshift = ifelse(anomalous_df$time_of_day[i] < 3, 4, 0),
      font = list(color = "#D84040", size = 15),
      bgcolor = "rgba(255,255,255,0.92)",
      bordercolor = "rgba(216,64,64,0.35)",
      borderpad = 2
    )
  })
  
  plotly::plot_ly(source = "mod2_compare") |>
    plotly::add_markers(
      data = normal_df,
      x = ~date, y = ~time_of_day,
      customdata = ~tip,
      hovertemplate = "%{customdata}<extra></extra>",
      name = "Normal",
      marker = list(
        color = "#5DCAA5", size = 11, opacity = 0.55,
        line = list(width = 0)
      )
    ) |>
    plotly::add_markers(
      data = anomalous_df,
      x = ~date, y = ~time_of_day,
      hoverinfo = "skip",
      showlegend = FALSE,
      marker = list(
        color = "rgba(216,64,64,0)", size = 58, opacity = 1,
        line = list(color = "rgba(216,64,64,0.28)", width = 4)
      )
    ) |>
    plotly::add_markers(
      data = anomalous_df,
      x = ~date, y = ~time_of_day,
      customdata = ~tip,
      hovertemplate = "%{customdata}<extra></extra>",
      name = "Anomalous (Injected)",
      marker = list(
        color = "#D84040", size = 24, opacity = 1,
        line = list(color = "#D84040", width = 1)
      )
    ) |>
    plotly::layout(
      autosize = TRUE,
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(color = "#1E293B", family = "Inter"),
      title = list(
        text = paste0(
          "<b>All saidit_post Events — Locating the 3 Anomalous Injections</b>",
          "<br><sup>108 total saidit_post events • 3 anomalous carry content_source ≠ NULL • dashed lines = confirmed incident dates</sup>"
        ),
        x = 0.03, xanchor = "left",
        font = list(size = 23, color = "#1E293B")
      ),
      margin = list(l = 78, r = 24, t = 78, b = 72),
      shapes = vline_shapes,
      annotations = c(date_annotations, file_annotations),
      legend = list(
        orientation = "h",
        x = 0.36, y = -0.16,
        bgcolor = "rgba(255,255,255,0)",
        font = list(color = "#1E293B", size = 15)
      ),
      xaxis = list(
        title = list(text = "Date", font = list(color = "#64748B", size = 18)),
        type = "date",
        range = x_range,
        tickformat = "%d %b",
        dtick = 7 * 24 * 60 * 60 * 1000,
        gridcolor = "#E2E8F0",
        zeroline = FALSE,
        tickfont = list(color = "#1E293B", size = 15),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = "Time of day (UTC)", font = list(color = "#64748B", size = 18)),
        range = c(0, 24),
        tickvals = c(0, 6, 12, 18, 24),
        ticktext = c("00:00", "06:00", "12:00", "18:00", "24:00"),
        gridcolor = "#E2E8F0",
        zeroline = FALSE,
        tickfont = list(color = "#1E293B", size = 15),
        automargin = TRUE
      )
    ) |>
    plotly::config(responsive = TRUE, displaylogo = FALSE)
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
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA))
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
      size = 5.2, fontface = "bold", colour = "#444444",
      fill = "#F5F5F5", linewidth = 0.3,
      label.padding = ggplot2::unit(0.35, "lines")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 5, y = step + 0.12, label = label_main,
                   colour = is_inject),
      size = 4.4, fontface = "bold"
    ) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "white", "TRUE" = "#FFE0B2"),
      guide  = "none"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 5, y = step - 0.15, label = label_sub),
      size = 3.8, colour = "white", alpha = 0.85
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
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      plot.title    = ggplot2::element_text(face = "bold", size = 16,
                                            margin = ggplot2::margin(b = 4),
                                            colour = "#1E293B"),
      plot.subtitle = ggplot2::element_text(colour = "#D84040",
                                            face = "bold", size = 12,
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