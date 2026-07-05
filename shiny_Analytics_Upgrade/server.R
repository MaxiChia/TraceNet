# =============================================================================
# server.R — TraceNet Shiny App
# Main server: preserves existing output IDs, adds safe guards for optional modules.
# =============================================================================

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # OPTIONAL INVESTIGATION FLOW
  # The visual-upgrade UI uses a static Demo Story tab, so this module is now
  # optional. This guard prevents deployment failure if mod_investigation.R is
  # not included in a reviewed/uploaded bundle.
  # ---------------------------------------------------------------------------
  if (
    exists("N_STEPS") &&
    exists("investigation_step_ui") &&
    exists("inv_render_step1") &&
    exists("inv_render_step2") &&
    exists("inv_render_step3") &&
    exists("inv_render_step4") &&
    exists("inv_render_step5")
  ) {
    inv_step <- reactiveVal(1)
    
    observeEvent(input$inv_next, {
      if (inv_step() < N_STEPS) inv_step(inv_step() + 1)
    }, ignoreInit = TRUE)
    
    observeEvent(input$inv_prev, {
      if (inv_step() > 1) inv_step(inv_step() - 1)
    }, ignoreInit = TRUE)
    
    output$inv_step_ui <- renderUI({ investigation_step_ui(inv_step()) })
    
    output$inv_plot <- plotly::renderPlotly({
      s <- inv_step()
      if (s == 1) inv_render_step1()
      else if (s == 3) inv_render_step3()
      else if (s == 4) inv_render_step4()
      else plotly::plot_ly()
    })
    
    output$inv_network <- visNetwork::renderVisNetwork({ inv_render_step2() })
    output$inv_plot_static <- renderPlot({ inv_render_step5() }, res = 110)
  }
  
  # ---------------------------------------------------------------------------
  # MODULE 1 — Focus actor connection summary
  # ---------------------------------------------------------------------------
  output$mod1_focus_actor_summary <- renderUI({
    
    req(input$mod1_focus_actor)
    
    if (input$mod1_focus_actor == "all") {
      return(tags$div(
        class = "tn-visual-note",
        tags$div(class = "tn-visual-note-icon", bsicons::bs_icon("person-lines-fill")),
        tags$div(
          tags$p(class = "tn-visual-note-title", "Focus Actor Summary"),
          tags$p(
            class = "tn-visual-note-body",
            "Select a focus actor from the sidebar to inspect that actor's role, incoming links, outgoing links, and selected centrality score."
          )
        )
      ))
    }
    
    actor_id <- input$mod1_focus_actor
    actor_clean <- stringr::str_remove(actor_id, "Agent/person:")
    
    actor_metrics <- network_metrics |>
      dplyr::mutate(
        actor_clean = stringr::str_to_lower(stringr::str_replace_all(actor, " ", "_"))
      ) |>
      dplyr::filter(actor_clean == !!actor_clean)
    
    if (nrow(actor_metrics) == 0) {
      return(tags$div(
        class = "tn-visual-note tn-visual-note-danger",
        tags$div(class = "tn-visual-note-icon", bsicons::bs_icon("exclamation-triangle")),
        tags$div(
          tags$p(class = "tn-visual-note-title", "No Metrics Found"),
          tags$p(
            class = "tn-visual-note-body",
            paste("No centrality metrics were found for", actor_clean)
          )
        )
      ))
    }
    
    metric <- input$mod1_centrality_metric
    
    role_text <- actor_metrics$role[1]
    degree_value <- actor_metrics$degree[1]
    in_value <- actor_metrics$in_degree[1]
    out_value <- actor_metrics$out_degree[1]
    selected_value <- actor_metrics[[metric]][1]
    
    tags$div(
      class = "tn-focus-summary",
      tags$div(
        class = "tn-focus-summary-header",
        tags$p(class = "tn-card-eyebrow", "Selected Focus Actor"),
        tags$h3(stringr::str_to_title(stringr::str_replace_all(actor_clean, "_", " ")))
      ),
      tags$div(
        class = "tn-focus-metric-grid",
        
        tags$div(
          class = "tn-focus-metric",
          tags$span("Role"),
          tags$strong(role_text)
        ),
        
        tags$div(
          class = "tn-focus-metric",
          tags$span("Total Connections"),
          tags$strong(round(degree_value, 3))
        ),
        
        tags$div(
          class = "tn-focus-metric",
          tags$span("Incoming Links"),
          tags$strong(round(in_value, 3))
        ),
        
        tags$div(
          class = "tn-focus-metric",
          tags$span("Outgoing Links"),
          tags$strong(round(out_value, 3))
        ),
        
        tags$div(
          class = "tn-focus-metric",
          tags$span(paste("Selected Metric:", metric)),
          tags$strong(round(selected_value, 4))
        )
      )
    )
  })
  
  # ---------------------------------------------------------------------------
  # MODULE 1 — Network centrality ranking table
  # ---------------------------------------------------------------------------
  output$mod1_centrality_table <- DT::renderDT({
    
    req(network_metrics)
    req(input$mod1_centrality_metric)
    req(input$mod1_top_n_centrality)
    
    metric <- input$mod1_centrality_metric
    
    table_df <- network_metrics |>
      dplyr::arrange(dplyr::desc(.data[[metric]])) |>
      dplyr::slice_head(n = input$mod1_top_n_centrality) |>
      dplyr::mutate(
        Rank = dplyr::row_number(),
        Actor = actor,
        Role = role,
        Degree = round(degree, 3),
        `In-degree` = round(in_degree, 3),
        `Out-degree` = round(out_degree, 3),
        Betweenness = round(betweenness, 3),
        Closeness = round(closeness, 3),
        Eigenvector = round(eigenvector, 3)
      ) |>
      dplyr::select(
        Rank, Actor, Role, Degree, `In-degree`, `Out-degree`,
        Betweenness, Closeness, Eigenvector
      )
    
    DT::datatable(
      table_df,
      rownames = FALSE,
      options = list(
        pageLength = nrow(table_df),
        dom = "t",
        scrollX = TRUE,
        scrollY = "300px",
        paging = FALSE
      )
    )
  })
  
  output$mod1_centrality_chart <- renderPlot({
    req(input$mod1_centrality_metric)
    req(input$mod1_top_n_centrality)
    mod1_render_centrality_chart(
      metric     = input$mod1_centrality_metric,
      metrics_df = network_metrics,
      top_n      = as.integer(input$mod1_top_n_centrality)
    )
  }, res = 110, bg = "#FFFFFF")
  
  # ---------------------------------------------------------------------------
  # MODULE 1 — System Topology & Event Reconstruction
  # ---------------------------------------------------------------------------
  mod1_data <- reactive({
    if (input$mod1_incident == "all") {
      incident_events
    } else {
      incident_events |>
        dplyr::filter(date == as.Date(input$mod1_incident))
    }
  })
  
  output$mod1_network <- visNetwork::renderVisNetwork({
    t0 <- Sys.time()
    on.exit({
      cat("Module 1 network render time:", 
          round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
          "seconds\n")
    })
    
    mod1_render_network(
      data               = mod1_data(),
      network_mode       = input$mod1_network_mode,
      show_labels        = input$mod1_show_labels,
      selected_date      = input$mod1_incident,
      centrality_metric  = input$mod1_centrality_metric,
      size_by_centrality = input$mod1_size_by_centrality,
      focus_actor        = input$mod1_focus_actor
    )
  })
  
  # Cache Module 1 timeline plots — served from global.R pre-computed cache
  output$mod1_timeline <- plotly::renderPlotly({
    timeline_cache[[input$mod1_incident]]
  })
  
  # ---------------------------------------------------------------------------
  # MODULE 2 — Content Source Anomaly Detection
  # ---------------------------------------------------------------------------
  output$mod2_panel_title <- renderText({
    switch(input$mod2_view,
           compare = "Normal vs Anomalous saidit_post Events",
           gate    = "Validation Gate Sequence",
           log     = "Full Event Log"
    )
  })
  
  # ---------------------------------------------------------------------------
  # MODULE 2 — Anomaly summary metrics
  # ---------------------------------------------------------------------------
  output$mod2_anomaly_summary <- renderUI({
    
    total_posts <- nrow(normal_posts) + nrow(anomalous_events)
    normal_count <- nrow(normal_posts)
    anomaly_count <- nrow(anomalous_events)
    
    anomaly_rate <- ifelse(
      total_posts > 0,
      anomaly_count / total_posts * 100,
      0
    )
    
    affected_dates <- anomalous_events |>
      dplyr::distinct(date) |>
      dplyr::arrange(date) |>
      dplyr::pull(date) |>
      format("%d %b") |>
      paste(collapse = " · ")
    
    injected_files <- anomalous_events |>
      dplyr::filter(!is.na(content_source)) |>
      dplyr::distinct(content_source) |>
      dplyr::pull(content_source) |>
      stringr::str_remove("\\.txt$") |>
      paste(collapse = " · ")
    
    terminal_actor <- "john_windward"
    
    tags$div(
      class = "tn-mod2-summary",
      
      tags$div(
        class = "tn-mod2-summary-card",
        tags$span("saidit_post"),
        tags$strong(format(total_posts, big.mark = ","))
      ),
      
      tags$div(
        class = "tn-mod2-summary-card",
        tags$span("Normal"),
        tags$strong(format(normal_count, big.mark = ","))
      ),
      
      tags$div(
        class = "tn-mod2-summary-card tn-mod2-summary-danger",
        tags$span("Anomalous"),
        tags$strong(anomaly_count)
      ),
      
      tags$div(
        class = "tn-mod2-summary-card",
        tags$span("Anomaly rate"),
        tags$strong(paste0(round(anomaly_rate, 4), "%"))
      ),
      
      tags$div(
        class = "tn-mod2-summary-card",
        tags$span("Affected dates"),
        tags$strong(affected_dates)
      ),
      
      tags$div(
        class = "tn-mod2-summary-card tn-mod2-summary-danger",
        tags$span("Injected files"),
        tags$strong(injected_files)
      ),
      
      tags$div(
        class = "tn-mod2-summary-card",
        tags$span("Terminal executor"),
        tags$strong(terminal_actor)
      )
    )
  })
  
  # ---------------------------------------------------------------------------
  # MODULE 2 — Compact plot legend / interpretation guide
  # ---------------------------------------------------------------------------
  output$mod2_plot_legend <- renderUI({
    
    if (input$mod2_view == "compare") {
      return(tags$div(
        class = "tn-mod2-legend",
        tags$span(class = "legend-dot-small legend-green"),
        tags$span("Normal saidit_post"),
        tags$span(class = "legend-dot-small legend-red"),
        tags$span("Anomalous saidit_post with content_source"),
        tags$span(class = "legend-line-small legend-red-line"),
        tags$span("Confirmed incident date"),
        tags$span(class = "legend-ring-small"),
        tags$span("Injected post highlight")
      ))
    }
    
    if (input$mod2_view == "gate") {
      return(tags$div(
        class = "tn-mod2-legend",
        tags$span(class = "legend-dot-small legend-gold"),
        tags$span("saidit_post_check validation gate"),
        tags$span(class = "legend-dot-small legend-red"),
        tags$span("Injected saidit_post"),
        tags$span(class = "legend-dot-small legend-grey"),
        tags$span("delete_file evidence removal"),
        tags$span(class = "legend-arrow-small", "→"),
        tags$span("Event sequence direction")
      ))
    }
    
    if (input$mod2_view == "log") {
      return(tags$div(
        class = "tn-mod2-legend",
        tags$span(class = "legend-dot-small legend-red"),
        tags$span("Rows with content_source populated are anomalous"),
        tags$span(class = "legend-dot-small legend-gold"),
        tags$span("Use search box to inspect actor, event type, or injected file")
      ))
    }
  })
  
  output$mod2_plot_inner <- plotly::renderPlotly({
    t0 <- Sys.time()
    on.exit({
      cat("Module 2 compare plot render time:",
          round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
          "seconds\n")
    })
    
    mod2_render_compare(
      normal_posts     = normal_posts,
      anomalous_events = anomalous_events
    )
  })
  
  output$mod2_plot_gate <- renderPlot({
    t0 <- Sys.time()
    on.exit({
      cat("Module 2 gate plot render time:",
          round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
          "seconds | incident:", input$mod2_incident,
          "\n")
    })
    
    mod2_render_gate(
      data          = incident_events,
      selected_date = as.Date(input$mod2_incident)
    )
  }, res = 120, bg = "#FFFFFF")
  
  # Cache Module 2 event-log data by incident date
  mod2_event_log_data_cache <- new.env(parent = emptyenv())
  
  output$mod2_event_log <- DT::renderDataTable({
    t0 <- Sys.time()
    on.exit({
      cat("Module 2 event log render time:",
          round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
          "seconds | incident:", input$mod2_incident,
          "\n")
    })
    
    cache_key <- input$mod2_incident
    
    if (!exists(cache_key, envir = mod2_event_log_data_cache, inherits = FALSE)) {
      log_data <- incident_events |>
        dplyr::filter(date == as.Date(input$mod2_incident))
      
      assign(cache_key, log_data, envir = mod2_event_log_data_cache)
    }
    
    cached_log_data <- get(
      cache_key,
      envir = mod2_event_log_data_cache,
      inherits = FALSE
    )
    
    mod2_render_log(
      data          = cached_log_data,
      selected_date = as.Date(input$mod2_incident)
    )
  })
  
  # ---------------------------------------------------------------------------
  # MODULE 3 — Historical Patterns & Intervention
  # ---------------------------------------------------------------------------
  observe({
    n_actors <- dplyr::bind_rows(
      get_chain_actors_per_date("2046-05-10", "2046-05-10 12:45:40"),
      get_chain_actors_per_date("2046-05-11", "2046-05-11 00:56:03"),
      get_chain_actors_per_date("2046-05-17", "2046-05-17 11:21:13")
    ) |>
      dplyr::distinct(actor) |>
      nrow()
    
    top_n_choices <- as.character(seq(5, n_actors))
    
    updateSelectInput(
      session,
      "mod3_top_n",
      choices = top_n_choices,
      selected = as.character(min(as.integer(input$mod3_top_n), n_actors))
    )
  })
  
  output$mod3_incident_filter_ui <- renderUI({
    
    if (
      input$mod3_view != "heatmap" ||
      input$mod3_heatmap_mode == "confirmed"
    ) {
      return(NULL)
    }
    
    selectInput(
      "mod3_incident_filter",
      "Incident Filter",
      choices = c(
        "All 3 confirmed incidents" = "all",
        "10 May — HiddenOrca.txt" = "2046-05-10",
        "11 May — MellowOtter.txt" = "2046-05-11",
        "17 May — SwiftWren.txt" = "2046-05-17"
      ),
      selected = "all"
    )
    
  })
  
  output$mod3_panel_title <- renderText({
    switch(input$mod3_view,
           heatmap = ifelse(
             input$mod3_heatmap_mode == "behaviour",
             "Actor Behaviour Heatmap",
             "Actor Recurrence Heatmap"
           ),
           sequence            = "Terminal Sequence Comparison Across Incidents",
           intervention_before = "Current State: Validation Gate — Vulnerable",
           intervention_after  = "Proposed Fix: Validation Gate — With content_source Check",
           activity            = "System Activity Baseline — When Is the System Most Active?"
    )
  })
  
  output$mod3_main_plot <- renderUI({
    t0 <- Sys.time()
    on.exit({
      cat("Module 3 main plot render time:",
          round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
          "seconds | view:", input$mod3_view,
          "| heatmap_mode:", input$mod3_heatmap_mode,
          "| incident_filter:", input$mod3_incident_filter,
          "| top_n:", as.integer(input$mod3_top_n),
          "\n")
    })
    
    if (input$mod3_view %in% c("intervention_before", "intervention_after")) {
      return(tags$div(
        class = "tn-intervention-plot-frame",
        plotOutput("mod3_plot_static", height = "calc(100vh - 165px)", width = "100%")
      ))
    }
    
    if (input$mod3_view == "activity") {
      return(plotly::plotlyOutput("mod3_activity_heatmap", height = "calc(100vh - 170px)"))
    }
    
    if (input$mod3_view == "heatmap") {
      
      heatmap_note <- if (input$mod3_heatmap_mode == "behaviour") {
        tags$div(
          class = "tn-visual-note tn-visual-note-warning",
          tags$div(class = "tn-visual-note-icon", bsicons::bs_icon("bar-chart-fill")),
          tags$div(
            tags$p(class = "tn-visual-note-title", "What this heatmap shows"),
            tags$p(
              class = "tn-visual-note-body",
              "This view shifts from actor presence to actor behaviour. It shows which recurring incident actors performed suspicious actions such as task delegation, file access, post validation, injected SaidIT posting, and evidence deletion."
            )
          )
        )
      } else {
        tags$div(
          class = "tn-visual-note tn-visual-note-info",
          tags$div(class = "tn-visual-note-icon", bsicons::bs_icon("person-lines-fill")),
          tags$div(
            tags$p(class = "tn-visual-note-title", "What this heatmap shows"),
            tags$p(
              class = "tn-visual-note-body",
              "This view shows which actors recur across the three confirmed anomalous incidents. It establishes repeated participation before the behaviour and sequence views explain what they did."
            )
          )
        )
      }
      
      return(tags$div(
        heatmap_note,
        plotly::plotlyOutput("mod3_plot_inner", height = "calc(100vh - 230px)")
      ))
    }
    
    plotly::plotlyOutput("mod3_plot_inner", height = "calc(100vh - 170px)")
  })
  
  # Cache Module 3 plotly heatmaps so comparison view does not recompute repeatedly
  mod3_heatmap_plot_cache <- new.env(parent = emptyenv())
  
  output$mod3_plot_inner <- plotly::renderPlotly({
    
    if (input$mod3_view == "heatmap") {
      
      cache_key <- paste(
        input$mod3_heatmap_mode,
        input$mod3_incident_filter,
        input$mod3_top_n,
        sep = "__"
      )
      
      if (!exists(cache_key, envir = mod3_heatmap_plot_cache, inherits = FALSE)) {
        heatmap_plot <- mod3_render_heatmap(
          top_n          = as.integer(input$mod3_top_n),
          heatmap_mode   = input$mod3_heatmap_mode,
          incident_filter = input$mod3_incident_filter
        )
        
        assign(
          cache_key,
          heatmap_plot,
          envir = mod3_heatmap_plot_cache
        )
      }
      
      return(get(
        cache_key,
        envir = mod3_heatmap_plot_cache,
        inherits = FALSE
      ))
    }
    
    mod3_render_sequence()
  })
  
  output$mod3_plot_static <- renderPlot({
    mod3_render_intervention(panel = input$mod3_view)
  }, res = 120, bg = "#FFFFFF")
  
  # ---------------------------------------------------------------------------
  # MODULE 3 — System Activity Baseline heatmap
  # MC2 Task 3a: "map out the activity and develop a baseline"
  # Lesson 8: Visualising and Analysing Time-Oriented Data
  # ---------------------------------------------------------------------------
  output$mod3_activity_heatmap <- plotly::renderPlotly({
    event_filter <- if (!is.null(input$mod3_activity_event_type) &&
                        nchar(input$mod3_activity_event_type) > 0) {
      input$mod3_activity_event_type
    } else {
      "all"
    }
    top_n <- if (!is.null(input$mod3_activity_top_n)) {
      as.integer(input$mod3_activity_top_n)
    } else {
      12L
    }
    mod3_render_activity_heatmap(
      data              = mc2_df,
      exclude_incidents = isTRUE(input$mod3_exclude_incidents),
      event_type_filter = event_filter,
      relay_only        = isTRUE(input$mod3_relay_only),
      top_n             = top_n
    )
  })
}
