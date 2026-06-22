# =============================================================================
# server.R — TraceNet Shiny App
# Main server: sources module files and calls each module's server function.
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Source module files (loaded here so they share global.R environment)
  # ---------------------------------------------------------------------------


  # ---------------------------------------------------------------------------
  # MODULE 1 — System Topology & Event Reconstruction
  # ---------------------------------------------------------------------------

  # Reactive: filter mc2_df to selected incident date
  mod1_data <- reactive({
    if (input$mod1_incident == "all") {
      incident_events                               # all 3 incident dates
    } else {
      incident_events |>
        dplyr::filter(date == as.Date(input$mod1_incident))
    }
  })

  # Network plot
  output$mod1_network <- visNetwork::renderVisNetwork({
    mod1_render_network(
      data          = mod1_data(),
      network_mode  = input$mod1_network_mode,
      show_labels   = input$mod1_show_labels,
      selected_date = input$mod1_incident
    )
  })

  # Timeline plot
  output$mod1_timeline <- plotly::renderPlotly({
    mod1_render_timeline(data = mod1_data())
  })

  # ---------------------------------------------------------------------------
  # MODULE 2 — Content Source Anomaly Detection
  # ---------------------------------------------------------------------------

  # Panel title (reactive to view selection)
  output$mod2_panel_title <- renderText({
    switch(input$mod2_view,
           compare = "Normal vs Anomalous saidit_post Events",
           gate    = "Validation Gate Sequence",
           log     = "Full Event Log"
    )
  })
  
  output$mod2_main_plot <- renderUI({
    if (input$mod2_view == "log") {
      return(tags$div(
        style = "padding:20px; color:#666; font-size:1em;",
        tags$p(tags$b("Full Event Log — Chain Actors Only")),
        tags$p("Showing all events on the selected incident date involving the 9 confirmed relay chain actors."),
        tags$p("Use the table below to search, filter, and export.")
      ))
    }
    if (input$mod2_view == "gate") {
      # Static ggplot — avoids plotly tile stretching
      plotOutput("mod2_plot_gate", height = "380px")
    } else {
      plotly::plotlyOutput("mod2_plot_inner", height = "400px")
    }
  })
  
  # Plotly output — compare view only
  output$mod2_plot_inner <- plotly::renderPlotly({
    mod2_render_compare(
      normal_posts     = normal_posts,
      anomalous_events = anomalous_events
    )
  })
  
  # Static ggplot output — gate/spotlight view
  output$mod2_plot_gate <- renderPlot({
    mod2_render_gate(
      data          = incident_events,
      selected_date = as.Date(input$mod2_incident)
    )
  }, res = 110)
  
  # Event log DT table
  output$mod2_event_log <- DT::renderDataTable({
    mod2_render_log(
      data          = incident_events,
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
    ) |> dplyr::distinct(actor) |> nrow()
    
    updateSliderInput(session, "mod3_top_n",
                      max   = n_actors,
                      value = min(input$mod3_top_n, n_actors)
    )
  })
  
  # Panel title (reactive to view selection)
  output$mod3_panel_title <- renderText({
    switch(input$mod3_view,
           heatmap               = "Actor Participation Heatmap",
           sequence              = "Terminal Sequence Comparison Across Incidents",
           intervention_before   = "Current State: Validation Gate — Vulnerable",
           intervention_after    = "Proposed Fix: Validation Gate — With content_source Check"
    )
  })
  
  # Main plot area — swaps output type based on view
  output$mod3_main_plot <- renderUI({
    if (input$mod3_view %in% c("intervention_before", "intervention_after")) {
      # Static renderPlot for ggplot-only diagrams (no plotly wrapper)
      plotOutput("mod3_plot_static", height = "650px")
    } else {
      plotly::plotlyOutput("mod3_plot_inner", height = "500px")
    }
  })
  
  # Plotly output — heatmap and sequence only
  output$mod3_plot_inner <- plotly::renderPlotly({
    if (input$mod3_view == "heatmap") {
      mod3_render_heatmap(top_n = input$mod3_top_n)
    } else {
      mod3_render_sequence()
    }
  })
  
  # Static ggplot output — intervention views only
  output$mod3_plot_static <- renderPlot({
    mod3_render_intervention(panel = input$mod3_view)
  }, res = 110)

}
