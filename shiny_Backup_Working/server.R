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
    mod1_render_network(
      data          = mod1_data(),
      network_mode  = input$mod1_network_mode,
      show_labels   = input$mod1_show_labels,
      selected_date = input$mod1_incident
    )
  })
  
  output$mod1_timeline <- plotly::renderPlotly({
    mod1_render_timeline(data = mod1_data(), selected_date = input$mod1_incident)
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
  
  output$mod2_main_plot <- renderUI({
    if (input$mod2_view == "log") {
      return(tags$div(
        class = "tn-visual-note tn-visual-note-danger",
        tags$div(class = "tn-visual-note-icon", bsicons::bs_icon("table")),
        tags$div(
          tags$p(class = "tn-visual-note-title", "Full Event Log — Chain Actors Only"),
          tags$p(class = "tn-visual-note-body", "Showing all events on the selected incident date involving the confirmed relay-chain actors. Use the table below to search, filter, and export.")
        )
      ))
    }
    
    if (input$mod2_view == "gate") {
      plotOutput("mod2_plot_gate", height = "calc(100vh - 170px)")
    } else {
      plotly::plotlyOutput("mod2_plot_inner", height = "calc(100vh - 170px)")
    }
  })
  
  output$mod2_plot_inner <- plotly::renderPlotly({
    mod2_render_compare(
      normal_posts     = normal_posts,
      anomalous_events = anomalous_events
    )
  })
  
  output$mod2_plot_gate <- renderPlot({
    mod2_render_gate(
      data          = incident_events,
      selected_date = as.Date(input$mod2_incident)
    )
  }, res = 120)
  
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
    ) |>
      dplyr::distinct(actor) |>
      nrow()
    
    updateSliderInput(session, "mod3_top_n",
                      max   = n_actors,
                      value = min(input$mod3_top_n, n_actors)
    )
  })
  
  output$mod3_panel_title <- renderText({
    switch(input$mod3_view,
           heatmap             = "Actor Participation Heatmap",
           sequence            = "Terminal Sequence Comparison Across Incidents",
           intervention_before = "Current State: Validation Gate — Vulnerable",
           intervention_after  = "Proposed Fix: Validation Gate — With content_source Check"
    )
  })
  
  output$mod3_main_plot <- renderUI({
    if (input$mod3_view %in% c("intervention_before", "intervention_after")) {
      tags$div(
        class = "tn-intervention-plot-frame",
        plotOutput("mod3_plot_static", height = "calc(100vh - 165px)", width = "100%")
      )
    } else {
      plotly::plotlyOutput("mod3_plot_inner", height = "calc(100vh - 170px)")
    }
  })
  
  output$mod3_plot_inner <- plotly::renderPlotly({
    if (input$mod3_view == "heatmap") {
      mod3_render_heatmap(top_n = input$mod3_top_n)
    } else {
      mod3_render_sequence()
    }
  })
  
  output$mod3_plot_static <- renderPlot({
    mod3_render_intervention(panel = input$mod3_view)
  }, res = 120, bg = "#0B1418")
}
