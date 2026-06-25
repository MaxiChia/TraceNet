# =============================================================================
# ui.R — TraceNet Shiny App
# Dashboard shell with Overview + three module tab panels.
# =============================================================================

ui <- bslib::page_navbar(
  title = div(
    "TraceNet"
  ),
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "darkly",
    bg         = "#0f1d22",
    fg         = "#e8eaea",
    primary    = "#5DCAA5",
    secondary  = "#1a2e35",
    success    = "#1D9E75",
    info       = "#E8C547",
    warning    = "#E8A52B",
    danger     = "#D84040",
    base_font  = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  
  # ---------------------------------------------------------------------------
  # OVERVIEW TAB
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("house"), " Overview"),
    value = "overview",
    
    bslib::card(
      bslib::card_body(
        style = "padding: 1.5rem;",
        
        # --- Investigation Summary Banner ------------------------------------
        div(
          style = paste0(
            "background: #f8f9fa; border: 1px solid #dee2e6; ",
            "border-left: 4px solid #4E79A7; border-radius: 6px; ",
            "padding: 1.25rem 1.5rem; margin-bottom: 1.25rem;"
          ),
          div(
            style = "display: flex; align-items: flex-start; gap: 1.5rem;",
            
            # Left: summary text + badges
            div(
              style = "flex: 1;",
              p(
                style = "font-size: 0.72rem; color: #888; text-transform: uppercase; letter-spacing: 0.05em; margin: 0 0 0.4rem;",
                "Investigation Summary"
              ),
              p(
                style = "font-size: 0.95rem; color: #212529; margin: 0 0 0.75rem; line-height: 1.6;",
                "On 17 May 2046, ", tags$code("john_windward"), " posted anomalous content to SaidIT
                 via an 11-node relay chain originating from ", tags$code("james_stern"), ".
                 The same behaviour occurred on 10 and 11 May 2046. All three incidents share
                 an identical four-event terminal sequence at one-second precision —
                 evidence of a scripted automated protocol, not a manual error."
              ),
              div(
                tags$span(
                  style = "display:inline-block; background:#f8d7da; color:#842029; font-size:0.75rem; font-weight:500; padding:3px 10px; border-radius:4px; margin-right:6px;",
                  "\u26a0 3 confirmed incidents"
                ),
                tags$span(
                  style = "display:inline-block; background:#cfe2ff; color:#084298; font-size:0.75rem; font-weight:500; padding:3px 10px; border-radius:4px; margin-right:6px;",
                  "11-node relay chain"
                ),
                tags$span(
                  style = "display:inline-block; background:#d1e7dd; color:#0a3622; font-size:0.75rem; font-weight:500; padding:3px 10px; border-radius:4px;",
                  "\u2713 Fix identified"
                )
              )
            ),
            
            # Right: total events counter
            div(
              style = "text-align: right; flex-shrink: 0;",
              p(style = "font-size: 0.72rem; color: #888; margin: 0;", "Dataset"),
              p(style = "font-size: 1.6rem; font-weight: 600; color: #212529; margin: 0; line-height: 1.2;", "185,147"),
              p(style = "font-size: 0.72rem; color: #888; margin: 0;", "total events")
            )
          )
        ),
        
        # --- Three Key Finding Cards ----------------------------------------
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; margin-bottom: 1.25rem;",
          
          # Card 1: Confirmed Incidents
          div(
            style = "background: #f8f9fa; border-radius: 6px; padding: 1rem 1.25rem;",
            p(style = "font-size: 0.7rem; color: #888; text-transform: uppercase; letter-spacing: 0.05em; margin: 0 0 0.4rem;",
              "Confirmed Incidents"),
            p(style = "font-size: 1rem; font-weight: 600; color: #212529; margin: 0 0 0.3rem;",
              "10, 11, 17 May 2046"),
            p(style = "font-size: 0.78rem; color: #666; margin: 0; line-height: 1.5;",
              tags$code("HiddenOrca.txt"), tags$br(),
              tags$code("MellowOtter.txt"), tags$br(),
              tags$code("SwiftWren.txt"))
          ),
          
          # Card 2: Anomaly Rule
          div(
            style = "background: #f8f9fa; border-radius: 6px; padding: 1rem 1.25rem;",
            p(style = "font-size: 0.7rem; color: #888; text-transform: uppercase; letter-spacing: 0.05em; margin: 0 0 0.4rem;",
              "Anomaly Detection Rule"),
            p(style = "font-size: 0.85rem; font-family: monospace; font-weight: 600; color: #E15759; margin: 0 0 0.3rem; word-break: break-all;",
              "content_source IS NOT NULL"),
            p(style = "font-size: 0.78rem; color: #666; margin: 0; line-height: 1.5;",
              "Fires on ", tags$code("saidit_post"), " events only", tags$br(),
              "3 of 185,147 rows match")
          ),
          
          # Card 3: Proposed Fix
          div(
            style = "background: #f8f9fa; border-radius: 6px; padding: 1rem 1.25rem;",
            p(style = "font-size: 0.7rem; color: #888; text-transform: uppercase; letter-spacing: 0.05em; margin: 0 0 0.4rem;",
              "Proposed Fix"),
            p(style = "font-size: 0.85rem; font-family: monospace; font-weight: 600; color: #59A14F; margin: 0 0 0.3rem;",
              "saidit_post_check"),
            p(style = "font-size: 0.78rem; color: #666; margin: 0; line-height: 1.5;",
              "Add null-check on ", tags$code("content_source"), tags$br(),
              "No new infrastructure needed")
          )
        ),
        
        # --- Module Navigation Guide ----------------------------------------
        p(
          style = "font-size: 0.72rem; color: #888; text-transform: uppercase; letter-spacing: 0.05em; margin: 0 0 0.6rem;",
          "Explore the Modules"
        ),
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px;",
          
          # Module 1
          div(
            style = paste0(
              "background: white; border: 1px solid #dee2e6; ",
              "border-top: 3px solid #4E79A7; border-radius: 6px; padding: 1rem 1.25rem;"
            ),
            p(style = "font-size: 0.9rem; font-weight: 600; color: #212529; margin: 0 0 0.4rem;",
              bsicons::bs_icon("diagram-3"), " System Topology"),
            p(style = "font-size: 0.8rem; color: #555; margin: 0; line-height: 1.6;",
              "Explore the 11-node relay chain, agent interaction network, and incident event timeline.
               Trace how ", tags$code("james_stern"), " initiates and ", tags$code("john_windward"),
              " executes the injection.")
          ),
          
          # Module 2
          div(
            style = paste0(
              "background: white; border: 1px solid #dee2e6; ",
              "border-top: 3px solid #E15759; border-radius: 6px; padding: 1rem 1.25rem;"
            ),
            p(style = "font-size: 0.9rem; font-weight: 600; color: #212529; margin: 0 0 0.4rem;",
              bsicons::bs_icon("shield-exclamation"), " Anomaly Detection"),
            p(style = "font-size: 0.8rem; color: #555; margin: 0; line-height: 1.6;",
              "Compare normal vs anomalous ", tags$code("saidit_post"), " structure.
               Step through the validation gate sequence and inspect the full
               event log for each of the three confirmed incidents.")
          ),
          
          # Module 3
          div(
            style = paste0(
              "background: white; border: 1px solid #dee2e6; ",
              "border-top: 3px solid #59A14F; border-radius: 6px; padding: 1rem 1.25rem;"
            ),
            p(style = "font-size: 0.9rem; font-weight: 600; color: #212529; margin: 0 0 0.4rem;",
              bsicons::bs_icon("clock-history"), " Historical Patterns"),
            p(style = "font-size: 0.8rem; color: #555; margin: 0; line-height: 1.6;",
              "Actor participation heatmap across all three incidents, terminal
               sequence comparison proving scripted timing, and before/after
               intervention flow at the validation gate.")
          )
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # MODULE 1 — System Topology & Event Reconstruction
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("diagram-3"), " System Topology"),
    value = "mod1",
    
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width  = 260,
        title  = "Controls",
        open   = TRUE,
        
        selectInput(
          "mod1_incident",
          "Select Incident Date",
          choices  = c("All" = "all",
                       "10 May 2046" = "2046-05-10",
                       "11 May 2046" = "2046-05-11",
                       "17 May 2046" = "2046-05-17"),
          selected = "all"
        ),
        hr(),
        radioButtons(
          "mod1_network_mode",
          "Network View",
          choices = c(
            "Relay Chain Only"         = "relay",
            "Core Actors + Neighbours" = "core"
          ),
          selected = "relay"
        ),
        checkboxInput("mod1_show_labels", "Show node labels", value = TRUE),
        hr(),
        helpText(
          "Relay Chain: confirmed May 17 2046 chain.",
          style = "font-size:0.8em; color:#888;"
        ),
        helpText("Click a node to inspect its event log below.")
      ),
      
      bslib::layout_column_wrap(
        width = 1,
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Agent Interaction Network"),
          visNetwork::visNetworkOutput("mod1_network", height = "450px")
        ),
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Incident Event Timeline"),
          plotly::plotlyOutput("mod1_timeline", height = "300px")
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # MODULE 2 — Content Source Anomaly Detection
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("shield-exclamation"), " Anomaly Detection"),
    value = "mod2",
    
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        title = "Controls",
        open  = TRUE,
        
        radioButtons(
          "mod2_view",
          "View",
          choices  = c("Normal vs Anomalous" = "compare",
                       "Injection Spotlight" = "gate",
                       "Full Event Log"      = "log"),
          selected = "compare"
        ),
        hr(),
        selectInput(
          "mod2_incident",
          "Incident Date (Gate/Log view)",
          choices = c("10 May 2046" = "2046-05-10",
                      "11 May 2046" = "2046-05-11",
                      "17 May 2046" = "2046-05-17"),
          selected = "2046-05-17"
        )
      ),
      
      bslib::layout_column_wrap(
        width = 1,
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(textOutput("mod2_panel_title", inline = TRUE)),
          uiOutput("mod2_main_plot")
        ),
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Event Log Explorer"),
          DT::dataTableOutput("mod2_event_log")
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # MODULE 3 — Historical Patterns & Intervention
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("clock-history"), " Historical Patterns"),
    value = "mod3",
    
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 260,
        title = "Controls",
        open  = TRUE,
        
        radioButtons(
          "mod3_view",
          "View",
          choices  = c(
            "Actor Participation Heatmap"  = "heatmap",
            "Terminal Sequence Comparison" = "sequence",
            "Intervention: Current State"  = "intervention_before",
            "Intervention: Proposed Fix"   = "intervention_after"
          ),
          selected = "heatmap"
        ),
        hr(),
        sliderInput(
          "mod3_top_n",
          "Top N actors (heatmap)",
          min = 5, max = 16, value = 15, step = 1
        ),
        hr(),
      ),
      
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(textOutput("mod3_panel_title", inline = TRUE)),
        uiOutput("mod3_main_plot")
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # About tab
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("info-circle"), " About"),
    value = "about",
    
    bslib::card(
      bslib::card_header("TraceNet \u2014 VAST Challenge 2026 MC2"),
      bslib::card_body(
        p("TraceNet is a visual analytics system built to investigate anomalous",
          "agent behaviour in a multi-agent AI system (Company A, 2046)."),
        p(strong("Confirmed findings:")),
        tags$ul(
          tags$li("Anomaly rule: content_source IS NOT NULL on saidit_post events"),
          tags$li("Three confirmed incidents: 10 May, 11 May, 17 May 2046"),
          tags$li("Terminal executor: john_windward"),
          tags$li("Injected files: HiddenOrca.txt, MellowOtter.txt, SwiftWren.txt"),
          tags$li("11-node relay chain originating from james_stern"),
          tags$li("Four-event terminal sequence at one-second intervals")
        ),
        hr(),
        p("Built with R Shiny \u00b7 ISSS608 AY2025-26 April Term \u00b7 SMU")
      )
    )
  ),
  
  # Footer
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$span(
      style = "font-size:0.8em; color:#aaa; padding-right:12px;",
      "ISSS608 \u00b7 AY2025-26"
    )
  )
)
