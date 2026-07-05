# =============================================================================
# ui.R — TraceNet Shiny App
# Visual upgrade: forensic command-centre shell + clearer investigation story.
# Analysis logic and output IDs are preserved.
# =============================================================================

# -----------------------------------------------------------------------------
# Small UI helpers
# -----------------------------------------------------------------------------
tn_badge <- function(text, type = "neutral", icon = NULL) {
  tags$span(
    class = paste("tn-badge", paste0("tn-badge-", type)),
    if (!is.null(icon)) bsicons::bs_icon(icon),
    tags$span(text)
  )
}

tn_metric <- function(label, value, caption = NULL, type = "default") {
  tags$div(
    class = paste("tn-metric", paste0("tn-metric-", type)),
    tags$p(class = "tn-metric-label", label),
    tags$div(class = "tn-metric-value", value),
    if (!is.null(caption)) tags$p(class = "tn-metric-caption", caption)
  )
}

tn_step <- function(number, title, body, type = "default", icon = "arrow-right") {
  tags$div(
    class = paste("tn-step", paste0("tn-step-", type)),
    tags$div(class = "tn-step-number", number),
    tags$div(
      class = "tn-step-content",
      tags$p(class = "tn-step-title", bsicons::bs_icon(icon), tags$span(title)),
      tags$p(class = "tn-step-body", body)
    )
  )
}

tn_module_card <- function(icon, eyebrow, title, body, accent = "teal", bullets = NULL) {
  tags$div(
    class = paste("tn-module-card", paste0("tn-accent-", accent)),
    tags$div(class = "tn-module-icon", bsicons::bs_icon(icon)),
    tags$p(class = "tn-eyebrow", eyebrow),
    tags$h3(title),
    tags$p(class = "tn-module-body", body),
    if (!is.null(bullets)) {
      tags$ul(class = "tn-mini-list", lapply(bullets, tags$li))
    }
  )
}

tn_page_intro <- function(eyebrow, title, body, badges = NULL) {
  tags$div(
    class = "tn-page-intro",
    tags$div(
      tags$p(class = "tn-eyebrow", eyebrow),
      tags$h2(title),
      tags$p(class = "tn-page-intro-body", body)
    ),
    if (!is.null(badges)) tags$div(class = "tn-badge-row", badges)
  )
}

tn_visual_note <- function(title, body, type = "note", icon = "search") {
  tags$div(
    class = paste("tn-visual-note", paste0("tn-visual-note-", type)),
    tags$div(class = "tn-visual-note-icon", bsicons::bs_icon(icon)),
    tags$div(
      tags$p(class = "tn-visual-note-title", title),
      tags$p(class = "tn-visual-note-body", body)
    )
  )
}

# -----------------------------------------------------------------------------
# Main UI
# -----------------------------------------------------------------------------
ui <- bslib::page_navbar(
  title = tags$div(
    class = "tn-brand",
    tags$img(src = "tracenet_logo.png", height = "34px", class = "tn-brand-logo", onerror = "this.style.display='none';"),
    tags$span(class = "tn-brand-wordmark", "TraceNet")
  ),
  theme = bslib::bs_theme(
    version      = 5,
    bootswatch   = "flatly",
    bg           = "#F8FAFB",
    fg           = "#1E293B",
    primary      = "#1D9E75",
    secondary    = "#E2E8F0",
    success      = "#1D9E75",
    info         = "#B45309",
    warning      = "#E8A52B",
    danger       = "#D84040",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  id = "main_nav",

  # ---------------------------------------------------------------------------
  # OVERVIEW TAB
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("house"), " Overview"),
    value = "overview",

    tags$section(
      class = "tn-hero",
      tags$div(
        class = "tn-hero-copy",
        tags$p(class = "tn-kicker", "VAST 2026 MC2 · Multi-agent forensic investigation"),
        tags$h1("Find the injected post. Trace the relay. Prove the fix."),
        tags$p(
          class = "tn-hero-lede",
          "TraceNet turns 185,147 event-log rows into a guided forensic story: three anomalous SaidIT posts, one repeated terminal sequence, and one intervention point that stops recurrence."
        ),
        tags$div(
          class = "tn-badge-row tn-hero-badges",
          tn_badge("3 confirmed incidents", "danger", "exclamation-triangle"),
          tn_badge("11-step relay · 9 unique agents", "gold", "diagram-3"),
          tn_badge("1 validation-gate fix", "success", "patch-check")
        )
      ),
      tags$div(
        class = "tn-hero-panel",
        tn_metric("Event log rows", "185,147", "full MC2 dataset", "default"),
        tn_metric("Anomalous posts", "3", "content_source is not NULL", "danger"),
        tn_metric("Terminal executor", "john_windward", "final actor in all incidents", "gold")
      )
    ),

    tags$section(
      class = "tn-section-grid tn-section-grid-3",
      tn_module_card(
        icon = "calendar2-week",
        eyebrow = "Confirmed incidents",
        title = "10, 11, 17 May 2046",
        body = "All three events show the same suspicious structure and converge on the same terminal executor.",
        accent = "red",
        bullets = c("HiddenOrca.txt", "MellowOtter.txt", "SwiftWren.txt")
      ),
      tn_module_card(
        icon = "bug",
        eyebrow = "Detection rule",
        title = "content_source IS NOT NULL",
        body = "The anomaly is rare: only three saidit_post events carry a populated content_source field.",
        accent = "gold",
        bullets = c("Normal posts: content_source = NA", "Anomalous posts: injected source file")
      ),
      tn_module_card(
        icon = "shield-check",
        eyebrow = "Proposed intervention",
        title = "Enhance saidit_post_check",
        body = "A null-check at the existing validation gate blocks the injected post before deletion events can erase the evidence trail.",
        accent = "green",
        bullets = c("No new infrastructure", "Blocks all three confirmed cases")
      )
    ),

    tags$section(
      class = "tn-section",
      tags$div(class = "tn-section-heading",
               tags$p(class = "tn-eyebrow", "Presentation walkthrough"),
               tags$h2("Use the app as a 4-step investigation narrative")),
      tags$div(
        class = "tn-storyline-grid",
        tn_step("01", "Spot the anomaly", "Start with the Overview and show that only 3 of 185,147 rows match the anomaly rule.", "danger", "search"),
        tn_step("02", "Trace the relay", "Move to System Topology and show how james_stern routes the task to john_windward.", "gold", "diagram-3"),
        tn_step("03", "Prove repetition", "Use Historical Patterns to compare the identical four-event sequence across all incidents.", "default", "clock-history"),
        tn_step("04", "Close with the fix", "End on the intervention diagram: one validation-gate null-check prevents recurrence.", "success", "patch-check")
      )
    ),

    tags$section(
      class = "tn-section-grid tn-section-grid-3",
      tn_module_card(
        icon = "diagram-3",
        eyebrow = "Module 01",
        title = "System Topology",
        body = "Interactive relay network and incident timeline. Best for showing the chain of responsibility.",
        accent = "teal"
      ),
      tn_module_card(
        icon = "shield-exclamation",
        eyebrow = "Module 02",
        title = "Anomaly Detection",
        body = "Needle-in-haystack comparison of normal versus anomalous SaidIT post structures.",
        accent = "red"
      ),
      tn_module_card(
        icon = "clock-history",
        eyebrow = "Module 03",
        title = "Historical Patterns",
        body = "The closing argument: recurrence, scripted timing, and intervention impact.",
        accent = "green"
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # MODULE 1 — System Topology & Event Reconstruction
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("diagram-3"), " System Topology"),
    value = "mod1",

    tn_page_intro(
      "Module 01 · System Topology & Event Reconstruction",
      "Follow the relay from origin to terminal executor",
      "Use this page to reconstruct how the task travelled through the agent network and how the final anomalous post was executed.",
      tagList(
        tn_badge("james_stern = relay root", "gold", "person"),
        tn_badge("john_windward = terminal executor", "danger", "person"),
        tn_badge("11-step relay · 9 unique agents", "neutral", "diagram-3")
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 220,
        title = "Investigation Controls",
        open = TRUE,

        selectInput(
          "mod1_incident",
          "Incident Date",
          choices = c("All incidents" = "all",
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
            "Relay Chain Only" = "relay",
            "Core Actor Importance" = "core"
          ),
          selected = "relay"
        ),
        checkboxInput("mod1_show_labels", "Show node labels", value = TRUE),
        
        selectInput(
          "mod1_focus_actor",
          "Focus Actor",
          choices = c(
            "All actors" = "all",
            "James Stern" = "Agent/person:james_stern",
            "Gabriel Sonar" = "Agent/person:gabriel_sonar",
            "Daniel Gangway" = "Agent/person:daniel_gangway",
            "Zoey Drydock" = "Agent/person:zoey_drydock",
            "Mia Fender" = "Agent/person:mia_fender",
            "Victoria Rigging" = "Agent/person:victoria_rigging",
            "Lily Anchorline" = "Agent/person:lily_anchorline",
            "Chloe Ballast" = "Agent/person:chloe_ballast",
            "John Windward" = "Agent/person:john_windward"
          ),
          selected = "all"
        ),
        
        hr(),
        
        tags$p(
          class = "sidebar-section-title",
          "Network Analytics"
        ),
        
        selectInput(
          "mod1_centrality_metric",
          "Centrality Measure",
          choices = c(
            "Degree" = "degree",
            "In-degree" = "in_degree",
            "Out-degree" = "out_degree",
            "Betweenness" = "betweenness",
            "Closeness" = "closeness",
            "Eigenvector" = "eigenvector"
          ),
          selected = "degree"
        ),
        
        sliderInput(
          "mod1_top_n_centrality",
          "Top N Actors",
          min = 5,
          max = 20,
          value = 10,
          step = 1
        ),
        
        checkboxInput(
          "mod1_size_by_centrality",
          "Size nodes by centrality",
          value = TRUE
        ),
        
        hr(),
        
        tags$div(
          class = "tn-sidebar-tip",
          tags$p(class = "tn-sidebar-tip-title", bsicons::bs_icon("cursor"), " Demo tip"),
          tags$p("Open with the relay chain view, then switch to timeline to show the time-ordered execution path.")
        )
      ),

      bslib::card(
        full_screen = TRUE,
        class = "tn-output-card",
        bslib::card_header(
          tags$div(
            class = "tn-card-header-row",
            tags$div(
              tags$p(class = "tn-card-eyebrow", "Topology visual"),
              tags$span("Relay Chain / Timeline")
            ),
            radioButtons(
              "mod1_display",
              NULL,
              choices = c(
                "Network" = "network",
                "Timeline" = "timeline",
                "Centrality" = "centrality"
              ),
              selected = "network",
              inline = TRUE
            )
          )
        ),
        bslib::card_body(
          tn_visual_note(
            "What to look for",
            "The presentation point is not just who is connected to whom. Look for the repeated path from james_stern to john_windward and the fast transition into the terminal post-and-delete sequence.",
            "note",
            "binoculars"
          ),
          conditionalPanel(
            condition = "input.mod1_display == 'network'",
            tags$div(
              visNetwork::visNetworkOutput("mod1_network", height = "calc(100vh - 260px)"),
              
              tags$div(
                class = "tn-inline-legend",
                tags$div(
                  class = "legend-grid legend-grid-compact",
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-dot legend-gold"),
                           tags$span("Gold node = relay root")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-dot legend-green"),
                           tags$span("Green node = relay/core actor")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-dot legend-yellow"),
                           tags$span("Yellow node = reused relay actor")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-dot legend-red"),
                           tags$span("Red node = terminal executor / anomaly")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-line legend-line-grey"),
                           tags$span("Grey edge = normal delegation")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-line legend-line-red"),
                           tags$span("Red edge = terminal anomaly path")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-size-demo"),
                           tags$span("Node size = selected centrality / actor importance")),
                  
                  tags$div(class = "legend-item",
                           tags$span(class = "legend-arrow-demo", "→"),
                           tags$span("Arrow = direction of task handoff"))
                )
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.mod1_display == 'timeline'",
            plotly::plotlyOutput("mod1_timeline", height = "calc(100vh - 170px)")
          ),
          
          conditionalPanel(
            condition = "input.mod1_display == 'centrality'",
            tags$div(
              class = "tn-analytics-view",
              tags$div(
                class = "tn-analytics-intro",
                tags$p(class = "tn-card-eyebrow", "Network Analytics"),
                tags$p(style = "font-size:0.85rem; font-weight:600; margin:0 0 4px;", "Top Actors by Selected Centrality"),
                tags$p(style = "font-size:0.78rem; color:#AAB7BA; margin:0 0 8px;",
                       "Use the sidebar to switch centrality measure and adjust top N actors shown in the chart and table below.")
              ),
              
              uiOutput("mod1_focus_actor_summary"),
              
              tags$div(
                class = "tn-centrality-guide compact",
                tags$p(class = "tn-centrality-guide-title", "Centrality guide"),
                tags$p(
                  class = "tn-centrality-guide-text",
                  tags$span(class = "centrality-term", "Degree:"), " total direct connections. ",
                  tags$span(class = "centrality-term", "In-degree:"), " incoming links. ",
                  tags$span(class = "centrality-term", "Out-degree:"), " outgoing links. ",
                  tags$span(class = "centrality-term", "Betweenness:"), " bridge/control point. ",
                  tags$span(class = "centrality-term", "Closeness:"), " reachability through fewer steps. ",
                  tags$span(class = "centrality-term", "Eigenvector:"), " influence through important neighbours."
                )
              ),
              
              tags$hr(),
              
              plotOutput("mod1_centrality_chart", height = "320px"),
              
              tags$hr(),
              
              DT::DTOutput("mod1_centrality_table")
            )
          )
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

    tn_page_intro(
      "Module 02 · Content Source Anomaly & Detection",
      "Show why the three posts are structurally abnormal",
      "This module makes the anomaly visible by comparing normal SaidIT posts against the rare cases where content_source is unexpectedly populated.",
      tagList(
        tn_badge("content_source IS NOT NULL", "danger", "code-square"),
        tn_badge("3 of 185,147 rows", "gold", "funnel"),
        tn_badge("gate check missed it", "neutral", "shield-slash")
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 220,
        title = "Investigation Controls",
        open = TRUE,

        radioButtons(
          "mod2_view",
          "View",
          choices = c("Normal vs Anomalous" = "compare",
                      "Injection Spotlight" = "gate",
                      "Full Event Log" = "log"),
          selected = "compare"
        ),
        hr(),
        selectInput(
          "mod2_incident",
          "Incident Date (Spotlight / Log)",
          choices = c("10 May 2046" = "2046-05-10",
                      "11 May 2046" = "2046-05-11",
                      "17 May 2046" = "2046-05-17"),
          selected = "2046-05-17"
        ),
        hr(),
        tags$div(
          class = "tn-sidebar-tip tn-sidebar-tip-danger",
          tags$p(class = "tn-sidebar-tip-title", bsicons::bs_icon("exclamation-diamond"), " Demo tip"),
          tags$p("Use Normal vs Anomalous first. The audience should see that the red points are rare before you explain the validation-gate failure.")
        )
      ),

      bslib::card(
        full_screen = TRUE,
        class = "tn-output-card",
        bslib::card_header(
          tags$div(
            class = "tn-card-header-row",
            tags$div(
              tags$p(class = "tn-card-eyebrow", "Anomaly visual"),
              textOutput("mod2_panel_title", inline = TRUE)
            ),
            tags$span(class = "tn-header-pill tn-header-pill-danger", "content_source ≠ NULL")
          )
        ),
        bslib::card_body(
          tn_visual_note(
            "What to look for",
            "The red injected posts are the exception. The Injection Spotlight then explains the failure mechanism: the validation gate fires but does not inspect content_source before the post is made.",
            "danger",
            "search"
          ),
          
          uiOutput("mod2_anomaly_summary"),
          
          uiOutput("mod2_plot_legend"),
          
          conditionalPanel(
            condition = "input.mod2_view == 'compare'",
            plotly::plotlyOutput("mod2_plot_inner", height = "calc(100vh - 170px)")
          ),
          
          conditionalPanel(
            condition = "input.mod2_view == 'gate'",
            plotOutput("mod2_plot_gate", height = "calc(100vh - 170px)")
          ),
          
          conditionalPanel(
            condition = "input.mod2_view == 'log'",
            DT::dataTableOutput("mod2_event_log")
          )
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

    tn_page_intro(
      "Module 03 · Historical Patterns & Intervention",
      "Prove recurrence, then end with the fix",
      "This is the closing argument for the presentation: the same actors and terminal sequence reappear across incidents, indicating a repeatable scripted protocol rather than a one-off error.",
      tagList(
        tn_badge("same 4-event terminal sequence", "gold", "arrow-repeat"),
        tn_badge("one-second precision", "danger", "stopwatch"),
        tn_badge("single intervention point", "success", "shield-check")
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 220,
        title = "Investigation Controls",
        open = TRUE,

        radioButtons(
          "mod3_view",
          "View",
          choices = c(
            "System Activity Baseline"     = "activity",
            "Actor Participation Heatmap"  = "heatmap",
            "Terminal Sequence Comparison" = "sequence",
            "Intervention: Current State"  = "intervention_before",
            "Intervention: Proposed Fix"   = "intervention_after"
          ),
          selected = "activity"
        ),
        hr(),
        selectInput(
          "mod3_heatmap_mode",
          "Heatmap Mode",
          choices = c(
            "Actor recurrence" = "confirmed",
            "Actor behaviour" = "behaviour"
          ),
          selected = "confirmed"
        ),
        
        uiOutput("mod3_incident_filter_ui"),
        
        selectInput(
          "mod3_top_n",
          "Top N actors (heatmap)",
          choices = as.character(5:16),
          selected = "12"
        ),
        hr(),
        
        # Activity Baseline controls — only visible when that view is selected
        conditionalPanel(
          condition = "input.mod3_view == 'activity'",
          checkboxInput(
            "mod3_exclude_incidents",
            "Exclude incident dates (clean baseline)",
            value = TRUE
          ),
          checkboxInput(
            "mod3_relay_only",
            "Relay chain actors only",
            value = FALSE
          ),
          sliderInput(
            "mod3_activity_top_n",
            "Top N actors",
            min = 5, max = 16, value = 12, step = 1
          ),
          selectInput(
            "mod3_activity_event_type",
            "Event type filter",
            choices = c(
              "All event types"        = "all",
              "saidit_post"            = "saidit_post",
              "queue_subordinate_task" = "queue_subordinate_task",
              "saidit_post_check"      = "saidit_post_check",
              "delete_file"            = "delete_file",
              "read_file"              = "read_file"
            ),
            selected = "all"
          )
        ),
        
        tags$div(
          class = "tn-sidebar-tip tn-sidebar-tip-success",
          tags$p(class = "tn-sidebar-tip-title", bsicons::bs_icon("star-fill"), " Demo tip"),
          tags$p("End your presentation here. Show the sequence comparison, then switch to Proposed Fix for a memorable close.")
        )
      ),

      bslib::card(
        full_screen = TRUE,
        class = "tn-output-card",
        bslib::card_header(
          tags$div(
            class = "tn-card-header-row",
            tags$div(
              tags$p(class = "tn-card-eyebrow", "Recurrence & intervention visual"),
              textOutput("mod3_panel_title", inline = TRUE)
            ),
            tags$span(class = "tn-header-pill tn-header-pill-success", "closing argument")
          )
        ),
        bslib::card_body(
          uiOutput("mod3_main_plot")
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # About tab
  # ---------------------------------------------------------------------------
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("info-circle"), " About"),
    value = "about",

    tn_page_intro(
      "About TraceNet",
      "Visual analytics for multi-agent anomaly investigation",
      "Built for ISSS608 Visual Analytics using R Shiny, Plotly, visNetwork, and DT.",
      tagList(
        tn_badge("VAST Challenge 2026 MC2", "neutral", "database"),
        tn_badge("ISSS608 AY2025-26", "gold", "book"),
        tn_badge("R Shiny", "success", "code-slash")
      )
    ),

    bslib::card(
      class = "tn-output-card tn-about-card",
      bslib::card_header("Confirmed findings"),
      bslib::card_body(
        tags$div(
          class = "tn-about-grid",
          tn_metric("Anomaly rule", "content_source IS NOT NULL", "on saidit_post events", "danger"),
          tn_metric("Incident dates", "10 · 11 · 17 May", "2046", "gold"),
          tn_metric("Terminal executor", "john_windward", "same final actor", "danger"),
          tn_metric("Injected files", "3", "HiddenOrca / MellowOtter / SwiftWren", "default")
        ),
        tags$hr(),
        tags$ul(
          class = "tn-findings-list",
          tags$li("The relay should be described as an 11-step relay sequence involving 9 unique agents."),
          tags$li("All three confirmed incidents share a four-event terminal sequence at one-second intervals."),
          tags$li("The proposed intervention is a null-check on content_source inside saidit_post_check."),
          tags$li("No new infrastructure is required because the validation gate already fires at the right time.")
        )
      )
    )
  ),

  # Footer
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$span(class = "tn-footer-text", "ISSS608 · AY2025-26")
  )
)
