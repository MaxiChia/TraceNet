# =============================================================================
# modules/mod_investigation.R — Investigation Flow
# =============================================================================

INVESTIGATION_STEPS <- list(
  list(num=1, title="The Anomalous Post",  sub="An unexpected SaidIT post triggers the investigation",    hint="3 posts in 185,147 events carry an injected content source"),
  list(num=2, title="The Relay Chain",     sub="Tracing who sent the task to john_windward",              hint="11 agents across 10h 30min — same structure on all 3 dates"),
  list(num=3, title="Prior Incidents",     sub="Has this happened before?",                               hint="Same actors, same chain — 10 and 11 May 2046 confirm recurrence"),
  list(num=4, title="Pattern Proof",       sub="Identical 4-event sequence at 1-second precision",        hint="Three different times of day — one automated script"),
  list(num=5, title="The Fix",             sub="One null-check blocks all three incidents",               hint="No new infrastructure needed — gate already fires at the right moment")
)
N_STEPS <- length(INVESTIGATION_STEPS)

# Tuned heights — generous so charts breathe
STEP_HEIGHTS      <- c("580px", "560px", "560px", "520px", "660px")
STEP_OUTPUT_TYPE  <- c("plotly", "network", "plotly", "plotly", "static")

# -----------------------------------------------------------------------------
investigation_step_ui <- function(current_step) {
  
  step     <- INVESTIGATION_STEPS[[current_step]]
  is_first <- current_step == 1
  is_last  <- current_step == N_STEPS
  height   <- STEP_HEIGHTS[current_step]
  otype    <- STEP_OUTPUT_TYPE[current_step]
  
  # Progress dots
  progress_items <- list()
  for (i in seq_len(N_STEPS)) {
    dot_style <- if (i < current_step) {
      "width:30px;height:30px;border-radius:50%;background:#59A14F;color:white;display:flex;align-items:center;justify-content:center;font-size:0.72rem;font-weight:600;flex-shrink:0;"
    } else if (i == current_step) {
      "width:30px;height:30px;border-radius:50%;background:#4E79A7;color:white;display:flex;align-items:center;justify-content:center;font-size:0.72rem;font-weight:600;flex-shrink:0;box-shadow:0 0 0 4px rgba(78,121,167,0.2);"
    } else {
      "width:30px;height:30px;border-radius:50%;background:#e9ecef;color:#adb5bd;display:flex;align-items:center;justify-content:center;font-size:0.72rem;font-weight:600;flex-shrink:0;"
    }
    dot_label <- if (i < current_step) "\u2713" else as.character(i)
    progress_items <- c(progress_items, list(tags$div(style = dot_style, dot_label)))
    if (i < N_STEPS) {
      lc <- if (i < current_step) "#59A14F" else "#dee2e6"
      progress_items <- c(progress_items, list(
        tags$div(style = paste0("flex:1;height:2px;background:", lc, ";margin:0 6px;"))
      ))
    }
  }
  
  step_labels <- lapply(seq_len(N_STEPS), function(i) {
    tags$div(
      style = paste0(
        "flex:1;text-align:center;font-size:0.67rem;padding:0 4px;line-height:1.3;",
        "color:", if (i == current_step) "#4E79A7" else "#adb5bd", ";",
        "font-weight:", if (i == current_step) "600" else "400", ";"
      ),
      INVESTIGATION_STEPS[[i]]$title
    )
  })
  
  plot_widget <- switch(otype,
                        "plotly"  = plotly::plotlyOutput("inv_plot",    height = height),
                        "network" = visNetwork::visNetworkOutput("inv_network", height = height),
                        "static"  = plotOutput("inv_plot_static",       height = height)
  )
  
  tagList(
    # Progress card
    tags$div(
      style = "background:white;border:1px solid #dee2e6;border-radius:8px;padding:0.85rem 1.25rem;margin-bottom:0.6rem;",
      tags$div(style = "display:flex;align-items:center;margin-bottom:0.45rem;", progress_items),
      tags$div(style = "display:flex;", step_labels)
    ),
    
    # Content — no extra card wrap, plot sits directly in the panel
    tags$div(
      style = "background:white;border:1px solid #dee2e6;border-radius:8px;padding:0.85rem 1.25rem;margin-bottom:0.6rem;",
      
      # Header row
      tags$div(
        style = "display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:0.7rem;",
        tags$div(
          tags$p(style = "font-size:0.65rem;color:#888;text-transform:uppercase;letter-spacing:0.06em;margin:0 0 0.15rem;",
                 paste0("Step ", current_step, " of ", N_STEPS)),
          tags$h5(style = "font-size:1rem;font-weight:700;color:#212529;margin:0 0 0.15rem;", step$title),
          tags$p(style = "font-size:0.8rem;color:#666;margin:0;", step$sub)
        ),
        tags$div(
          style = "background:#f0f4f8;border-radius:6px;padding:5px 12px;font-size:0.72rem;color:#4E79A7;max-width:220px;text-align:right;line-height:1.45;flex-shrink:0;margin-left:1rem;",
          step$hint
        )
      ),
      
      # Plot — full width, generous height
      plot_widget
    ),
    
    # Nav buttons
    tags$div(
      style = "display:flex;justify-content:space-between;align-items:center;padding:0 0.1rem 0.5rem;",
      if (is_first) tags$div() else
        actionButton("inv_prev",
                     label = tagList(tags$i(class="bi bi-arrow-left"), " Previous"),
                     class = "btn btn-outline-secondary btn-sm",
                     style = "font-size:0.82rem;padding:0.38rem 1rem;"
        ),
      tags$span(
        style = "font-size:0.75rem;color:#888;font-style:italic;",
        if (is_last) "\u2713 Investigation complete"
        else paste0("Next: ", INVESTIGATION_STEPS[[current_step + 1]]$title)
      ),
      if (is_last) tags$div() else
        actionButton("inv_next",
                     label = tagList(
                       "Next: ", INVESTIGATION_STEPS[[current_step + 1]]$title,
                       tags$i(class="bi bi-arrow-right", style="margin-left:5px;")
                     ),
                     class = "btn btn-primary btn-sm",
                     style = "font-size:0.82rem;padding:0.38rem 1rem;"
        )
    )
  )
}

# -----------------------------------------------------------------------------
# Per-step render functions — each calls the right existing module function
# -----------------------------------------------------------------------------
inv_render_step1 <- function() mod2_render_compare(normal_posts, anomalous_events)
inv_render_step2 <- function() mod1_render_network(incident_events, "relay", TRUE, "all")
inv_render_step3 <- function() mod3_render_heatmap(top_n = 16)
inv_render_step4 <- function() mod3_render_sequence()
inv_render_step5 <- function() mod3_render_intervention(panel = "intervention_after")
