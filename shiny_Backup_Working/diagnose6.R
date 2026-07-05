source('global.R')
chain_actors <- c("john_windward","chloe_ballast","daniel_gangway",
                  "lily_anchorline","mia_fender","victoria_rigging",
                  "zoey_drydock","gabriel_sonar","james_stern")
for (d in as.character(INCIDENT_DATES)) {
  n <- incident_events |>
    dplyr::filter(
      date == as.Date(d),
      purrr::map_lgl(parties, ~ any(stringr::str_detect(
        .x, paste(chain_actors, collapse="|"))))
    ) |> nrow()
  cat(d, ":", n, "chain actor events\n")
}
