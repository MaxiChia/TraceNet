source('global.R')
relay <- mc2_df |>
  dplyr::filter(short_name == "queue_subordinate_task") |>
  dplyr::mutate(
    from_p = purrr::map_chr(parties, ~ .x[1]),
    to_p   = purrr::map_chr(parties, ~ .x[2])
  ) |>
  dplyr::filter(
    grepl("james_stern|victoria_rigging|mia_fender|lily_anchorline|john_windward|daniel_gangway|chloe_ballast", from_p) |
    grepl("james_stern|victoria_rigging|mia_fender|lily_anchorline|john_windward|daniel_gangway|chloe_ballast", to_p)
  ) |>
  dplyr::select(datetime, from_p, to_p) |>
  dplyr::arrange(datetime)
print(relay, n = 50)
