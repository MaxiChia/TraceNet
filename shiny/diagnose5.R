source('global.R')
cat('--- anomalous_events full detail ---\n')
print(anomalous_events[, c('datetime','content_source','parties')])
cat('\n--- terminal sequence per incident ---\n')
for (d in as.character(INCIDENT_DATES)) {
  cat('\n', d, ':\n')
  seq_events <- incident_events |>
    dplyr::filter(
      date == as.Date(d),
      short_name %in% c('saidit_post_check','saidit_post','delete_file'),
      purrr::map_lgl(parties, ~any(stringr::str_detect(.x, 'john_windward')))
    ) |>
    dplyr::arrange(datetime) |>
    dplyr::select(datetime, short_name, content_source)
  print(seq_events)
}
