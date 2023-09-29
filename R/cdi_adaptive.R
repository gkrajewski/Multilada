#' Summarise adaptive CDI submissions
#'
#' Get various information about adaptive CDI submissions.
#'
#' @param data A `dataframe` containing answers to a *CDI-Online* **adaptive** form,
#'   as returned by [cdi_read()] (or obtained otherwise).
#'
#' @details So far it summarises submissions only.
#'   Extracting information on individual responses, items, etc. might be added later
#'   (here or in separate functions).
#'
#' @returns A `dataframe` summarising key information for each submission.
#'
#' @export
cdi_adaptive <- function(data) {
     ws_cat %>% dplyr::filter(.data$final == 1) %>%
          dplyr::rename(id = .data$idx, sex = .data$gender, birth_date = .data$birth, no_items = .data$q_id,
                        guardian = .data$filler, start = .data$start_date, end = .data$end_date) %>%
          dplyr::mutate(duration = lubridate::as.duration(.data$end - .data$start)) %>%
          dplyr::select(.data$id, .data$sex, .data$birth_date, .data$guardian, .data$start, .data$end,
                        .data$duration, .data$group, .data$start_theta, .data$no_items, .data$theta, .data$se_theta)
}
