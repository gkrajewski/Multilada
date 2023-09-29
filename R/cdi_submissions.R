#' Summarise all submissions
#'
#' Useful to get a list of IDs together with basic stats.
#' `cdi_time()` is an alias for backward compatibility.
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param run A logical value indicating whether to include column "run".
#'
#' @param form A logical value indicating whether to include column "form".
#'
#' @param sort.by.end A logical value indicating whether to sort by end date
#'   (by default the resulting `tibble` is sorted by start date).
#'
#' @returns A `tibble` with the following columns:
#'   - "id",
#'   - optionally "run" and "form",
#'   - "start_date", "end_date", "duration"
#'   (difference between "end_date" and "start_date"),
#'   - "birth_date", "sex", "guardian" (*What is your relationship to the child?*),
#'   and possibly "guardian.other", if relevant data collected for a given form
#'   ("demographic" input type in `settings.csv`).
#'
#' @export
cdi_submissions <- function(data, run = FALSE, form = FALSE, sort.by.end = FALSE) {
        data %>% dplyr::filter(.data$answer_type == "demographic") %>%
                 tidyr::pivot_wider(id_cols = c(.data$id, .data$start_date),
                                    names_from = .data$answer_id,
                                    values_from = .data$answer1,
                                    names_repair = "universal_quiet") %>%
                 dplyr::mutate(dplyr::across(dplyr::matches("birth_date"), lubridate::ymd)) -> submissions
        data %>% dplyr::select(.data$id, .data$run, .data$form, .data$start_date, .data$end_date) %>%
                 dplyr::mutate(duration = lubridate::as.duration(.data$end_date - .data$start_date)) %>%
                 dplyr::distinct() -> data
        if(! run) data %>% dplyr::select(- .data$run) -> data
        if(! form) data %>% dplyr::select(- .data$form) -> data
        dplyr::left_join(data, submissions) -> data
        if(sort.by.end) data %>% dplyr::arrange(.data$end_date) else
                        data %>% dplyr::arrange(.data$start_date)
}

#' @rdname cdi_submissions
#' @export
cdi_time <- cdi_submissions
