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
#'   - "birth_date", "sex", and "guardian" (*What is your relationship to the child?*),
#'   if relevant data collected for a given form
#'   ("demographic" input type in `settings.csv`),
#'   - optionally "run" and "form",
#'   - "start" ("start_date" in the `data` dataframe),
#'   "end" ("end_date" in the `data` dataframe), "duration"
#'   (difference between "end_date" and "start_date" in the `data` dataframe).
#'
#' @export
cdi_submissions <- function(data, run = FALSE, form = FALSE, sort.by.end = FALSE) {
        data %>% dplyr::filter(.data$answer_type == "demographic") %>%
                 tidyr::pivot_wider(id_cols = .data$id,
                                    names_from = .data$answer_id,
                                    values_from = .data$answer1) %>%
                 dplyr::mutate(dplyr::across(dplyr::matches("birth_date"), lubridate::ymd)) -> submissions
        data %>% dplyr::rename(start = .data$start_date,
                               end = .data$end_date) %>%
                 dplyr::select(.data$id, .data$run, .data$form, .data$start, .data$end) %>%
                 dplyr::mutate(duration = lubridate::as.duration(.data$end - .data$start)) %>%
                 dplyr::distinct() -> data
        if(! run) data %>% dplyr::select(- .data$run) -> data
        if(! form) data %>% dplyr::select(- .data$form) -> data
        dplyr::right_join(submissions, data) -> data
        if(sort.by.end) data %>% dplyr::arrange(.data$end) else
                        data %>% dplyr::arrange(.data$start)
}

#' @rdname cdi_submissions
#' @export
cdi_time <- cdi_submissions
