#' @importFrom rlang .data

#' @export
cdi_forms <- function(database = NULL) {
     connection <- multilada_connect(database, "CDI database (dev or prod)")
     forms <- RMariaDB::dbGetQuery(connection, "SHOW TABLES")
     RMariaDB::dbDisconnect(connection)
     forms <- stringr::str_match(forms[, 1], "^form_(.+)")[, 2]
     return(forms[stats::complete.cases(forms)])
}

#' @export
cdi_read <- function(form, database = NULL) {
     statement <- paste0("SELECT * FROM `form_", form, "`")
     connection <- multilada_connect(database, "CDI database (dev or prod)")
     cdi <- RMariaDB::dbGetQuery(connection, statement) %>%
             dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor))
     RMariaDB::dbDisconnect(connection)
     return(cdi)
}

#' @export
cdi_time <- function(data) {
        data %>% dplyr::group_by(.data$id) %>%
                dplyr::summarise(start = .data$start_date, duration = lubridate::as.duration(.data$end_date - .data$start_date)) %>%
                dplyr::distinct() %>% dplyr::arrange(.data$start)
}

#' @export
cdi_count_checkboxAlt <- function(data, type, category = NULL, answer = "first") {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "checkboxAlt") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        if(answer == "first") data %>% dplyr::filter(.data$answer1 == "1") -> data
        if(answer == "second") data %>% dplyr::filter(.data$answer2 == "1") -> data
        if(answer == "both") data %>% dplyr::filter(.data$answer1 == "1" & .data$answer2 == "1") -> data
        if(answer == "none") data %>% dplyr::filter(.data$answer1 == "0" & is.na(.data$answer2)) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @export
cdi_count_radio <- function(data, type, category = NULL, answer = "1") {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "radio") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        data %>% dplyr::filter(.data$answer1 == answer) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @export
cdi_count_oneCheckboxGroup <- function(data, type, category = NULL) {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "oneCheckboxGroup") -> data
        if(!is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}
