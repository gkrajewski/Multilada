#' Preprocess responses to *WP Form Maker* matrix-type questions
#'
#' Matrix questions...
#'
#' @param data blah
#' @param names_prefix blah
#' @param keep blah
#'
#' @details
#' Additional details...
#'
#' @returns description
#'
#' @examples
#' # example code
#'
#' @export
fm_matrix <- function(data, names_prefix = NULL, keep = FALSE) {
     if(is.null(names_prefix)) colnames(data)[2] -> names_prefix
     pattern <- stringr::regex("\\[
                               (?<varname>[^,\\[\\]]+?)
                               ,
                               (?<value>[^,\\[\\]]+?)
                               \\]
                               =(?<response>[01])", comments = TRUE)

     stringr::str_match_all(dplyr::pull(data[, 2]), pattern) %>% purrr::map(function(x) {
          as.data.frame(x) %>%
               dplyr::select(- .data$V1) %>%
               dplyr::filter(.data$response == 1) %>%
               dplyr::select(- .data$response)
     }) %>% purrr::list_rbind(names_to = "rowid") %>%
          tidyr::pivot_wider(names_from = .data$varname, values_from = .data$value,
                             names_prefix = paste0(names_prefix, "_"),
                             names_repair = "universal_quiet") -> result

     if(! keep) data %>% dplyr::select(1) -> data
     data %>% tibble::rowid_to_column() %>%
          dplyr::left_join(result, by = "rowid") %>%
          dplyr::arrange(.data$rowid) %>%
          dplyr::select(- .data$rowid)
}

#' @rdname fm_matrix
#' @export
fm_grading <- function(data, names_prefix = NULL, keep = FALSE) {
     if(is.null(names_prefix)) colnames(data)[2] -> names_prefix
     pattern <- stringr::regex("(?<varname>[^:, ]+?)
                               :\\s
                               (?<value>[^:, ]*?)
                               ,\\s", comments = TRUE)

     stringr::str_match_all(dplyr::pull(data[, 2]), pattern) %>% purrr::map(function(x) {
          as.data.frame(x) %>%
               dplyr::select(- .data$V1)
     }) %>% purrr::list_rbind(names_to = "rowid") %>%
          tidyr::pivot_wider(names_from = .data$varname, values_from = .data$value,
                             names_prefix = paste0(names_prefix, "_"),
                             names_repair = "universal_quiet") %>%
             dplyr::mutate(dplyr::across(2 : tidyselect::last_col(), ~ dplyr::na_if(., y = "")))  -> result

     if(! keep) data %>% dplyr::select(1) -> data
     data %>% tibble::rowid_to_column() %>%
          dplyr::left_join(result, by = "rowid") %>%
          dplyr::arrange(.data$rowid) %>%
          dplyr::select(- .data$rowid)
}
