#' Preprocess responses to *WP Form Maker* matrix-type questions
#'
#' Matrix questions...
#'
#' @param data
#' @param names_prefix
#' @param keep
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

     stringr::str_match_all(data[, 2], pattern) %>% purrr::map(function(x) {
          dplyr::as_tibble(x[, -1]) %>%
               dplyr::group_by(.data$varname) %>%
               dplyr::group_map(~ {
                    dplyr::bind_cols(.y, .x[.x$response == "1", "value"])
               }) %>% purrr::list_rbind()
     }) %>% purrr::list_rbind(names_to = "rowid") %>%
          tidyr::pivot_wider(names_from = .data$varname, values_from = .data$value,
                             names_prefix = paste0(names_prefix, "_"),
                             names_repair = "universal_quiet",
                             names_sort = TRUE) -> result

     if(! keep) data %>% select(1) -> data
     data %>% rowid_to_column() %>%
          left_join(result, by = "rowid") %>%
          arrange(rowid) %>% select(- rowid)
}
