#' Parse responses to *WP Form Maker* multiple item fields
#'
#' `fm_matrix()` parses responses to (radio) "matrix" fields:
#' same scale applied to a number of items.
#' `fm_grading()` parses responses to (numeric) so-called "grading" fields:
#' same type of response to a number of items.
#' `csv` files downloaded with the "Export to CSV" option of the WP *Form Maker* plugin
#' keep all such responses in a single column, making it impossible to analyse them.
#'
#' @param data A data frame with at least two columns.
#'   The first column is expected to contain IDs and the second column
#'   should be the column to be parsed. NB this a very "non-tidyverse" and
#'   very base *R* approach but it keeps the parsed responses linked
#'   to the appropriate IDs and allows limiting parsing to a subset of IDs
#'   (if you're fluent in base *R*...).
#' @param names_prefix An optional `character` providing a prefix to the names of
#'   resulting columns. If `NULL` (the default) the names are constructed by prefixing
#'   each item label with the name of the original column (with "_" as
#'   a connector). Use this opion if you want to change this prefix
#'   (perhaps to something simpler).
#' @param keep `Logical`. Should the original column be kept in the returned
#'   data frame. Defaults to `FALSE`. Setting to `TRUE` is helpful when you want
#'   to check if the parsing works as expected. To merge the parsed responses back
#'   to the original *Form Maker* data frame you should keep the default
#'   (the original column is already there).
#'
#' @details "Grading" fields should accept only numeric input but for some reason
#' (a bug in *Form Maker*?) it hasn't always behaved like this and `fm_grading()`
#' accepts `character` entries as well.
#'
#' @returns A data frame with the first (ID) column of `data`, the resulting
#'   columns with responses to each item, and optionally with the second
#'   (parsed) column of `data`.
#'
#' @examples
#' \dontrun{
#' # To check the parsing first:
#' fm_grading(df[, c("id", "first_contact_lang")], keep = TRUE) %>% view()
#'
#' # To add the parsed responses back to the original df:
#' left_join(df, fm_grading(df[, c("id", "first_contact_lang")])) -> df
#' }
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

     stringr::str_match_all(data[[2]], pattern) %>% purrr::map(function(x) {
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

     stringr::str_match_all(data[[2]], pattern) %>% purrr::map(function(x) {
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
