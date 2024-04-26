#' Prepare `variables_file` for `fm_read()`
#'
#' Reads column names of a `csv` file downloaded using the "Export to CSV" option
#' of the WordPress *Form Maker* plugin (i.e. it reads field names of a given form).
#' "Repairs" duplicated names, as required by [fm_read()], returns a tibble, which
#' can be used to create `variables_file` required by [fm_read()], and
#' optionally saves it as such a file.
#'
#' @param source_file A `character`, the name of a `csv` file downloaded
#'   using the "Export to CSV" option of the WP *Form Maker* plugin.
#' @param lang A `character`, the name of the column with field names, e.g.,
#'   a language ISO code ("no", "pl" etc.).
#' @param simplified A logical value. If set to `FALSE` (default), the resulting tibble
#'   contains all the columns required by [fm_read()]
#'   ("Import", "Label", and "Type" filled with `NA`s).
#'   If set to `TRUE`, a single column tibble is returned
#'   (only column `lang` with field names).
#' @param target_file A `character`, an optional name of file to which
#'   the resulting tibble will be saved.
#' @param csv2 A logical value. If set to `FALSE` (default), the source file is read
#'   using `readr::read_csv()`. If set to `TRUE`, `readr::read_csv2()` is used.
#'
#' @details For field names (column names of the *FM* responses file)
#'   it basically applies [vctrs::vec_as_names()] with `repair = "unique"`,
#'   adding "...j" to each duplicated column, where `j` is the column position
#'   (consistent with [readr::read_csv()]'s default behaviour used in [fm_read()].
#'   The option `csv2 = TRUE` adds the support for reading a `source_file` with
#'   a non-comma separator (e.g., a semicolon).
#'
#' @returns A tibble with `lang` and optionally "Import", "Label", and "Type" columns.
#'   If `target_file` is specified, returns invisibly.
#'
#' @seealso [fm_read()]
#'
#' @export
fm_variables <- function(source_file, lang, simplified = FALSE, target_file = NULL, csv2 = FALSE) {
    if(csv2) {
        var_df <- readr::read_csv2(source_file,
                                   n_max = 1,
                                   col_names = FALSE,
                                   show_col_types = FALSE)
    } else {
        var_df <- readr::read_csv(source_file,
                                  n_max = 1,
                                  col_names = FALSE,
                                  show_col_types = FALSE)
    }
    var_df %>% dplyr::slice_head(n = 1) %>% as.character() %>%
        vctrs::vec_as_names(repair = "unique") %>% as.vector() -> var_df
    tibble::tibble({{lang}} := var_df) -> var_df
    if(! simplified) tibble::add_column(var_df, Import = NA, Label = NA, Type = NA, .before = 1) -> var_df
    if (is.null(target_file)) {
        return(var_df)
    } else {
        readr::write_csv(var_df, target_file, na = "")
        invisible(var_df)
    }
}