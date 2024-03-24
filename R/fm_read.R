#' Read and preprocess WP *Form Maker* data
#'
#' Reads a `csv` file downloaded using the "Export to CSV" option
#' of the WordPress *Form Maker* plugin.
#'
#' @param data_file A `character`, the name of the file which the data are to be read from.
#'   It should be a `csv` file downloaded using the "Export to CSV" option
#'   of the WP *Form Maker* plugin.
#' @param variables_file A `character`, the name of the "dictionary" file. It should be
#'   a `csv` file containing at least columns named "Label", "Import", "Type",
#'   and `lang` (the latter contains column names of `data_file` for a given language).
#'   See ‘Details’.
#' @param translations_file A `character`, the name of the (optional) "translations" file.
#'   It should be a `csv` file containing at least columns named "Translation" and
#'   "Label" and a column named `lang`, which contains original values
#'   (for a given language) to be translated. See ‘Details’.
#' @param lang A `character`, the name of language specific columns in `variables_file`
#'   and `translations_file`. It may be, e.g., a language ISO code ("no", "pl" etc.).
#'
#' @details Reading import settings from an external `csv` file (`variables_file`)
#'   rather than setting them directly in *R* might seem inconvenient at least but
#'   this way it is easy to share and modify the settings across projects, versions,
#'   and languages. `variables_file` should be treated as a "dictionary" file,
#'   with columns containing original *Form Maker* form field names for
#'   each language version (usually actual questions) and a "Label" column
#'   with language independent target variable names.
#'
#'   Only columns with labels provided in the "Label" column are imported.
#'   Additionally, there is the "Import" column to quickly switch on and off importing:
#'   variables for which this column is empty are not imported.
#'
#'   Language specific field names in `variables_file` should look exactly
#'   like column names of `data_file` downloaded from the website (usually actual questions).
#'   The only exception is duplicated names (questions), which should have
#'   "...j" appended, where `j` is the column position (consistent with
#'   [readr::read_csv()]'s default `name_repair = "unique"`; see also
#'   low level [vctrs::vec_as_names()] for details and [fm_variables()],
#'   a helper function to prepare such a column of field names or indeed
#'   a whole empty `variables_file`).
#'
#'   **Everything** is imported as `character`, which is conservative and safe
#'   (also all empty fields are imported as `NA`s).
#'   The "Type" column of `variables_file` may be used to convert the type
#'   of an imported column to something else. It should contain
#'   the name of the required type ("type", so that `as.type()` exists) or,
#'   if the imported column contains strings to be converted to the date or
#'   date-time type, it should contain the expected format of the string,
#'   as used by [strptime()], i.e., the value of `format` argument to
#'   [as.POSIXct()].
#'
#'   If `translations_file` is provided, occurrences of values in its
#'   `lang` column in the imported data frame are changed to the corresponding
#'   values in its "Translation" column. If "Label" column is empty
#'   for a given row, this is done across the whole data frame
#'   (e.g., translating *ja*'s to *yes*'es across all the columns).
#'   Otherwise the translation is scoped to the column (variable)
#'   named in "Label" (using a label from `variables_file`).
#'
#'   `fm_read()` proceeds in this order: it imports data from `data_file` to
#'   a data frame, renames its columns using "Label" from `variables_file`,
#'   translates all values in all its columns using `translations_file`
#'   (if provided), and only then converts the type of selected columns
#'   using "Type" from `variables_file`. So, for example, to convert
#'   all yes/no responses to logical values, `translations_file` may be used
#'   to convert each "tak"/"ja" to "TRUE" and each "nie"/"nei" to "FALSE" and
#'   "logical" may be added to the "Type" column in relevant rows of `variables_file`.
#'
#'   All file reading is done via [readr::read_csv()] with parsing messages
#'   suppressed but the parsing issues warning on. If you see it, it is not
#'   very useful, as you don't know which file import is concerned and
#'   you can't run [readr::problems()] as advised by the warning message.
#'   Other than that, when reading `variables_file` and `translations_file`
#'   anything other than expected columns is quietly ignored (so you can keep
#'   columns with some extra information, notes etc. in these files).
#'
#' @returns A tibble imported from `data_file` with column names, values, and
#'   types changed as described above.
#'
#' @seealso [fm_variables()] for preparing a new `variables_file` or
#'   just a single, language specific, column with *Form Maker* field names,
#'   which may be added to `variables_file`.
#'
#' @examples
#' # an example variables_file and translations_file might be helpful
#'
#' @export
fm_read <- function(data_file, variables_file, translations_file = NULL, lang) {
     data <- readr::read_csv(data_file, col_types = readr::cols(.default = "c"),
                             name_repair = "unique_quiet", show_col_types = FALSE)

     keys <- readr::read_csv(variables_file, na = "NA",
                             name_repair = "unique_quiet", show_col_types = FALSE)
     keys <- keys[keys$Label != "", ]
     keys <- keys[keys$Import != "", ]

     data <- data.table::setnames(data, keys[[lang]], keys$Label, skip_absent = TRUE)
     data <- data %>% dplyr::select(tidyselect::any_of(keys$Label))

     #Translations
     if(! is.null(translations_file)) {
          trans <- readr::read_csv(translations_file, na = "NA",
                                   name_repair = "unique_quiet", show_col_types = FALSE)
          trans$x <- trans[[lang]]
          trans <- trans[, c("Translation", "x", "Label")]
          trans <- trans[trans$x != "", ]

          for (i in 1:nrow(trans)){
                  if(trans$Label[i] == "") data[data == trans$x[i]] <- trans$Translation[i]
                  else dplyr::if_else(data[[trans$Label[i]]] == trans$x[i],
                                      trans$Translation[i],
                                      data[[trans$Label[i]]]) -> data[[trans$Label[i]]]
          }
     }

     #Type conversion
     for(type in keys$Type) {
          if(type == "") next
          if(substr(type, 1, 1) == "%") {
               data[dplyr::pull(keys[keys$Type == type, "Label"])] <- lapply(
                    data[dplyr::pull(keys[keys$Type == type, "Label"])], function(x) as.POSIXct(x, format=type)
               )
          } else {
               data[dplyr::pull(keys[keys$Type == type, "Label"])] <- lapply(
                    data[dplyr::pull(keys[keys$Type == type, "Label"])], paste0("as.", type)
               )
          }
     }
     return(data)
}


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
#' @param simplified A logical value. If set to `TRUE` (default), the resulting tibble
#'   contains all the columns required by [fm_read()]
#'   ("Import", "Label", and "Type" filled with `NA`s).
#'   If set to `FALSE` returns a single column tibble
#'   (only column `lang` with field names).
#' @param target_file A `character`, an optional name of file to which
#'   the resulting tibble will be saved.
#'
#' @details For field names (column names of the *FM* responses file)
#'   it basically applies [vctrs::vec_as_names()] with `repair = "unique"`,
#'   adding "...j" to each duplicated column, where `j` is the column position
#'   (consistent with [readr::read_csv()]'s default behaviour used in [fm_read()].
#'
#' @returns A tibble with `lang` and optionally "Import", "Label", and "Type" columns.
#'   If `target_file` is specified, returns invisibly.
#'
#' @seealso [fm_read()]
#'
#' @export
fm_variables <- function(source_file, lang, simplified = FALSE, target_file = NULL) {
        readr::read_csv(source_file, n_max = 1, col_names = FALSE,
                        show_col_types = FALSE) %>%
        dplyr::slice_head(n = 1) %>% as.character() %>%
        vctrs::vec_as_names(repair = "unique") %>% as.vector() -> var_df
        tibble::tibble(lang = var_df) -> var_df
        if(! simplified) tibble::add_column(var_df,
                                            Import = NA, Label = NA, Type = NA, .before = 1) ->
                var_df
        if(is.null(target_file)) return(var_df)
        else {
                readr::write_csv(var_df, target_file, na = "")
                invisible(var_df)
        }
}
