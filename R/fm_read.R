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
#'   Anything else is quietly ignored (unless causes [read.csv()] to throw an error).
#'   See ‘Details’.
#' @param translations_file A `character`, the name of the (optional) "translations" file.
#'   It should be a `csv` file containing at least a column named "Translation"
#'   and a column named `lang`, which contains original values (for a given language)
#'   to be translated. Anything else is quietly ignored
#'   (unless causes [read.csv()] to throw an error).
#' @param lang A `character`, the name of language specific columns in `variables_file`
#'   and `translations_file`. It may be, e.g., a language ISO code ("no", "pl" etc.).
#'
#' @details Reading import settings from an external `csv` file (`variables_file`)
#'   rather than setting them directly in *R* might seem inconvenient at least but
#'   this way it is easy to share and modify the settings across projects, versions,
#'   and languages. `variables_file` should be treated as a "dictionary" file,
#'   with columns containing original *Form Maker* form field names for
#'   each language version and a "Label" column with language independent target
#'   variable names.
#'
#'   Only columns with labels provided in the "Label" column are imported.
#'   Additionally, there is the "Import" column to quickly switch on and off importing:
#'   variables for which this column is empty are not imported. Language specific
#'   field names in `variables_file` should look exactly like column names of
#'   a data frame returned by `read.csv(data_file)`, **not** like they look **in**
#'   `data_file` downloaded from the website.
#'
#'   The "Type" column of `variables_file` may be used to convert the type
#'   of the imported column (probably `numeric` or `character`). It should contain
#'   the name of the required type ("type" so that `as.type()` exists) or,
#'   if the imported column contains strings to be converted to a date or
#'   date-time type, it should contain the expected format of the string,
#'   as used by [strptime()], i.e., the value of `format` argument to
#'   [as.POSIXct()].
#'
#'   If `translations_file` is provided, all occurrences of values in its
#'   `lang` column in the imported data frame are changed to the corresponding
#'   values in its "Translation" column. So far this is not a column-wise
#'   operation (e.g., translating *ja*'s to *yes*'es across all the columns).
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
#' @returns A data frame imported from `data_file` with column names, values, and
#'   types changed as described above.
#'
#' @examples
#' # an example variables_file and translations_file might be helpful
#'
#' @export
fm_read <- function(data_file, variables_file, translations_file = NULL, lang) {
     data <- utils::read.csv(data_file)

     keys <- utils::read.csv(variables_file)
     keys <- keys[keys$Label != "", ]
     keys <- keys[keys$Import != "", ]

     data <- data.table::setnames(data, keys[, lang], keys$Label, skip_absent = TRUE)
     data <- data %>% dplyr::select(tidyselect::any_of(keys$Label))

     if(! is.null(translations_file)) {
          trans <- utils::read.csv(translations_file)
          trans$x <- trans[, lang]
          trans <- trans[, c("Translation", "x")]

          trans <- trans[trans$x != "", ]
          for (i in 1:nrow(trans)){
               data[data == trans$x[i]] <- trans$Translation[i]
          }
     }

     #Type conversion
     for(type in keys$Type) {
          if(type == "") next
          if(substr(type, 1, 1) == "%") {
               data[keys[keys$Type == type, "Label"]] <- lapply(
                    data[keys[keys$Type == type, "Label"]], function(x) as.POSIXct(x, format=type)
               )
          } else {
               data[keys[keys$Type == type, "Label"]] <- lapply(
                    data[keys[keys$Type == type, "Label"]], paste0("as.", type)
               )
          }
     }
     return(data)
}
