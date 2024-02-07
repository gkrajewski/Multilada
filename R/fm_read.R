#' Read and preprocess *WP Form Maker* data
#'
#' Reads a `csv` file downloaded using the "Export to CSV" option
#' of the *WP Form Maker* plugin.
#'
#' @param data_file description
#' @param variables_file description
#' @param translations_file description
#' @param lang description
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
fm_read <- function(data_file, variables_file, translations_file = NULL, lang) {
     data <- utils::read.csv(data_file)

     keys <- utils::read.csv(variables_file)
     keys <- keys[keys$Label != "", ]
     keys <- keys[keys$If.needed != "", ]

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
     for(type in keys$Type.import) {
          if(type == "") next
          if(substr(type, 1, 1) == "%") {
               data[keys[keys$Type.import == type, "Label"]] <- lapply(
                    data[keys[keys$Type.import == type, "Label"]], function(x) as.POSIXct(x, format=type)
               )
          } else {
               data[keys[keys$Type.import == type, "Label"]] <- lapply(
                    data[keys[keys$Type.import == type, "Label"]], paste0("as.", type)
               )
          }
     }
     return(data)
}
