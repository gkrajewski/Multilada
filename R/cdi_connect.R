#' Connect to a CDI-Online database
#'
#' `cdi_forms()` gets a list of all *CDI-Online* forms stored as tables
#' with saved responses in a given database.
#' `cdi_read()` retrieves all responses to a given form (a table in a database).
#'
#' The underlying workhorse for connecting to a database is [multilada_connect()]
#'   and ultimately [multilada_credentials()] for dealing with database credentials.
#'   Check the latter to better understand the credentials asking/saving/retrieving details.
#'
#' @importFrom rlang .data
#'
#' @param database A `character` identifying a Multilada database or, to be precise,
#'   all secret credentials needed to connect to the database (e.g., "host", "port").
#'   If `database = NULL` (the default), you are asked to provide each credential when prompted.
#'   Otherwise the function attempts to retrieve the credentials for the database from
#'   the system-based credential store (e.g., *Keychain* on macOS). If it fails to find
#'   a given credential, you are asked to provide it and then
#'   it is saved in the credential store for future use.
#'
#' @param form A `character`, name of the table to read. It's best to run cdi_forms() first
#'   to see available options.
#'
#' @returns For `cdi_forms()` a `character` vector of table names for *CDI-Online* forms.
#'   *CDI-Online* app creates a name for a table by appending the value of parameter `lang`
#'   to the value of parameter `form` using "_" as a separator.
#'
#'   For `cdi_read()` a tibble with all responses for a given form. All character columns
#'   are coerced to factors. The format of the responses mirrors
#'   the way *CDI-Online* app saves them to a table in a database.
#'
#' @examples
#' # To print list of names of all CDI forms stored in a given database:
#' \dontrun{
#' cdi_forms("cdi")}
#' # If you have already saved the credentials needed to connect
#' # to that database under the name "cdi" in your system,
#' # they will be retrevied. Otherwise you will be asked for them
#' # and they will be saved under the name "cdi" for future use.
#' # ---
#' #
#' # If there is a form saved under the name "ws_pl" in that database,
#' # to import it too a dataframe:
#' \dontrun{
#' ws_pl <- cdi_read("cdi", "ws_pl")}
#'
#' @export
cdi_forms <- function(database = NULL) {
     connection <- multilada_connect(database, "CDI database")
     forms <- RMariaDB::dbGetQuery(connection, "SHOW TABLES")
     RMariaDB::dbDisconnect(connection)
     forms <- stringr::str_match(forms[, 1], "^form_(.+)")[, 2]
     return(forms[stats::complete.cases(forms)])
}

#' @rdname cdi_forms
#'
#' @export
cdi_read <- function(database = NULL, form) {
     statement <- paste0("SELECT * FROM `form_", form, "`")
     connection <- multilada_connect(database, "CDI database")
     cdi <- RMariaDB::dbGetQuery(connection, statement) %>%
             dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor))
     RMariaDB::dbDisconnect(connection)
     return(tibble::as_tibble(cdi))
}
