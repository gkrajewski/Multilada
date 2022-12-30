#' @export
cdi_forms <- function(database = NULL) {
     connection <- multilada_connect(database, "CDI database (dev or prod)")
     forms <- RMariaDB::dbGetQuery(connection, "SHOW TABLES")
     RMariaDB::dbDisconnect(connection)
     forms <- stringr::str_match(forms[, 1], "^form_(.+)")[, 2]
     return(forms[complete.cases(forms)])
}

#' @export
cdi_read <- function(form, database = NULL) {
     statement <- paste0("SELECT * FROM `form_", form, "`")
     connection <- multilada_connect(database, "CDI database (dev or prod)")
     cdi <- RMariaDB::dbGetQuery(connection, statement) %>%
             dplyr::mutate(cdi, across(where(is.character), as.factor))
     RMariaDB::dbDisconnect(connection)
     return(cdi)
}
