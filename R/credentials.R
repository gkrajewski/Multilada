#' Get or set credentials securely
#'
#' `multilada_credentials()` gets a list of secret credentials (*elements*). It
#' can ask for them in a secure way and (optionally) save them to a system-based
#' credential store (e.g., *Keychain* on macOS), or retrieve them from the
#' system-based credential store if they already are stored there.
#'
#' We use package `keyring` to save and securely retrieve secret credentials.
#' `keyring` is supposed to be system-independent and should work on macOS,
#' Windows and Linux but I have tested it only with macOS's *Keychain*.
#'
#' Each *secret* in the system-based credential store is identified by a
#' *service* name and may optionally have a *username* attached (because the
#' most typical use case is to store a *secret password* for a *username* in a
#' given *service*). We use the same *service* name (e.g., "pn-recruitment") for
#' all credentials to bundle them together and we use *usernames* to distinguish
#' them from each other (e.g., "host", "port").
#'
#' You can access and check the stored credentials manually. On macOS you will
#' use the Keychain Access app. It shows *service* as *name*, *username* as
#' *account*, and all our entries are classified as *application passwords*
#' (column *Kind*). I have no idea how it is implemented in other systems
#' though.
#'
#'
#' @param key A `character`, the identifier of all *elements* that go together,
#'   e.g., it could be "pn-recruitment" for all credentials needed to access the
#'   PolkaNorski recruitment database.
#'
#'   If `key = NULL` (the default), the function only asks for the *secret* for
#'   each *element* (in a secure way, e.g., masking the responses as they are
#'   typed), without using the system credential store (and package `keyring`).
#'
#'   If `key` is provided and there is a *secret* stored for a given *element*
#'   and a given *key* in the system credential store, the *secret* is
#'   retrieved. Otherwise, the function asks for the *secret* for that *element*
#'   (in a secure way, e.g., masking the responses as they are typed) and saves
#'   it in the system credential store.
#'
#'   In the `keyring` package `key` is called a *service*, in macOS Keychain it
#'   is a *name* etc.
#'
#' @param elements A character vector of names of all credentials needed.
#'   Defaults to `c("host", "port", "name", "username", "password")`, which
#'   covers all credentials required to access a database (but we might decide
#'   to un-*secret*, e.g., the port number to PN Recruitment database and
#'   hard-code it in our scripts).
#'
#' @param prompt A `character`, the prefix to a prompt displayed, when the
#'   function needs to ask for a *secret* for a given *element* (either because
#'   `key = NULL`, or because there is no *secret* for a given *element* and a
#'   given *key*).
#'
#'   Defaults to "Database" which, when asking for, e.g., the "host", would
#'   display "Database host: " as the promopt.
#'
#' @returns A named list of *secrets* for all *elements* with *elements*
#' used as names.
#'
#' **Secrets returned as unmasked `characters` (i.e., plain text).**
#'
#' @examples
#' # To ask for all credentials needed to access the PolkaNorski Recruitment database:
#' pn_rec_credentials <- multilada_credentials(prompt = "PN Recruitment database")
#' # ---
#' # To use the system credential store:
#' \dontrun{
#' pn_rec_credentials <- multilada_credentials("pn-recruitment", prompt = "PN Recruitment database")
#' }
#' # If a given element (e.g., "host") is not yet stored in the system credential store,
#' # the function asks for it and saves it to the store, so that next time it can be retrieved
#' # without asking again.
#' # ---
#' # To access a single credentials element, e.g., the port number labeled as "port":
#' pn_rec_credentials$port
#'
#' @export
multilada_credentials <- function(key = NULL, elements = c("host", "port", "name", "username", "password"),
                                  prompt = "Database") {
        sapply(X = elements,
               FUN = multilada_credentials_element, key, prompt, simplify = FALSE)
}

#' Get or set credentials securely
#'
#' The workhorse of `multilada_credentials()`.
#'
#' @export
multilada_credentials_element <- function(element, key, prompt) {
        full_prompt <- paste0(prompt, " ", element, ": ")
        if(is.null(key)) {
                askpass::askpass(full_prompt)
        } else {
                if(! element %in% keyring::key_list(key)$username) keyring::key_set(key, element, prompt = full_prompt)
                keyring::key_get(key, element)
        }
}

#' Create encrypted credentials file
#'
#' Gets credentials securely and saves them in encrypted file.
#' **File backend for `keyring` doesn't work on Windows**.
#'
#' @export
multilada_credentials_file_set <- function(key, elements = c("host", "port", "name", "username", "password"),
                                           prompt = "Database") {
        file_name <- paste0(rappdirs::user_config_dir("r-keyring"), "/", key, ".keyring")
        if(file.exists(file_name)) file.remove(file_name)
        multilada_filekey <- keyring::backend_file$new(key)
        multilada_filekey$keyring_create(key)
        sapply(X = elements,
               FUN = function(x) {
                       full_prompt <- paste0(prompt, " ", x, ": ")
                       multilada_filekey$set(key, x, prompt = full_prompt)
               }
        )
        multilada_filekey$keyring_lock(key)
        print(paste("Credentials saved to", file_name))
}

#' Check encrypted credentials file's location
#'
#' **File backend for `keyring` doesn't work on Windows**.
#'
#' @export
multilada_credentials_file_where <- function(key = NULL) {
        print(paste0("Credentials file should be here: ", rappdirs::user_config_dir("r-keyring"), "/"))
        if(! is.null(key)) {
                if(file.exists(paste0(rappdirs::user_config_dir("r-keyring"), "/", key, ".keyring"))) {
                        print(paste0(key, '.keyring is there.'))
                } else {
                        print(paste0(key, '.keyring is not there.'))
                }
        }
}

#' Get credentials from encrypted file
#'
#' **File backend for `keyring` doesn't work on Windows**.
#'
#' @export
multilada_credentials_file_get <- function(key, elements = c("host", "port", "name", "username", "password")) {
        file_name <- paste0(rappdirs::user_config_dir("r-keyring"), "/", key, ".keyring")
        if(! file.exists(file_name)) stop(paste(file_name, "not found."))
        multilada_filekey <- keyring::backend_file$new(key)
        if(! key %in% multilada_filekey$list()$service) stop(paste(file_name, "corrupted."))
        if(! all(elements %in% multilada_filekey$list()$username)) stop(paste(file_name, "corrupted."))
        credentials <- sapply(X = elements,
                              FUN = multilada_filekey$get, service = key, simplify = FALSE)
        multilada_filekey$keyring_lock(key)
        return(credentials)
}

