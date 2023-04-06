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
#'   For `cdi_read()` a `dataframe` with all responses for a given form. The format of the responses mirrors
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
     return(cdi)
}

#' List IDs of all submissions together with start dates, end dates, and filling times
#'
#' Useful to get a summary of submissions.
#' `cdi_time()` is an alias for backward compatibility.
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param run A logical value indicating whether to include column "run".
#'
#' @param form A logical value indicating whether to include column "form".
#'
#' @param sort.by.end A logical value indicating whether to sort by end date
#'   (by default the resulting `tibble` is sorted by start date).
#'
#' @returns A `tibble` with columns: "id", "start" ("start_date" in the `data` dataframe),
#'   "end" ("end_date" in the `data` dataframe), "duration"
#'   (difference between "end_date" and "start_date" in the `data` dataframe), and
#'   optionally "run".
#'
#' @export
cdi_submissions <- function(data, run = FALSE, form = FALSE, sort.by.end = FALSE) {
        data %>% dplyr::group_by(.data$id) %>%
                dplyr::summarise(run = .data$run,
                                 form = .data$form,
                                 start = .data$start_date,
                                 end = .data$end_date,
                                 duration = lubridate::as.duration(.data$end_date - .data$start_date)) %>%
                dplyr::distinct() -> data
        if(! run) data %>% dplyr::select(- .data$run) -> data
        if(! form) data %>% dplyr::select(- .data$form) -> data
        if(sort.by.end) data %>% dplyr::arrange(.data$end) else
                        data %>% dplyr::arrange(.data$start)
}

#' @rdname cdi_submissions
#' @export
cdi_time <- cdi_submissions

#' Count responses to given question type in CDI-Online form
#'
#' These functions count (given) responses to questions of a given type for each ID.
#' Types (aka `answer_type`s) implemented so far are:
#' * `oneCheckboxGroup`: type used for simple checkbox lists (e.g., word lists in *CDI:WS*, *CDI-III*);
#' * `manyCheckboxGroups`: type used for checkbox lists with two options (e.g., word list in *CDI:WG*);
#' * `checkboxAlt`: type used for checkbox alternatives (e.g., some *Complexity* sections);
#' * `radio`: traditional radio button question type.
#'
#' @param data A `dataframe` containing answers to a *CDI-Online* form,
#'   e.g., as returned by [cdi_read()] (or obtained otherwise).
#'
#' @param type A `character`, the value of the `type` column of the `data` dataframe,
#'   e.g., "word" for `oneCheckboxGroup`, "alternatives" for `checkboxAlt`.
#'
#' @param category A `character`, the value of the `category` column of the `data` dataframe,
#'   e.g., "animals", "action_words", or "quantifiers" for the type "word" in *CDI:WG* or *CDI:WS*.
#'   If set, only answers in this category are counted. Otherwise answers for the whole type are considered.
#'
#' @param answer A `character` specifing which asnwers to count.
#'   For `checkboxAlt` and `manyCheckboxGroups` possible values are
#'   "first" (default), "second", "both", or "none"
#'   (for `manyCheckboxGroups` "first" usually means *understands* and "second" means *produces*),
#'   For `radio` it should be an integer
#'   (coerced to `character`) indicating the option to count and defaulting to "1"
#'   (which usually codes "yes" in *yes or no* questions).
#'
#' @details To find out what `type` or `category` you are looking for, check the `data` dataframe or
#'  `items.csv` for a given form in the `www/languages/` of your *CDI-Online* installation.
#'   To find out which function to use, check the `answer_type` column in the `data` dataframe or
#'   the `type` column in `settings.csv` for a given form.
#'
#'   `type` is the highest level in the hierarchy of items in *CDI-Online* forms.
#'   `category` is a sub-`type` and also a basic level of the item hierarchy,
#'   displayed as a single page. Hence sometimes `categories` are meaningful
#'   but sometimes they are used only for the sake of the UX:
#'   they divide longer `types` into a number of pages, e.g., "complexity1", "complexity2" etc.
#'   (as a matter of fact `type` can also be used just for that reason,
#'   e.g., "gestures_first", "gestures_games" etc. without overarching "gestures").
#'
#' @returns A `dataframe` with columns: "id", "n" (resulting count), "type" (the argument to the function,
#' not `answer_type`), "answer" (if applicable), and "category" (if not `NULL`).
#'
#' @examples
#' \dontrun{
#' cdi_count_oneCheckboxGroup(ws_pl, "word")
#' cdi_count_oneCheckboxGroup(ws_pl, "word", "vehicles")
#' cdi_count_checkboxAlt("cdi3-1_pl", "alternatives")
#' }
#'
#' @export
cdi_count_checkboxAlt <- function(data, type, category = NULL, answer = "first") {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "checkboxAlt") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        if(answer == "first") data %>% dplyr::filter(.data$answer1 == "1") -> data
        if(answer == "second") data %>% dplyr::filter(.data$answer2 == "1") -> data
        if(answer == "both") data %>% dplyr::filter(.data$answer1 == "1" & .data$answer2 == "1") -> data
        if(answer == "none") data %>% dplyr::filter(.data$answer1 == "0" & is.na(.data$answer2)) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @rdname cdi_count_checkboxAlt
#' @export
cdi_count_manyCheckboxGroups <- function(data, type, category = NULL, answer = "first") {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "manyCheckboxGroups") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        if(answer == "first") data %>% dplyr::filter(.data$answer1 == "1") -> data
        if(answer == "second") data %>% dplyr::filter(.data$answer2 == "1") -> data
        if(answer == "both") data %>% dplyr::filter(.data$answer1 == "1" & .data$answer2 == "1") -> data
        if(answer == "none") data %>% dplyr::filter(.data$answer1 == "0" & is.na(.data$answer2)) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @rdname cdi_count_checkboxAlt
#' @export
cdi_count_radio <- function(data, type, category = NULL, answer = "1") {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "radio") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        data %>% dplyr::filter(.data$answer1 == answer) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @rdname cdi_count_checkboxAlt
#' @export
cdi_count_oneCheckboxGroup <- function(data, type, category = NULL) {
        data %>% dplyr::select(.data$id) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "oneCheckboxGroup") -> data
        if(!is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        data %>% dplyr::group_by(.data$id) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' Add item definitions to `oneCheckboxGroup` data
#'
#' Results for `oneCheckboxGroup` items (e.g., word lists in *CDI:WS*, *CDI-III*)
#' contain only checked items with their sequence numbers within a category, i.e., a single page
#' without the rest of items and item definitions.
#' `cdi_itemise_oneCheckboxGroup()` adds these missing information,
#' which is helpful for item-level analyses.
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param items A `dataframe` containing item definitions for a *CDI-Online* form, e.g., as imported from
#'   `items.csv` file found in the appropriate subfolder of `www/languages/` in your *CDI-Online* installation
#'   (required columns are `type`, `category`, and `definition`).
#'
#' @returns A `dataframe` with columns: "id", "type", "category", "item_id" (sequence number within category),
#' "response" (0 or 1), and "definition", as well as any other columns contained in `items`.
#'
#' @export
cdi_itemise_oneCheckboxGroup <- function(data, items) {
        items %>% dplyr::filter(.data$type == "word") %>%
                dplyr::group_by(.data$category) %>%
                dplyr::mutate(item_id = dplyr::row_number()) -> items
        data %>% dplyr::filter(.data$type == "word" & .data$answer_type == "oneCheckboxGroup") %>%
                dplyr::select(.data$id, .data$type, .data$category, .data$answer1, .data$answer2) -> data
        data$answer1 <- as.integer(as.character(data$answer1)) # Comments are saved in this column as well
        data$answer2 <- 1 # For proper response coding (1 vs 0)
        data %>% dplyr::group_by(.data$id) %>% dplyr::group_modify(~
                dplyr::full_join(.x, items, by = c("type" = "type", "category" = "category", "answer1" = "item_id"))
        ) -> data
        data %>% dplyr::rename(item_id = .data$answer1, response = .data$answer2) %>% tidyr::replace_na(list(response=0))
}

#' Add item definitions to `checkboxAlt` data
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param items A `dataframe` containing item definitions for a *CDI-Online* form, e.g., as imported from
#'   `items.csv` file found in the appropriate subfolder of `www/languages/` in your *CDI-Online* installation
#'   (required columns are `type`, `category`, and `definition`).
#'
#' @returns A `dataframe` with columns: "id", "type", "category", "item_id" (sequence number within category),
#' "answer1" (0 or 1), "answer2" (0 or 1), and "definition", as well as any other columns contained in `items`.
#'
#' @export
cdi_itemise_checkboxAlt <- function(data, type, items) {
        items %>% dplyr::filter(.data$type == {{type}}) %>%
                dplyr::group_by(.data$category) %>%
                dplyr::mutate(item_id = dplyr::row_number()) -> items
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "checkboxAlt") %>%
                dplyr::select(.data$id, .data$type, .data$category, .data$answer_id, .data$answer1, .data$answer2) -> data
        data$answer1 <- as.integer(as.character(data$answer1)) # Comments are saved in this column as well
        data$answer2 <- as.integer(as.character(data$answer2)) # Not sure why it's loaded as a factor
        data$answer_id <- as.integer(as.character(data$answer_id)) # Not sure why it's loaded as a factor
        data %>% dplyr::group_by(.data$id) %>% dplyr::group_modify(~
                dplyr::full_join(.x, items, by = c("type" = "type", "category" = "category", "answer_id" = "item_id"))
        ) -> data
        data %>% dplyr::rename(item_id = .data$answer_id) %>% tidyr::replace_na(list(answer2=0))
}

#' Add item definitions to `radio` data
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param items A `dataframe` containing item definitions for a *CDI-Online* form, e.g., as imported from
#'   `items.csv` file found in the appropriate subfolder of `www/languages/` in your *CDI-Online* installation
#'   (required columns are `type`, `category`, and `definition`).
#'
#' @returns A `dataframe` with columns: "id", "type", "category", "item_id" (sequence number within category),
#' "answer" (number coded), and "definition", as well as any other columns contained in `items`.
#'
#' @export
cdi_itemise_radio <- function(data, type, items) {
        items %>% dplyr::filter(.data$type == {{type}}) %>%
                dplyr::group_by(.data$category) %>%
                dplyr::mutate(item_id = dplyr::row_number()) -> items
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "radio") %>%
                dplyr::select(.data$id, .data$type, .data$category, .data$answer_id, .data$answer1) -> data
        data$answer1 <- as.integer(as.character(data$answer1)) # Comments are saved in this column as well
        data$answer_id <- as.integer(as.character(data$answer_id)) # Not sure why it's loaded as a factor
        data %>% dplyr::group_by(.data$id) %>% dplyr::group_modify(~
                dplyr::full_join(.x, items, by = c("type" = "type", "category" = "category", "answer_id" = "item_id"))
        ) -> data
        data %>% dplyr::rename(item_id = .data$answer_id, answer = .data$answer1)
}

