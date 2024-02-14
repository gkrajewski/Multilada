#' Count responses to given question type in CDI-Online form
#'
#' These functions count (given) responses to questions of a given type for each ID.
#' Types (aka `answer_type`s) implemented so far are:
#' * `oneCheckboxGroup`: type used for simple checkbox lists (e.g., word lists in *CDI:WS*, *CDI-III*);
#' * `manyCheckboxGroups`: type used for checkbox lists with two options (e.g., word list in *CDI:WG*);
#' * `checkboxAlt`: type used for checkbox alternatives (e.g., some *Complexity* sections);
#' * `radio`: traditional radio button question type.
#'
#' @param data A data frame containing answers to a *CDI-Online* **static** form,
#'   as returned by [cdi_read()] (or obtained otherwise).
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
#'   "first", "second", "both", or "none".
#'   For `manyCheckboxGroups` "first", default, usually means *understands* and "second" means *produces*.
#'   For `checkboxAlt` "second", default, usually means more complex alternative and "first" means the simpler one.
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
#' @returns A data frame with columns: "id", "n" (resulting count), "type" (the argument to the function,
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
cdi_count_checkboxAlt <- function(data, type, category = NULL, answer = "second") {
        data %>% dplyr::select(.data$id, .data$end_date) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "checkboxAlt") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        if(answer == "first") data %>% dplyr::filter(.data$answer1 == "1") -> data
        if(answer == "second") data %>% dplyr::filter(.data$answer2 == "1") -> data
        if(answer == "both") data %>% dplyr::filter(.data$answer1 == "1" & .data$answer2 == "1") -> data
        if(answer == "none") data %>% dplyr::filter(.data$answer1 == "0" & is.na(.data$answer2)) -> data
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @rdname cdi_count_checkboxAlt
#' @export
cdi_count_manyCheckboxGroups <- function(data, type, category = NULL, answer = "first") {
        data %>% dplyr::select(.data$id, .data$end_date) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "manyCheckboxGroups") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        if(answer == "first") data %>% dplyr::filter(.data$answer1 == "1") -> data
        if(answer == "second") data %>% dplyr::filter(.data$answer2 == "1") -> data
        if(answer == "both") data %>% dplyr::filter(.data$answer1 == "1" & .data$answer2 == "1") -> data
        if(answer == "none") data %>% dplyr::filter(.data$answer1 == "0" & is.na(.data$answer2)) -> data
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @rdname cdi_count_checkboxAlt
#' @export
cdi_count_radio <- function(data, type, category = NULL, answer = "1") {
        data %>% dplyr::select(.data$id, .data$end_date) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "radio") -> data
        if(! is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        data %>% dplyr::filter(.data$answer1 == answer) -> data
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(answer = answer, category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}

#' @rdname cdi_count_checkboxAlt
#' @export
cdi_count_oneCheckboxGroup <- function(data, type, category = NULL) {
        data %>% dplyr::select(.data$id, .data$end_date) %>% dplyr::distinct() -> id
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "oneCheckboxGroup") -> data
        if(!is.null(category)) data %>% dplyr::filter(.data$category == {{category}}) -> data
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::count() -> data
        dplyr::left_join(id, data) -> data
        data %>% dplyr::mutate(category = category, type = type) %>%
                tidyr::replace_na(list(n = 0))
}
