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
#' @param items A data frame containing item definitions for a *CDI-Online* form, e.g., as imported from
#'   `items.csv` file found in the appropriate subfolder of `www/languages/` in your *CDI-Online* installation
#'   (required columns are `type`, `category`, and `definition`).
#'
#' @returns A data frame with columns: "id", "type", "category", "item_id" (sequence number within category),
#' "response" (0 or 1), and "definition", as well as any other columns contained in `items`.
#'
#' @export
cdi_itemise_oneCheckboxGroup <- function(data, items) {
        items %>% dplyr::filter(.data$type == "word") %>%
                dplyr::group_by(.data$category) %>%
                dplyr::mutate(item_id = dplyr::row_number()) -> items
        data %>% dplyr::filter(.data$type == "word" & .data$answer_type == "oneCheckboxGroup") %>%
                dplyr::select(.data$id, .data$end_date,
                              .data$type, .data$category, .data$answer1, .data$answer2) -> data
        data$answer1 <- as.integer(as.character(data$answer1)) # Comments are saved in this column as well
        data$answer2 <- 1 # For proper response coding (1 vs 0)
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::group_modify(~
                dplyr::full_join(.x, items, by = c("type" = "type", "category" = "category", "answer1" = "item_id"))
        ) -> data
        data %>% dplyr::rename(item_id = .data$answer1, response = .data$answer2) %>% tidyr::replace_na(list(response=0))
}

#' Add item definitions to `checkboxAlt` data
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param items A data frame containing item definitions for a *CDI-Online* form, e.g., as imported from
#'   `items.csv` file found in the appropriate subfolder of `www/languages/` in your *CDI-Online* installation
#'   (required columns are `type`, `category`, and `definition`).
#'
#' @returns A data frame with columns: "id", "type", "category", "item_id" (sequence number within category),
#' "answer1" (0 or 1), "answer2" (0 or 1), and "definition", as well as any other columns contained in `items`.
#'
#' @export
cdi_itemise_checkboxAlt <- function(data, type, items) {
        items %>% dplyr::filter(.data$type == {{type}}) %>%
                dplyr::group_by(.data$category) %>%
                dplyr::mutate(item_id = dplyr::row_number()) -> items
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "checkboxAlt") %>%
                dplyr::select(.data$id, .data$end_date,
                              .data$type, .data$category, .data$answer_id, .data$answer1, .data$answer2) -> data
        data$answer1 <- as.integer(as.character(data$answer1)) # Comments are saved in this column as well
        data$answer2 <- as.integer(as.character(data$answer2)) # Not sure why it's loaded as a factor
        data$answer_id <- as.integer(as.character(data$answer_id)) # Not sure why it's loaded as a factor
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::group_modify(~
                dplyr::full_join(.x, items, by = c("type" = "type", "category" = "category", "answer_id" = "item_id"))
        ) -> data
        data %>% dplyr::rename(item_id = .data$answer_id) %>% tidyr::replace_na(list(answer2=0))
}

#' Add item definitions to `radio` data
#'
#' @inheritParams cdi_count_checkboxAlt
#'
#' @param items A data frame containing item definitions for a *CDI-Online* form, e.g., as imported from
#'   `items.csv` file found in the appropriate subfolder of `www/languages/` in your *CDI-Online* installation
#'   (required columns are `type`, `category`, and `definition`).
#'
#' @returns A data frame with columns: "id", "type", "category", "item_id" (sequence number within category),
#' "answer" (number coded), and "definition", as well as any other columns contained in `items`.
#'
#' @export
cdi_itemise_radio <- function(data, type, items) {
        items %>% dplyr::filter(.data$type == {{type}}) %>%
                dplyr::group_by(.data$category) %>%
                dplyr::mutate(item_id = dplyr::row_number()) -> items
        data %>% dplyr::filter(.data$type == {{type}} & .data$answer_type == "radio") %>%
                dplyr::select(.data$id, .data$end_date,
                              .data$type, .data$category, .data$answer_id, .data$answer1) -> data
        data$answer1 <- as.integer(as.character(data$answer1)) # Comments are saved in this column as well
        data$answer_id <- as.integer(as.character(data$answer_id)) # Not sure why it's loaded as a factor
        data %>% dplyr::group_by(.data$id, .data$end_date) %>% dplyr::group_modify(~
                dplyr::full_join(.x, items, by = c("type" = "type", "category" = "category", "answer_id" = "item_id"))
        ) -> data
        data %>% dplyr::rename(item_id = .data$answer_id, answer = .data$answer1)
}
