# Processing FormMaker data dumped from MySQL

form_name <- "IRMIK3"
form_file_name <- "wor2969_formmaker.csv"
responses_file_name <- "wor2969_formmaker_submits_20250215.csv"

readr::read_csv(form_file_name, col_select=c(id, title, form_fields)) |>
     dplyr::filter(title == form_name) -> form

form_fields <- tibble::as_tibble(stringr::str_match_all(form$form_fields, "(?:^|\\*)(?<item>\\d+)\\*:\\*id\\*.+?(?<label>[^*]+)\\*:\\*w_field_label")[[1]][, 2:3])

readr::read_csv(responses_file_name) |> dplyr::filter(form_id == form$id) |>
     dplyr::mutate(element_label = as.character(element_label)) |>
     dplyr::left_join(form_fields, dplyr::join_by(element_label == item)) -> responses

