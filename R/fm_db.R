#### Processing backup of WP database ####

brew_command <- "/opt/homebrew/bin/brew"
mysql_command <- "/opt/homebrew/bin/mysql"
mysql_user <- "root"
mysql_database <- "multilada_forms"
# Get the most recent dump file (if any):
if(is.na(mysql_dumpfile <- rev(sort(list.files(pattern = "^backup_.*_MultiLADA_UW_Forms_.*-db$")))[1]))
     stop("No backup files found.")

# Import the backup from a shell script:
system2("/bin/bash", args = c("multilada_forms_import.sh",
                              brew_command, mysql_command, mysql_user, mysql_database, mysql_dumpfile))

RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = mysql_database, username = mysql_user) -> connection
RMariaDB::dbGetQuery(connection, "SELECT * FROM `wor2969_formmaker_submits`") -> formmaker_submits
RMariaDB::dbGetQuery(connection, "SELECT * FROM `wor2969_formmaker`") -> formmaker_forms
RMariaDB::dbDisconnect(connection)


#### Processing FormMaker data dumped from MySQL ####

form_file_name <- "wor2969_formmaker.csv"
responses_file_name <- "wor2969_formmaker_submits_20250215.csv"

readr::read_csv(form_file_name) -> formmaker_forms
readr::read_csv(responses_file_name) -> formmaker_submits


#### Extracting given form and its data ####

form_name <- "PABIQ-SCR-PL - Copy"

formmaker_forms |>
     dplyr::select(id, title, form_fields) |>
     dplyr::filter(title == form_name) -> form
form_fields <- tibble::as_tibble(stringr::str_match_all(form$form_fields, "(?:^|\\*)(?<item>\\d+)\\*:\\*id\\*.+?(?<label>[^*]+)\\*:\\*w_field_label")[[1]][, 2:3])

formmaker_submits |>
     dplyr::filter(form_id == form$id) |>
     dplyr::mutate(element_label = as.character(element_label)) |>
     dplyr::left_join(form_fields, dplyr::join_by(element_label == item)) -> responses

