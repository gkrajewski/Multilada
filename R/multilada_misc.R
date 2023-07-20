#' Calculate age in months
#'
#' Properly (!) calculates age in months given date of birth and submission (testing etc.) date.
#'
#' @param dob,submission_date Vectors of dates in the "ymd" format.
#'
#' @details No argument testing yet implemented. They need to follow Ymd or ymd format strictly.
#'
#' @returns A `numeric` vector.
#' @export
age_months <- function(dob, submission_date) {
     floor(lubridate::interval(dob, submission_date) / months(1))
}
