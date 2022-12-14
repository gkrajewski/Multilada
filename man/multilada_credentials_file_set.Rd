% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/credentials.R
\name{multilada_credentials_file_set}
\alias{multilada_credentials_file_set}
\title{Create encrypted credentials file}
\usage{
multilada_credentials_file_set(
  key,
  elements = c("host", "port", "name", "username", "password"),
  prompt = "Database"
)
}
\description{
Gets credentials securely and saves them in encrypted file.
\strong{File backend for \code{keyring} doesn't work on Windows}.
}
