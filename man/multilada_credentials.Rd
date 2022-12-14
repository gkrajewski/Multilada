% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/credentials.R
\name{multilada_credentials}
\alias{multilada_credentials}
\title{Get or set credentials securely}
\usage{
multilada_credentials(
  key = NULL,
  elements = c("host", "port", "name", "username", "password"),
  prompt = "Database"
)
}
\arguments{
\item{key}{A \code{character}, the identifier of all \emph{elements} that go together,
e.g., it could be "pn-recruitment" for all credentials needed to access the
PolkaNorski recruitment database.

If \code{key = NULL} (the default), the function only asks for the \emph{secret} for
each \emph{element} (in a secure way, e.g., masking the responses as they are
typed), without using the system credential store (and package \code{keyring}).

If \code{key} is provided and there is a \emph{secret} stored for a given \emph{element}
and a given \emph{key} in the system credential store, the \emph{secret} is
retrieved. Otherwise, the function asks for the \emph{secret} for that \emph{element}
(in a secure way, e.g., masking the responses as they are typed) and saves
it in the system credential store.

In the \code{keyring} package \code{key} is called a \emph{service}, in macOS Keychain it
is a \emph{name} etc.}

\item{elements}{A character vector of names of all credentials needed.
Defaults to \code{c("host", "port", "name", "username", "password")}, which
covers all credentials required to access a database (but we might decide
to un-\emph{secret}, e.g., the port number to PN Recruitment database and
hard-code it in our scripts).}

\item{prompt}{A \code{character}, the prefix to a prompt displayed, when the
function needs to ask for a \emph{secret} for a given \emph{element} (either because
\code{key = NULL}, or because there is no \emph{secret} for a given \emph{element} and a
given \emph{key}).

Defaults to "Database" which, when asking for, e.g., the "host", would
display "Database host: " as the promopt.}
}
\value{
A named list of \emph{secrets} for all \emph{elements} with \emph{elements}
used as names.

\strong{Secrets returned as unmasked \code{characters} (i.e., plain text).}
}
\description{
\code{multilada_credentials()} gets a list of secret credentials (\emph{elements}). It
can ask for them in a secure way and (optionally) save them to a system-based
credential store (e.g., \emph{Keychain} on macOS), or retrieve them from the
system-based credential store if they already are stored there.
}
\details{
We use package \code{keyring} to save and securely retrieve secret credentials.
\code{keyring} is supposed to be system-independent and should work on macOS,
Windows and Linux but I have tested it only with macOS's \emph{Keychain}.

Each \emph{secret} in the system-based credential store is identified by a
\emph{service} name and may optionally have a \emph{username} attached (because the
most typical use case is to store a \emph{secret password} for a \emph{username} in a
given \emph{service}). We use the same \emph{service} name (e.g., "pn-recruitment") for
all credentials to bundle them together and we use \emph{usernames} to distinguish
them from each other (e.g., "host", "port").

You can access and check the stored credentials manually. On macOS you will
use the Keychain Access app. It shows \emph{service} as \emph{name}, \emph{username} as
\emph{account}, and all our entries are classified as \emph{application passwords}
(column \emph{Kind}). I have no idea how it is implemented in other systems
though.
}
\examples{
# To ask for all credentials needed to access the PolkaNorski Recruitment database:
pn_rec_credentials <- multilada_credentials(prompt = "PN Recruitment database")
# ---
# To use the system credential store:
\dontrun{
pn_rec_credentials <- multilada_credentials("pn-recruitment", prompt = "PN Recruitment database")
}
# If a given element (e.g., "host") is not yet stored in the system credential store,
# the function asks for it and saves it to the store, so that next time it can be retrieved
# without asking again.
# ---
# To access a single credentials element, e.g., the port number labeled as "port":
pn_rec_credentials$port

}
