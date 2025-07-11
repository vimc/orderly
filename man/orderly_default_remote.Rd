% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remote.R
\name{orderly_default_remote_set}
\alias{orderly_default_remote_set}
\alias{orderly_default_remote_get}
\title{Set default remote location}
\usage{
orderly_default_remote_set(value, root = NULL, locate = TRUE)

orderly_default_remote_get(root = NULL, locate = TRUE)
}
\arguments{
\item{value}{A string describing a remote, a remote object, or
\code{NULL} to clear}

\item{root}{The path to an orderly root directory, or \code{NULL}
(the default) to search for one from the current working
directory if \code{locate} is \code{TRUE}.}

\item{locate}{Logical, indicating if the configuration should be
searched for.  If \code{TRUE} and \code{config} is not given,
then orderly looks in the working directory and up through its
parents until it finds an \code{orderly_config.yml} file.}
}
\value{
The default remote (for
\code{orderly_default_remote_get}). The function
\code{orderly_default_remote_set} is called for its side effects
only.
}
\description{
Set and get default remote locations.  Default locations are
specific to an orderly repository (based on the path of the
repository) so there is no interaction between different orderly
projects.
}
\examples{
# Same setup as in orderly_remote_path, with a remote orderly:
path_remote <- orderly1::orderly_example("demo")
id <- orderly1::orderly_run("other", list(nmin = 0),
                           root = path_remote, echo = FALSE)
orderly1::orderly_commit(id, root = path_remote)
id <- orderly1::orderly_run("use_dependency",
                           root = path_remote, echo = FALSE)
orderly1::orderly_commit(id, root = path_remote)

# And a local orderly
path_local <- orderly1::orderly_example("demo")

# We'll create an object to interact with this remote using
# orderly_remote_path.
remote <- orderly1::orderly_remote_path(path_remote)

# There is no remote set by default:
try(orderly1::orderly_default_remote_get(root = path_local))

# We can set one:
orderly1::orderly_default_remote_set(remote, root = path_local)

# and now we can retrieve it:
orderly1::orderly_default_remote_get(root = path_local)

# Note that this has not affected the other orderly:
try(orderly1::orderly_default_remote_get(root = path_remote))
}
