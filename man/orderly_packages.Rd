% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{orderly_packages}
\alias{orderly_packages}
\title{Return details of packages required by all src reports in this orderly repo}
\usage{
orderly_packages(root = NULL, locate = TRUE)
}
\arguments{
\item{root}{The path to an orderly root directory, or \code{NULL}
(the default) to search for one from the current working
directory if \code{locate} is \code{TRUE}.}

\item{locate}{Logical, indicating if the configuration should be
searched for.  If \code{TRUE} and \code{config} is not given,
then orderly looks in the working directory and up through its
parents until it finds an \code{orderly_config.yml} file.}
}
\value{
List of packages required by all src reports
}
\description{
Return details of packages required by all src reports in this orderly repo
}
\examples{
path <- orderly1::orderly_example("minimal")
orderly1::orderly_packages(root = path)
}
