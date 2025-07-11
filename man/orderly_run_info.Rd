% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recipe_run.R
\name{orderly_run_info}
\alias{orderly_run_info}
\title{Information on current orderly run}
\usage{
orderly_run_info(path = NULL)
}
\arguments{
\item{path}{Path to the report currently being run.  This should
be left as \code{NULL} when running a report, and the path to
the report being run should be used when using
\code{\link[=orderly_test_start]{orderly_test_start()}}}
}
\value{
A list of metadata about the current report
}
\description{
This function allows inspection of some of orderly's metadata
during an orderly run.  The format returned is internal to orderly
and subject to change.  It is designed to be used within report
code.  To use in conjunction with
\code{\link[=orderly_test_start]{orderly_test_start()}}, you must pass in the path to the
report in question.
}
\section{Warning}{


It is important that this data is treated as \emph{readonly}!
}

\examples{
path <- orderly1::orderly_example("demo")

# This example uses orderly_run_info within its script, saving the
# output to "output.rds"
readLines(file.path(path, "src", "use_dependency", "script.R"))

# Run the dependency:
id <- orderly1::orderly_run("other", list(nmin = 0), root = path)
orderly1::orderly_commit(id, root = path)

# Then the report
id <- orderly1::orderly_run("use_dependency", root = path)

# This is the contents:
readRDS(file.path(path, "draft", "use_dependency", id, "info.rds"))
}
