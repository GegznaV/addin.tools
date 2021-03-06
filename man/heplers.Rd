% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_blank_line_needed_below.R
\name{is_blank_line_needed_below}
\alias{is_blank_line_needed_below}
\title{' Check if a blank line is needed below.}
\usage{
is_blank_line_needed_below(
  where = c("last row", "first row"),
  context = rs_get_context()
)
}
\arguments{
\item{where}{(string)
If \code{"first"} - checks for a blank line below the first selected row.
If \code{"last"} - checks for a blank line below the last selected row.}

\item{context}{(class \code{document_context}) \cr
Object with context of active RStudio document.}
}
\description{
Checks if a blank line should be added below either all selected rows or
the first selected row.
}
