% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_values_function.R
\name{compare_values}
\alias{compare_values}
\title{Core function that diffs the values of a merged dataset.}
\usage{
compare_values(merged_dataset, name_vector = NA)
}
\arguments{
\item{merged_dataset}{Data.frame of the format (primary_key, value_from_first, value_from_second).}

\item{name_vector}{Vector of length 3 to rename the computed columns.}
}
\description{
This function is the second step in reconciling datasets. It compares the values on a merged dataset.
}
\examples{
compare_values()
}
\keyword{Data}
\keyword{comparison,}
\keyword{diffing,}
\keyword{merge,}
\keyword{reconciliation}

