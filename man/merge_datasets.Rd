% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_datasets_function.R
\name{merge_datasets}
\alias{merge_datasets}
\title{Core function that combines datasets for comparison.}
\usage{
merge_datasets(first_dataset, second_dataset, name_vector = NA)
}
\arguments{
\item{first_dataset}{Data.frame of the format (primary_key, value).}

\item{second_dataset}{Data.frame of the format (primary_key, value).}

\item{name_vector}{Vector of length 2 to rename the computed columns.}
}
\description{
This function is the first step in reconciling datasets. It mashes together the datasets that need to be reconciled.
}
\examples{
merge_datasets()
}
\keyword{Data}
\keyword{comparison,}
\keyword{diffing,}
\keyword{merge,}
\keyword{reconciliation}

