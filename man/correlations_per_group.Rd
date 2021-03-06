% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation_per_group.R
\name{correlations_per_group}
\alias{correlations_per_group}
\title{Calculate correlation for selected variables in groups}
\usage{
correlations_per_group(.data, ..., group_var = NULL)
}
\arguments{
\item{.data}{Data frame containing the data.}

\item{...}{Variable names (quoted or unquoted) to calculate the correlation for.}

\item{group_var}{Variable used to divide data into categories. Default value is \code{NULL}.}
}
\value{
Data frame containing correlations summaries.
}
\description{
Calculate correlation for selected variables in groups
}
\examples{
df_corrs <- correlations_per_group(mtcars, wt, qsec, group_var = cyl)
}
