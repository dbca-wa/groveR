% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{extent_change_plot}
\alias{extent_change_plot}
\title{A function to generate a stacked bar chart for mangrove change summaries.}
\usage{
extent_change_plot(icsv, areaname, cap)
}
\arguments{
\item{icsv}{Character representation of the name of the extent change
csv file including file path.}

\item{areaname}{Character string of desired monitoring area name for inclusion
to output png name.}

\item{cap}{Character string of caption (not title) to supply to the plot. If
none required use empty quotes i.e. "".}
}
\value{
A PNG format graphic will be written to the `extent_change\`
    directory.
}
\description{
\code{change_extent_plot} creates stacked bar chart of vegetation change summaries.
}
\details{
Takes a file path to an extent change csv file as created by
    \code{link{extent_change}} located in `extent_change\` and produce
    a stacked bar chart representing the same.
}
\examples{
\dontrun{
extent_change_plot(icsv = "extent_change/lgcsmp_2014-2023_extent_change.csv",
    areaname = "lgcsmp", cap = "RSSA")
}

}
\author{
Bart Huntley, \email{bart.huntley@dbca.wa.gov.au}
}
