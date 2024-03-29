% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amstar2_barplot.R
\name{amstar2_barplot}
\alias{amstar2_barplot}
\title{Creates a publication-ready stacked barplot based on the ggplot2 package.}
\usage{
amstar2_barplot(
  data = data,
  font_size = 3.5,
  font_color = "white",
  bar_width = 0.8
)
}
\arguments{
\item{data}{A dataset containing the item ratings.}

\item{font_size}{A number that controls the aesthetic of font size of the percentages in the bars. Default is `font_size = 3.5`.}

\item{font_color}{A color that controls the aesthetic of color of the percentages in the bars. Default is `font_color = "white"`.}

\item{bar_width}{A number that controls the width between the bars. Default is `bar_width = 0.8`.}
}
\value{
A “ggplot2”-based stacked barplot.
}
\description{
It shows the distribution of ratings (“Yes”, “Partial Yes”, “No”, “No MA”) as percentages of SRs for each AMSTAR 2 item.
}
\examples{
library(amstar2Vis)

DATASET <- readxl::read_excel(system.file("extdata", "sample_dat.xlsx", package = "amstar2Vis"))

amstar2_barplot(DATASET)
}
