% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tangram_to_df.R
\name{tangram_to_df}
\alias{tangram_to_df}
\title{tangram_to_df}
\usage{
tangram_to_df(tbl, add_rown = FALSE, html = TRUE, hide_p = FALSE)
}
\arguments{
\item{tbl}{a \code{tangram} object (table)}

\item{add_rown}{logical. Whether to add a row id (\code{id}) to the output dataframe}

\item{html}{logical. Whether to include html code for indentation.}

\item{hide_p}{logical. Whether to hide p-values for variables with "nopv" in the label (first column). This will remove the "nopv" text as well.}
}
\description{
This function converts a \code{tangram} object to dataframe. Will be in html format.
}
\examples{
library(tangram)
tab <- tangram(drug~bili+albumin+stage::Categorical+protime+sex+age+spiders,
              data = pbc,
              style = "lancet",
              id = "mytbl3")
tangram_to_df(tbl = tab, html = F)

## hide_p example
tab2 <- tangram(drug~bili+albumin+stage::Categorical+protime+sex+age+spiders,
              data = pbc \%>\% 
              labelled::set_variable_labels(albumin = "nopv Albumin (gm/dl)"),
              style = "lancet",
              test = T,
              id = "mytbl3")
tangram_to_df(tbl = tab, html = F)
}
