% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mainfun.plot.R
\name{ggmf}
\alias{ggmf}
\title{ggplot2 extension for a mf object}
\usage{
ggmf(output, ecosystem, scale = "fixed", fit.lm = TRUE)
}
\arguments{
\item{output}{the output from \code{mf}}

\item{ecosystem}{\code{'single'} or \code{'multiple'}. MF measures in a single ecosystem or multiple ecosystems.}

\item{scale}{Are scales shared across all facets (the default, \code{"fixed"}), or do they vary across rows (\code{"free_x"}), columns (\code{"free_y"}), or both rows and columns (\code{"free"})?}

\item{fit.lm}{If \code{TRUE}, the default, fitted line of linear model and estimated of slope are displayed.}
}
\value{
For \code{ecosystem = 'single'}, return a figure for multi-functionality with uncorrelated/correlated functions. For \code{ecosystem = 'multiple'}, return a figure when input contains only uncorrelated or correlated MF decomposition or
 a list of two figures when input contains both uncorrelated and correlated MF decomposition.
}
\description{
\code{ggmf}: the \code{\link[ggplot2]{ggplot}} extension for \code{mf} object to plot the correlation between species richness and multi-functionality
}
\examples{

## single ecosystem
data("Europe_Forest")
output1 <- MF_measures(data = Europe_Forest, fun_cols = 4:29, ecosystem = 'single')

## Display fitted line of linear model
ggmf(output1, ecosystem = 'single', scale = 'fixed', fit.lm = TRUE)

## Not display fitted line of linear model
ggmf(output1, ecosystem = 'single', scale = 'fixed', fit.lm = FALSE)


## multiple ecosystems
output2 <- MF_measures(data = Europe_Forest, fun_cols = 4:29, ecosystem = 'multiple', species_col = "composition")

## Display fitted line of linear model
ggmf(output2, ecosystem = 'multiple', scale = 'fixed', fit.lm = TRUE)

## Not display fitted line of linear model
ggmf(output2, ecosystem = 'multiple', scale = 'fixed', fit.lm = FALSE)

}
