% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_lm.R
\name{plot_lm}
\alias{plot_lm}
\title{Residual plots}
\usage{
plot_lm(x, plots = c(1L:4L), id.n = 3, labels.id = names(residuals(x)),
  caption = NULL, arrange = T, ...)
}
\arguments{
\item{x}{an lm model object}

\item{plots}{select plots 1-4 (hist,qqnorm,resid fit, std resid fit)}

\item{id.n}{no idea}

\item{labels.id}{plot labels}

\item{caption}{Title}

\item{arrange}{Arrange with gridArrange (T)}

\item{...}{Not implemented}
}
\description{
plot_lm plots graphs useful in diagnosing linear models.
}
\details{
This is (sort of) a copy of the stats plot.lm function,
but with ggplot output. It's a work in progress...
}
