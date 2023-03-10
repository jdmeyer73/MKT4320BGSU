% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stmnl.R
\name{stmnl}
\alias{stmnl}
\title{Easy Standard Mutlinomial Logistic Regression Results}
\usage{
stmnl(model)
}
\arguments{
\item{model}{A \code{multinom} object from the \code{nnet} package}
}
\description{
This takes a multinomial logistic regression model result from
    the \code{multinom} function of the \code{nnet} package and returns
    model fit and odds-ratio estimates
}
\details{
REQUIRED PACKAGES:
\itemize{
  \item broom
}
}
\examples{
stmnl(model)
}
