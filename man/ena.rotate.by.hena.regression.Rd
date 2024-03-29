% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ena.rotate.by.regression.R
\name{ena.rotate.by.hena.regression}
\alias{ena.rotate.by.hena.regression}
\title{ENA Rotate by regression}
\usage{
ena.rotate.by.hena.regression(enaset, params)
}
\arguments{
\item{enaset}{An \code{\link{ENAset}}}

\item{params}{list of parameters, may include:
x_var: Regression formula for x direction, such as "lm(formula=V ~ Condition + GameHalf + Condition : GameHalf)".
y_var: Regression formula for y direction (optional).
points: A unit by connection weight matrix for rotation. If not provided, points in enaset are used.
fullNames: If true, all independent variable names are included in the x and y names.
   Otherwise, only first variable name is used.}
}
\value{
\code{\link{ENARotationSet}}
}
\description{
This function allows user to provide a regression formula for rotation on x and optionally on y.
   If regression formula for y is not provide, svd is applied to the residual data deflated by x to get y coordinates.
}
