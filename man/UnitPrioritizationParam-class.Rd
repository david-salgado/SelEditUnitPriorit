% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UnitPrioritizationParam-class.R
\docType{class}
\name{UnitPrioritizationParam-class}
\alias{UnitPrioritizationParam-class}
\title{S4 class for the parameters of the prioritization of units}
\description{
Definition of the S4 class named \code{UnitPrioritizationParam} for the parameters
of unit prioritization in the optimization approach to selective editing.
}
\section{Slots}{

\describe{
\item{\code{UnitScFunction}}{Character vector of length 1 with the name of the unit (global) score
function.}

\item{\code{ScFunctionParam}}{Named list with the parameters for the unit score function.}

\item{\code{DesignW}}{Named list with the design weights of each variable with item score.}
}}

\examples{
# An empty UnitPrioritizationParam object:
new(Class = 'UnitPrioritizationParam')

\dontrun{
UnitPriorParam <- new(Class = 'UnitPrioritizationParam',
                      UnitScFunction = 'SelEditFunctions::MinkUnitSc',
                      ScFunctionParam =  list(alpha = 1, Weights = 1))

}

}
