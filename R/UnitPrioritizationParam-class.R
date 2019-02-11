#' @title S4 class for the parameters of the prioritization of units
#'
#' @description Definition of the S4 class named \code{UnitPrioritizationParam} for the parameters
#' of unit prioritization in the optimization approach to selective editing.
#'
#' @slot UnitScFunction Character vector of length 1 with the name of the unit (global) score
#' function.
#'
#' @slot ScFunctionParam Named list with the parameters for the unit score function.
#'
#' @slot DesignW Named list with the design weights of each variable with item score.
#'
#' @examples
#' # An empty UnitPrioritizationParam object:
#' new(Class = 'UnitPrioritizationParam')
#'
#' \dontrun{
#' UnitPriorParam <- new(Class = 'UnitPrioritizationParam',
#'                       UnitScFunction = 'SelEditFunctions::MinkUnitSc',
#'                       ScFunctionParam =  list(alpha = 1, Weights = 1))
#'
#' }
#'
#' @export
setClass(Class = "UnitPrioritizationParam",
         slots = c(UnitScFunction = 'character',
                   ScFunctionParam = 'list',
                   DesignW = 'list'),
         prototype = list(UnitScFunction = 'SelEditFunctions::MinkUnitSc',
                          ScFunctionParam = list(alpha = 1, Weights = 1),
                          DesignW = list()),
         validity = function(object){



             return(TRUE)
         }
)
