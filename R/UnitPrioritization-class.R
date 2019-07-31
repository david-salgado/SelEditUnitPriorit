#' @title S4 class for the prioritization of units
#'
#' @description Definition of the S4 class named \code{UnitPrioritization} for the unit
#' prioritization in the optimization approach to selective editing.
#'
#' @slot Domains \linkS4class{data.table} with a row per domain with the values of the variables
#' determining the domains of the data set.
#' 
#' @slot Units List of data.tables with as many components as domains (in the same order of the rows
#' of slot \code{Domains}) with every unit per domain.
#'
#' @slot UnitScores List with as many components as domains (in the same order of the rows
#' of slot \code{Domains}) with the scores of each unit (in the same order of the corresponding
#' data.table in slot \code{Units}).
#'
#' @slot UnitPriority List with as many components as domains (in the same order of the rows
#' of slot \code{Domains}) with the priority of each unit (in the same order of the corresponding
#' data.table in slot \code{Units}).
#'
#' @slot PriorityInfo Object of class \linkS4class{StQ} with complementary information about the
#' prioritization of units.
#'
#' @examples
#' # An empty UnitPrioritization object:
#' new(Class = 'UnitPrioritization')
#'
#' \dontrun{
#' UnitPriorParam <- new(Class = 'UnitPrioritization')
#'
#' }
#'
#' @export
setClass(Class = "UnitPrioritization",
         slots = c(Domains = 'data.table',
                   Units = 'list',
                   UnitScores = 'list',
                   UnitPriority = 'list',
                   PriorityInfo = 'StQ'),
         prototype = list(Domains = data.table(),
                          Units = list(),
                          UnitScores = list(),
                          UnitPriority = list(),
                          PriorityInfo = StQ::StQ()),
         validity = function(object){



             return(TRUE)
         }
)
