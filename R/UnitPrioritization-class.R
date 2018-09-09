#' @title S4 class for the prioritization of units
#'
#' @description Definition of the S4 class named \code{UnitPrioritization} for the unit
#' prioritization in the optimization approach to selective editing.
#'
#' @slot Domains \linkS4class{data.table} with the values of the variables indicating the domains.
#'
#' @slot Units list with the units of each domain.
#'
#' @slot UnitScores list with the scores of each unit.
#'
#' @slot UnitPriority list with the priority.
#'
#' @slot PriorityInfo list with complementary information about the prioritization of units.
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
