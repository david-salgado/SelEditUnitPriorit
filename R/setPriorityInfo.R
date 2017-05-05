#' @title Set complementary info about the prioritization of units.
#'
#' @description This setter sets the slot PriorityInfo in a \linkS4class{UnitPrioritization} object.
#'
#' @param object Object of class \linkS4class{UnitPrioritization}.
#'
#' @param Param \linkS4class{data.table} with the info on the prioritization of units.
#'
#' @return Object of class \linkS4class{UnitPrioritization}.
#'
#' @rdname setPriorityInfo
#'
#' @examples
#' \dontrun{
#'
#' UnitPriorParam <- new(Class = 'UnitPrioritizationParam',
#'                       UnitScFunction = 'MinkUnitSc',
#'                       ScFunctionParam =  list(alpha = 1, Weights = 1))
#'
#' PrioritizeUnits(ErrorMoment, UnitPriorParam)
#'
#' }
setGeneric("setPriorityInfo<-", function(object, value) {standardGeneric("setPriorityInfo<-")})

#' @rdname setPriorityInfo
#'
#' @include UnitPrioritization-class.R
#'
#' @import data.table
#'
#' @export
setReplaceMethod(
    f = "setPriorityInfo",
    signature = c("UnitPrioritization", "data.table"),
    function(object, value){

        object@PriorityInfo <- value
        return(object)

    }
)