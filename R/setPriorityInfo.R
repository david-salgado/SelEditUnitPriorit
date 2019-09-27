#' @title Set complementary info about the prioritization of units.
#'
#' @description This setter sets the slot PriorityInfo in a \linkS4class{UnitPrioritization} object.
#'
#' @param object Object of class \linkS4class{UnitPrioritization}.
#'
#' @param value Object of class \linkS4class{StQ} with the info on the prioritization of units.
#'
#' @return Object of class \linkS4class{UnitPrioritization}.
#'
#' @rdname setPriorityInfo
#'
setGeneric("setPriorityInfo<-", function(object, value) {standardGeneric("setPriorityInfo<-")})

#' @rdname setPriorityInfo
#'
#' @include UnitPrioritization-class.R
#'
#' @import data.table StQ
#'
#' @export
setReplaceMethod(
    f = "setPriorityInfo",
    signature = c("UnitPrioritization", "StQ"),
    function(object, value){

        object@PriorityInfo <- value
        return(object)

    }
)