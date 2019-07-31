#' @title Return the units with their editing priority
#'
#' @description \code{getPriority} extracts the slots \code{UnitPriority} and \code{Units} to
#' return a \linkS4class{data.table} with each unit and its editing priority.
#'
#' @param object Object of class \linkS4class{UnitPrioritization}.
#'
#' @return Numeric vector with the values of priority.
#'
#' @examples
#' \dontrun{
#' source('R:/USIE/Proyecto_DepSel_VarQual/datos/PreparacionStQsAnalisis.R', encoding = 'UTF-8')
#' fitPar <- new(Class = 'fitParam', edData = ff_2011_model.StQ, rawData = fd_2011_model.StQ,
#'               selection = FALSE,  formula = 'Ocupacion_35.__11.1.3._ ~ ActivEcono_35.__2.1.1._',
#'               selParam = list())
#' getEdData(fitPar)
#'}
#'
#'
#' @include UnitPrioritization-class.R
#'
#' @export
setGeneric("getPriority", function(object){standardGeneric("getPriority")})

#' @rdname getPriority
#'
#' @include UnitPrioritization-class.R
#'
#' @export
setMethod(
    f = "getPriority",
    signature = c("UnitPrioritization"),
    function(object){

        output <- rbindlist(object@Units)
        priority <- Reduce(c, object@UnitPriority)
        output[, Priority := priority]
        return(output[])
    }
)
