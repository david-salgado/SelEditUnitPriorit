#' \code{PrioritizeUnits} computes the prioritization of units
#'
#' @param object Object of class \linkS4class{ErrorMoments}.
#'
#' @param Param Object of class \linkS4class{UnitPrioritizationParam}.
#'
#' @return Object of class \linkS4class{UnitPrioritization}.
#'
#'
#' @examples
#' \dontrun{
#' ErrorMoments <- readRDS('../ErrorMoments.rds')
#' UnitPriorParam <- new(Class = 'UnitPrioritizationParam',
#'                       UnitScFunction = 'MinkUnitSc',
#'                       ScFunctionParam =  list(alpha = 1, Weights = 1))
#'
#' PrioritizeUnits(ErrorMoments, UnitPriorParam)
#'
#' }
setGeneric("PrioritizeUnits", function(object, Param) {standardGeneric("PrioritizeUnits")})

#' @rdname PrioritizeUnits
#'
#' @include UnitPrioritizationParam-class.R UnitPrioritization-class.R
#'
#' @import SelEditErrorMoment SelEditFunctions
#'
#' @export
setMethod(f = "PrioritizeUnits",
          signature = c("ErrorMoments", "UnitPrioritizationParam"),
          function(object, Param){

              VarNames <- slot(object, 'VarNames')
              nCol <- length(VarNames)
              UnitScores <- lapply(seq(along = object@Units), function(cellIndex){

                  array <- object@Moments[[cellIndex]]
                  moments <- array$v
                  cellDim <- array$dim[[1]]
                  tempList <- split(moments, rep(1:nCol, each = cellDim))
                  names(tempList) <- VarNames
                  tempDT <- as.data.table(tempList)[
                      , globalSc := do.call(Param@UnitScFunction, c(list(x = as.matrix(.SD)), Param@ScFunctionParam)), .SDcols = VarNames]
                  gbScore <- tempDT[['globalSc']]
                  return(gbScore)
              })

              # UnitScores <- lapply(seq(along = object@Moments), function(indexArray){
              #
              #     nRow <- object@Moments[[indexArray]]$dim[1]
              #     Mat <- matrix(NA_real_,
              #                   nrow = nRow,
              #                   ncol = nCol,
              #                   dimnames = list(NULL, VarNames))
              #     for (i in seq(along = VarNames)){
              #
              #         indexSubset <- ((i-1) * nRow + 1):(i * nRow)
              #         Mat[, i] <- object@Moments[[indexArray]]$v[indexSubset]
              #     }
              #     output <- do.call(Param@UnitScFunction, c(list(x = Mat), Param@ScFunctionParam))
              #     return(output)
              #
              # })

              UnitPriority <- lapply(UnitScores, function(score){

                  dt.aux <- data.table(init.order = seq(along = score), scores = score)

                  if (length(Param@DesignW) == 0){

                      setkeyv(dt.aux, 'scores')
                      dt.aux[, priority := rev(seq(along = score))]
                      setkey(dt.aux, 'init.order')

                      out <- dt.aux[['priority']]
                      return(out)

                  } else {

                      for (indexVar in seq(along = object@Moments)){

                          dt.aux[, paste0('DesignW', VarNames[indexVar]) := -Param@DesignW[[ VarNames[indexVar] ]]]

                      }
                      dt.aux[, scores := -scores]
                      setkeyv(dt.aux, c('scores', paste0('DesignW', VarNames)))
                      dt.aux[, priority := seq(along = score)]
                      setkey(dt.aux, 'init.order')
                      out <- dt.aux[['priority']]
                      return(out)

                  }

              })

              output <- new(Class = 'UnitPrioritization',
                            Domains = object@Domains,
                            Units = object@Units,
                            UnitScores = UnitScores,
                            UnitPriority = UnitPriority,
                            PriorityInfo = StQ::StQ())

              return(output)

          }
)