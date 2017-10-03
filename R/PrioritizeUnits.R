#' \code{PrioritizeUnits} computes the prioritization of units
#'
#' @param object Object of class \linkS4class{SelEditErrorMoment}.
#'
#' @param Param Object of class \linkS4class{UnitPrioritizationParam}.
#'
#' @return Object of class \linkS4class{UnitPrioritization}.
#'
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
              UnitScores <- lapply(seq(along = object@Moments), function(indexArray){

                  nRow <- object@Moments[[indexArray]]$dim[1]
                  Mat <- matrix(NA_real_,
                                nrow = nRow,
                                ncol = nCol,
                                dimnames = list(NULL, VarNames))
                  for (i in seq(along = VarNames)){

                      indexSubset <- ((i-1) * nRow + 1):(i * nRow)
                      Mat[, i] <- object@Moments[[indexArray]]$v[indexSubset]
                  }
                  output <- do.call(Param@UnitScFunction, c(list(x = Mat), Param@ScFunctionParam))
                  return(output)

              })

              UnitPriority <- lapply(UnitScores, function(score){
                  
                  dt.aux <- data.table(init.order = seq(along = score), scores = score)
                  setkeyv(dt.aux, 'scores')
                  dt.aux[, priority := rev(seq(along = score))]
                  setkey(dt.aux, 'init.order')
                  out <- dt.aux[['priority']]
                  return(out)

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