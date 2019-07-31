#' @title Return the complementary info abouth the prioritization of units.
#'
#' @description \code{ComputePriorityInfo} returns an object of class \linkS4class{StQ} with
#' complementary information about the prioritization of units in the optimization approach to
#' selective editing. It basically returns the error estimates, the prediction error standard
#' deviates, the quantile of the error moments per domain, the quantile of sampling weights per
#' domain and the quantile of the unit score per domain.
#'
#' @param object Object of class \linkS4class{UnitPrioritization}.
#'
#' @param ObsPredModelParam Object of class \linkS4class{contObsPredModelParam}.
#'
#' @param Moments Object of class \linkS4class{ErrorMoments}.
#'
#' @param DD Object of class DD.
#'
#' @return Returns an object of class \linkS4class{StQ} with the complementary info.
#'
#' @import data.table contObsPredModelParam SelEditErrorMoment
#'
#' @include UnitPrioritization-class.R
#'
#' @importFrom StQ ExtractNames melt_StQ
#'
#' @export
setGeneric("ComputePriorityInfo", function(object, ObsPredModelParam, Moments, DD){standardGeneric("ComputePriorityInfo")})

#' @rdname ComputePriorityInfo
#'
#' @export
setMethod(
    f = "ComputePriorityInfo",
    signature = c("UnitPrioritization", "contObsPredModelParam", "ErrorMoments", "DD"),
    function(object, ObsPredModelParam, Moments, DD) {

        VarNames <- Moments@VarNames
        DomainNames <- names(Moments@Domains)
        nVar <- length(VarNames)
        IDQual <- names(object@Units[[1]])

        output <- list()
        for (i in seq(along = VarNames)) {

            VarName <- VarNames[i]

            MomentQuants <- lapply(Moments@Moments, function(sArray){

                MomentsVector <- sArray$v
                nUnits <- length(MomentsVector) / nVar

                index <- seq(from = 1 + (i - 1) * nUnits, to = i * nUnits, by = 1)
                LocalMoments <- MomentsVector[index]
                MomentQuant <- ecdf(LocalMoments)(LocalMoments)
                return(MomentQuant)
            })

            ObsPredData.StQ <- ObsPredModelParam@Data#[IDDD == localVarName]

            ObsPredData.dt <- dcast_StQ(ObsPredData.StQ)
            Units.list <- copy(Moments@Units)
            Units <- rbindlist(Units.list)
            DesignW.dt <- ObsPredData.dt[, c(IDQual, DomainNames, paste0('DesignW', VarName)), with = FALSE]
            setkeyv(DesignW.dt, names(Units))
            DesignW.dt <- DesignW.dt[Units]
            ECDF <- function(x){ecdf(x)(x)}
            DesignWQuants <- DesignW.dt[, DesignWQuantile := ECDF(get(paste0('DesignW', VarName))), by = DomainNames]

            localOutput <- lapply(seq(along = Units.list), function(index){

                localDT <- Units.list[[index]]
                setkeyv(localDT, names(localDT))
                localDT$MomentQuantile <- MomentQuants[[index]]
                UnitScore <- object@UnitScores[[index]]
                localDT[, UnitScoreQuantile := ecdf(UnitScore)(UnitScore)]
                return(localDT)
            })
            localOutput <- rbindlist(localOutput)
            localOutput <- merge(localOutput, DesignWQuants, by = IDQual, all.x = TRUE)
            localOutput[, (DomainNames) := NULL]
            setnames(localOutput, 'MomentQuantile', paste0('MomentQuant', VarName))

            ObsPredVarNames <- paste0(c('Pred', 'ObsErrorSTD', 'PredErrorSTD', 'DesignW'), VarName)
            localVarNames <- c(VarName, ObsPredVarNames)
            ObsPredData.StQ <- ObsPredModelParam@Data#[IDDD == localVarName]
            ObsPredData.dt <- ObsPredData.dt[, c(IDQual, localVarNames), with = F]
            localOutput[, (paste0('DesignW', VarName)) := NULL]
            localOutput <- merge(localOutput, ObsPredData.dt, all.x = TRUE, by = IDQual)
            localOutput[, (VarName) := NULL]
            localOutput[, (paste0('DesignW', VarName)) := NULL]
            setnames(localOutput, 'DesignWQuantile', UnitToIDDDNames('CuantPeso', DD))
            setnames(localOutput, 'UnitScoreQuantile', UnitToIDDDNames('CuantGlob', DD))
            setnames(localOutput, paste0('ObsErrorSTD', VarName), paste(UnitToIDDDNames('STDErrorObs', DD), names(VarName), sep = '_'))
            setnames(localOutput, paste0('PredErrorSTD', VarName), paste(UnitToIDDDNames('STDErrorPred', DD), names(VarName), sep = '_'))
            setnames(localOutput, paste0('MomentQuant', VarName), paste(UnitToIDDDNames('CuantMom', DD), names(VarName), sep = '_'))
            setnames(localOutput, paste0('Pred', VarName), paste(UnitToIDDDNames('PredValue', DD), names(VarName), sep = '_'))

            output[[VarName]] <- localOutput

        }

        output <- Reduce(function(x, y){merge(x, y, all = TRUE, by = intersect(names(x), names(y)))},
                         output)
        output <- melt_StQ(output, DD)
        return(output)


    }
)