#' @title Return the complementary info abouth the prioritization of units.
#'
#' @description \code{ComputePriorityInfo} returns a \linkS4class{data.table} with complementary
#' information about the prioritization of units in the optimization approach to selective editing.
#' It basically returns the error estimates, the prediction error standard deviates, the quantile of
#' the error moments per domain, the quantile of sampling weights per domain and the quantile of the
#' unit score per quantile.
#'
#' @param object Object of class \linkS4class{UnitPrioritization}.
#'
#' @param ObsPredModelParam Object of class \linkS4class{contObsPredModelParam}.
#'
#' @param Moments Object of class \linkS4class{ErrorMoments}.
#'
#' @param DD Object of class DD.
#'
#' @return Returns a \linkS4class{data.table} with the complementary info.
#'
#' @examples
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
            setnames(localOutput, 'MomentQuantile', paste0('MomentQuant', VarName))
            #localOutput[, (paste0('DesignW', VarName)) := NULL]

            localVarName <- ExtractNames(VarName)
            #ObsPredVarNames <- paste0(c('Pred', 'PredErrorSTD', 'DesignW'), VarName)
            ObsPredVarNames <- paste0(c('Pred', 'ObsErrorSTD', 'PredErrorSTD', 'DesignW'), VarName)
            localVarNames <- c(VarName, ObsPredVarNames)
            ObsPredData.StQ <- ObsPredModelParam@Data#[IDDD == localVarName]
            ObsPredData.dt <- ObsPredData.dt[, c(IDQual, localVarNames), with = F]
            localOutput[, (paste0('DesignW', VarName)) := NULL]
            localOutput <- merge(localOutput, ObsPredData.dt, all.x = TRUE, by = IDQual)
            #localOutput[, (paste0('PredError', VarName)) := get(VarName) - get(paste0('Pred', VarName))]
            #localOutput[, (paste0('Pred', VarName)) := NULL]
            #localOutput[, (VarName) := NULL]
            localOutput[, (paste0('DesignW', VarName)) := NULL]
            #setcolorder(localOutput, c(IDQual, 'DesignWQuantile', 'UnitScoreQuantile', VarName, paste0('Pred', VarName), paste0('PredError', VarName), paste0('PredErrorSTD', VarName), paste0('MomentQuant', VarName)))
            setnames(localOutput, 'DesignWQuantile', 'Parametro_07._5.1.1.5.')
            setnames(localOutput, 'UnitScoreQuantile', 'Parametro_07._5.1.1.6.')
            #setnames(localOutput, paste0('PredError', VarName), paste0('Parametro_07._5.1.1.7._', VarName))
            setnames(localOutput, paste0('ObsErrorSTD', VarName), paste0('Parametro_07._5.1.1.8._', VarName))
            #setnames(localOutput, paste0('PredErrorSTD', VarName), paste0('Parametro_07._5.1.1.8._', VarName))
            setnames(localOutput, paste0('PredErrorSTD', VarName), paste0('Parametro_07._5.1.1.7._', VarName))
            setnames(localOutput, paste0('MomentQuant', VarName), paste0('Parametro_07._5.1.1.9._', VarName))
            setnames(localOutput, paste0('Pred', VarName), paste0('Parametro_07._5.1.1.10._', VarName))
            output[[VarName]] <- localOutput

        }

        output <- Reduce(function(x, y){merge(x, y, all = TRUE, by = intersect(names(x), names(y)))},
                         output)
        output <- melt_StQ(output, DD)
        return(output)


    }
)

