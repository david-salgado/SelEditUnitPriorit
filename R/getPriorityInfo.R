#' @title Return the complementary info abouth the prioritization of units.
#'
#' @description \code{getPriorityInfo} returns a \linkS4class{data.table} with complementary
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
#' @return Returns a \linkS4class{data.table} with the complementary info.
#'
#' @examples
#'
#' @import data.table contObsPredModelParam SelEditErrorMoment
#'
#' @importFrom StQ ExtractNames
#'
#' @export
setGeneric("getPriorityInfo", function(object, ObsPredModelParam, Moments){standardGeneric("getPriorityInfo")})

#' @rdname getPriorityInfo
#'
#' @export
setMethod(
    f = "getPriorityInfo",
    signature = c("UnitPrioritization", "contObsPredModelParam", "ErrorMoments"),
    function(object, ObsPredModelParam, Moments) {

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
            localOutput[, DesignWQuantile := DesignWQuants[['DesignWQuantile']]]
            setnames(localOutput, 'MomentQuantile', paste0('MomentQuant', VarName))

            localVarName <- ExtractNames(VarName)
            ObsPredVarNames <- paste0(c('Pred', 'PredErrorSTD', 'DesignW'), VarName)
            localVarNames <- c(VarName, ObsPredVarNames)
            ObsPredData.StQ <- ObsPredModelParam@Data#[IDDD == localVarName]
            ObsPredData.dt <- ObsPredData.dt[, c(IDQual, localVarNames), with = F]
            localOutput <- merge(localOutput, ObsPredData.dt, all.x = TRUE, by = IDQual)
            localOutput[[paste0('PredError', VarName)]] <- localOutput[[VarName]] - localOutput[[paste0('Pred', VarName)]]
            localOutput[, (paste0('Pred', VarName)) := NULL]
            localOutput[, (VarName) := NULL]
            localOutput[, (paste0('DesignW', VarName)) := NULL]
            localOutput$IDEdit <- localVarName
            setcolorder(localOutput, c(IDQual, 'DesignWQuantile', 'UnitScoreQuantile', 'IDEdit', paste0('PredError', VarName), paste0('PredErrorSTD', VarName), paste0('MomentQuant', VarName)))
            setnames(localOutput, 'UnitScoreQuantile', 'Parametro_07._5.1.1.6.')
            setnames(localOutput, paste0('PredError', VarName), 'Parametro_07._5.1.1.7.')
            setnames(localOutput, paste0('PredErrorSTD', VarName), 'Parametro_07._5.1.1.8.')
            setnames(localOutput, paste0('MomentQuant', VarName), 'Parametro_07._5.1.1.9.')
            setnames(localOutput, 'DesignWQuantile', 'Parametro_07._5.1.1.5')
            output[[VarName]] <- localOutput

        }

        output <- rbindlist(output)
        return(output)


    }
)

