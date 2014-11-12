##' This function returns fills the balance
##'
##' The balance calculated from the calculate balance function is
##' filled in either the balance element or in discrepancy element
##' 181.
##'
##' @param calculatedBalance The column corresponding to the balance
##' calculated by the function calculateBalance.
##' @param balanceNum The value column corresponding to the balancing
##' element.
##' @param balanceSymb The symb column corresponding to the balancing
##' element.
##' @param element181Num The column corresponding to value of element
##' 181.
##' @param element181Symb The column corresponding to symbol of element
##' 181.
##' @param data The data
##' @export
##' 


fillBalance = function(calculatedBalance, balanceNum, balanceSymb,
    element181Num, element181Symb, data){
    ## Replace if the balancing element is repaceable and calculated
    ## balancing value is greater than zero
    replaceIndex = which(replaceable(data[, balanceSymb, with = FALSE]) &
                         data[, calculatedBalance, with = FALSE] > 0)

    originalValue = unlist(data[, balanceNum, with = FALSE])
    originalValue[replaceIndex] =
        unlist(data[replaceIndex, calculatedBalance, with = FALSE])
    originalSymb = unlist(data[, balanceSymb, with = FALSE]) 
    originalSymb[replaceIndex] = "B"

    discrepancyValue = unlist(data[, element181Num, with = FALSE])
    discrepancyValue[replaceIndex] = 0
    discrepancySymb = unlist(data[, element181Symb, with = FALSE])
    discrepancySymb[replaceIndex] = "B"

    ## If neither is satisfied, then set the balancing element to
    ## 0 and the calculated balancing value to statistical
    ## discrepancy.
    discrepancyValue[!replaceIndex] =
        unlist(data[!replaceIndex, calculatedBalance, with = FALSE])
    discrepancySymb[!replaceIndex] = "B"
    replaced = rep(FALSE, NROW(data))
    replaced[replaceIndex] = TRUE
    list(originalValue, originalSymb, discrepancyValue,
         discrepancySymb, replaced)
}
