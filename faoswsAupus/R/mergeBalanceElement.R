##' This function appends balance elements to the aupus data
##'
##' The balance elements contains wild card, the function will fill in
##' the country and year specific values, then year wild card then
##' global wild card.
##'
##' @param aupus The aupus data extracted from getAupus function.
##' @param balanceElementData The balance element data extracted from
##' getBalanceElement function.
##' @param balanceElementNum The column name corresponds to the
##' balance element values.
##' @param verbose To print out information
##' 
##' @export
##' 

mergeBalanceElement = function(aupus, balanceElementData,
    balanceElementNum, verbose = FALSE){
    base = merge(aupus, balanceElementData[[1]], all.x = TRUE)
    ## Fill in wild card
    for(i in 2:length(balanceElementData)){
        wildCardFill(base, balanceElementData[[i]], balanceElementNum,
                     verbose)
    }
    base
}
