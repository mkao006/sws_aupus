##' The function takes wild card data to fill in original data for a
##' particular variable.
##'
##' Shares, ratio and balance element have area and year specific
##' rates, but at the same time they have wild card values which are
##' to be applied when the specific rates are not available.
##'
##' This function fills in the gap with wild card values when year
##' specific values are not available.
##'
##' @param originalData The data to be filled in
##' @param wildCardData The wild card data, see ratio or shares data
##' extracted from the getShare and getRatio function.
##' @param variable The name of the variable to be filled.
##' @export

wildCardFill = function(originalData, wildCardData, variable,
    verbose = FALSE, ...){
    if(verbose)
        cat("Number of Miss for vairable", variable, ":",
            sum(is.na(originalData[, variable, with = FALSE])),
            "\n")
    evalText = paste0(variable, " := i.", variable)
    index = unique(wildCardData[originalData[is.na(get(variable)),
        key(wildCardData), with = FALSE], ][!is.na(get(variable)), ])
    setkeyv(index, key(wildCardData))
    okey = key(originalData)
    setkeyv(originalData, key(index))
    originalData[index[!is.na(get(variable)), 
                       c(key(index), variable),
                       with = FALSE],
                 eval(parse(text = evalText))]
    setkeyv(originalData, okey)
    if(verbose)
        cat("Number of Miss for vairable", variable, ":",    
            sum(is.na(originalData[, variable, with = FALSE])),
            "\n")
}
