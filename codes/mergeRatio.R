##' This function appends ratios to the aupus data
##'
##' The ratio data contains wild card, the function will fill in
##' the country and year specific values, then year wild card then
##' global wild card.
##'
##' @param aupus The aupus data extracted from getAupus function.
##' @param ratioData The ratio data from the function getRatio.
##' @param verbose To print out information
##' 
##' @export
##' 

mergeRatio = function(aupus, ratioData, verbose = FALSE){
    base = merge(aupus, ratioData[[1]], all.x = TRUE)
    ## Fill in wild card
    for(i in 2:length(ratioData)){
        lapply(grep("RATIO", colnames(ratioData[[i]]), value = TRUE),
               FUN = function(x){
                   wildCardFill(base, ratioData[[i]], x,
                                verbose = verbose)
               }
               )
    }    
    base
}
