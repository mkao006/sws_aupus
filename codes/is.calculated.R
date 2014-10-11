##' Function to determine whether a value is calculated based on symbols
##'
##' @param symb
##' @param calculatedSymb
##' @export
##' 

is.calculated = function(symb, calculatedSymb = "C"){
    symb %in% calculatedSymb
}
